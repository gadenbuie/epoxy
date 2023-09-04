#' Epoxy Inline Transformer
#'
#' @description This epoxy transformer is heavily inspired by the inline
#' formatters in the [cli package](https://cli.r-lib.org). The syntax is quite
#' similar, but \pkg{epoxy}'s syntax is slightly different to accommodate
#' reporting use cases.
#'
#' To transform a template expression inline, you include a keyword, prefixed
#' with a dot (`.`) that is used to format the value of the template expression
#' in place.
#'
#' ```{r}
#' epoxy("It cost {.dollar 123456}.", .transformer = "inline")
#' ```
#'
#' The formatters, e.g. `.dollar` in the example above, can be customized using
#' the arguments of `epoxy_transform_inline()`. Pass a customized
#' [scales::label_dollar()] to `.dollar` to achieve a different transformation.
#'
#' ```{r}
#' dollars_nzd <- scales::label_dollar(suffix = " NZD")
#'
#' epoxy(
#'   "It cost {.dollar 123456}.",
#'   .transformer = epoxy_transform_inline(.dollar = dollars_nzd)
#' )
#' ```
#'
#' Note that, unlike
#' [inline markup with cli](https://cli.r-lib.org/reference/inline-markup.html),
#' the text within the template expression, other than the keyword, is treated
#' as an R expression.
#'
#' ```{r}
#' money <- 123456
#' epoxy("It cost {.dollar money}.", .transformer = "inline")
#' ```
#'
#' You can also nest inline markup expressions.
#'
#' ```{r}
#' money <- c(123.456, 234.567)
#' epoxy("It will cost either {.or {.dollar money}}.", .transformer = "inline")
#' ```
#'
#' Finally, you can provide your own functions that are applied to the evaluated
#' expression. In this example, I add a `.runif` inline formatter that generates
#' `n` random numbers (taken from the template expression) and sorts them.
#'
#' ```{r}
#' set.seed(4242)
#'
#' epoxy(
#'   "Here are three random percentages: {.and {.pct {.runif 3}}}.",
#'   .transformer = epoxy_transform_inline(
#'     .runif = function(n) sort(runif(n))
#'   )
#' )
#' ```
#'
#' @example man/examples/epoxy_transform_inline.R
#'
#' @param ... Additional named inline transformers as functions taking at least
#'   one argument. The evaluated expression from the template expression is
#'   passed as the first argument to the function. The argument names must
#'   include the leading `.` to match the syntax used inline.
#' @param transformer The transformer to apply to the replacement string. This
#'   argument is used for chaining the transformer functions. By providing a
#'   function to this argument you can apply an additional transformation after
#'   the current transformation. In nearly all cases, you can let
#'   `epoxy_transform()` handle this for you. The chain ends when
#'   [glue::identity_transformer()] is used as the `transformer`.
#' @eval roxy_inline_params()
#'
#' @inherit epoxy_transform return
#' @seealso [epoxy_transform()], [epoxy_transform_set()]
#' @family epoxy's glue transformers
#' @export
epoxy_transform_inline <- function(
	...,
	transformer = glue::identity_transformer,
	.and         = and::and,
	.or          = and::or,
	.incr        = sort,
	.decr        = function(x) sort(x, decreasing = TRUE),
	.bytes       = scales::label_bytes(),
	.date        = function(x) format(x, format = "%F"),
	.time        = function(x) format(x, format = "%T"),
	.datetime    = function(x) format(x, format = "%F %T"),
	.dollar      = scales::label_dollar(prefix = engine_pick("$", "$", "\\$")),
	.number      = scales::label_number(),
	.comma       = scales::label_comma(),
	.ordinal     = scales::label_ordinal(),
	.percent     = scales::label_percent(suffix = engine_pick("%", "%", "\\%")),
	.pvalue      = scales::label_pvalue(),
	.scientific  = scales::label_scientific(),
	.uppercase   = toupper,
	.lowercase   = tolower,
	.titlecase   = function(x) tools::toTitleCase(as.character(x)),
	.sentence    = function(x) `substr<-`(x, 1, 1, toupper(substr(x, 1, 1))),
	.squote      = function(x) sQuote(x, q = getOption("epoxy.fancy_quotes", FALSE)),
	.dquote      = function(x) dQuote(x, q = getOption("epoxy.fancy_quotes", FALSE)),
	.strong      = NULL,
	.emph        = NULL,
	.code        = NULL
) {
	force(transformer)

	dots <- rlang::dots_list(...)
	if (!all(nzchar(rlang::names2(dots)))) {
		rlang::abort("Arguments to `epoxy_transform_inline()` must all be named.")
	}
	not_dotted <- setdiff(names(dots), grep("^[.]", names(dots), value = TRUE))
	if (length(not_dotted) > 0) {
		not_dotted <- sprintf("`.%s` instead of `%s`", not_dotted, not_dotted)
		names(not_dotted) <- rep_len("*", length(not_dotted))
		rlang::abort(c(
			"Functions provided in `...` must be named with a leading dot (`.`).",
			"i" = "Did you mean to use the following?",
			not_dotted
		))
	}

	# for rcmdcheck
	tools::toTitleCase("")
	scales::percent(0.1)

	# Get the defaults for the inline transformers
	defaults <- epoxy_transform_inline_defaults()

	# Get user-provided arguments to this function call
	user <- rlang::call_args(match.call())
	user <- user[setdiff(names(user), c("...", "transformer"))]
	if (length(user)) {
		user <- lapply(user, rlang::eval_bare, env = parent.frame())
		user <- epoxy_transform_inline_add_aliases(user)
		user <- discard_null(user)
	}

	# Merge the user-provided arguments with the defaults
	fmts <- purrr::list_assign(defaults, !!!user)
	fmts <- epoxy_transform_inline_add_aliases(fmts)

	fmts_custom <- fmts[setdiff(names(fmts), names(defaults))]
	fmts_defaults <- fmts[names(defaults)]

	self <- function(text, envir) {
		if (detect_wrapped_delims(text)) {
			# we might need to remove one layer of evaluation
			text <- remove_outer_delims(text)
		}
		text <- text_orig <- trimws(text)
		'!DEBUG inline {text: "`text`"}'

		# https://github.com/r-lib/cli/blob/8d8a211c/R/inline.R#L241
		inline_regex <- "(?s)^[.]([-[:alnum:]_]+)[[:space:]]+(.*)"

		if (!grepl(inline_regex, text, perl = TRUE)) {
			"!DEBUG inline regex unmatched"
			return(transformer(text, envir))
		}

		class <- sub(inline_regex, "\\1", text, perl = TRUE)
		text_sans_class <- sub(inline_regex, "\\2", text, perl = TRUE)

		text <- remove_outer_delims(text_sans_class)
		class <- paste0(".", class) # restore leading dot

		# recurse into template before applying the current transformation
		text <- self(text, envir)

		maybe_custom_class <- function(text) {
			if (class %in% rlang::names2(fmts_custom)) {
				"!DEBUG inline custom class `{class}`"
				return(fmts_custom[[class]](text))
			}

			# if this isn't a known inline class, then we pass the original template
			# text to the next transformer, who might know what to do with it.
			"!DEBUG inline was unmatched"
			tryCatch(
				{
					'!DEBUG inline trying {text: "`text`"}'
					transformer(text_orig, envir)
				},
				error = function(err_og) {
					tryCatch(
						{
							'!DEBUG inline trying {text: "`text_sans_class`"}'
							transformer(text_sans_class, envir)
						},
						error = function(err_sc) {
							rlang::abort(
								glue("Could not evaluate text '{text_orig}`"),
								parent = err_og
							)
						}
					)
				}
			)
		}

		text_fn <-
			if (class %in% names(fmts_defaults)) {
				fmts_defaults[[class]]
			} else {
				maybe_custom_class
			}

		withCallingHandlers(
			text_fn(text),
			error = function(cnd) {
				rlang::abort(
					glue('Could not transform the text "{text}" using `{class}`.'),
					parent = cnd
				)
			}
		)
	}

	self
}

detect_wrapped_delims <- function(text, open = NULL, close = NULL) {
	delims <- getOption("epoxy:::private", list())
	open <- open %||% delims$.open %||% "{"
	close <- close %||% delims$.close %||% "}"

	text <- trimws(text)

	first_open <- function(open) {
		gregexpr(open, text, fixed = TRUE)[[1]][[1]]
	}

	idx_open <- first_open(open)
	idx_open_escaped <- first_open(strrep(open, 2))
	if (idx_open == idx_open_escaped || idx_open != 1) {
		return(FALSE)
	}

	last_close <- function(close) {
		idx_close <- gregexpr(close, text, fixed = TRUE)[[1]]
		idx_close[[length(idx_close)]]
	}

	idx_close <- last_close(close)
	idx_close_escaped <- last_close(strrep(close, 2))

	idx_final_close <- nchar(text) - nchar(close) + 1
	idx_final_escaped <- nchar(text) - (nchar(close) * 2) + 1

	ends_with_close <- idx_close == idx_final_close
	ends_with_escaped <- idx_close_escaped == idx_final_escaped

	if (ends_with_escaped || !ends_with_close) {
		return(FALSE)
	}

	TRUE
}

remove_outer_delims <- function(text) {
	delims <- getOption("epoxy:::private", list())
	open <- delims$.open %||% "{"
	close <- delims$.close %||% "}"

	if (!grepl(open, text, fixed = TRUE)) {
		return(text)
	}

	text <- strsplit(text, open, fixed = TRUE)[[1]][-1]
	text <- paste(text, collapse = open)
	text <- strsplit(text, close, fixed = TRUE)[[1]]
	# collapse removes the final closer for us
	text <- paste(text, collapse = close)
	attr(text, "is_inner_expr") <- TRUE
	text
}

.globals$inline <- list(md = list(), html = list(), latex = list())

epoxy_transform_inline_defaults <- function() {
	# Base defaults are stored in the function arguments
	fn <- epoxy_transform_inline
	defaults <- formals(fn)
	defaults <- defaults[setdiff(names(defaults), c("...", "transformer"))]
	defaults <- lapply(defaults, rlang::eval_bare, env = rlang::get_env(fn))

	# A few defaults are internal
	# FIXME: do this inside epoxy_transform_inline()
	defaults$.comma <- epoxy_comma(defaults$.comma)
	defaults$.strong <- epoxy_bold
	defaults$.emph <- epoxy_italic
	defaults$.code <- epoxy_code

	# User-supplied session defaults
	if (length(.globals$inline) > 0) {
		engine <- engine_current("md")
		session <- .globals[["inline"]][[engine]]
		session <- purrr::compact(session)
		defaults <- purrr::list_assign(defaults, !!!session)
	}
	defaults
}

epoxy_inline_aliases <- c(
	.bold   = ".strong",
	.italic = ".emph",
	.dttm   = ".datetime",
	.num    = ".number",
	.pct    = ".percent",
	.uc     = ".uppercase",
	.lc     = ".lowercase",
	.tc     = ".titlecase",
	.sc     = ".sentence"
)

epoxy_transform_inline_add_aliases <- function(fmts) {
	aliases <- epoxy_inline_aliases
	aliases <- aliases[aliases %in% names(fmts)]

	for (i in seq_along(aliases)) {
		alias <- names(aliases)[i]
		impl <- aliases[[i]]
		if (!alias %in% names(fmts) && impl %in% names(fmts)) {
			fmts[[alias]] <- fmts[[impl]]
		}
	}

	fmts
}

epoxy_comma <- function(comma_numeric) {
	force(comma_numeric)
	function(x) {
		if (is.character(x)) {
			return(paste(x, collapse = ", "))
		}
		comma_numeric(x)
	}
}

epoxy_bold <- function(text) {
	text <- engine_pick(text, escape_html(text))
	before <- engine_pick("**", "<strong>", "\\textbf{")
	after <- engine_pick("**", "</strong>", "}")
	x <- paste0(before, text, after)
	engine_pick(x, html_chr(x))
}

epoxy_italic <- function(text) {
	text <- engine_pick(text, escape_html(text))
	before <- engine_pick("_", "<em>", "\\emph{")
	after <- engine_pick("_", "</em>", "}")
	x <- paste0(before, text, after)
	engine_pick(x, html_chr(x))
}

epoxy_code <- function(text) {
	text <- engine_pick(text, escape_html(text))
	before <- engine_pick("`", "<code>", "\\texttt{")
	after <- engine_pick("`", "</code>", "}")
	x <- paste0(before, text, after)
	engine_pick(x, html_chr(x))
}

# nocov start
roxy_inline_params <- function() {
	args <- rlang::fn_fmls(epoxy_transform_inline)
	args <- args[setdiff(names(args), c("...", "transformer"))]
	args <- purrr::map(args, rlang::expr_text)
	args <- purrr::map_chr(args, function(expr) {
		if (expr == "NULL") {
			return("chosen internally based on the output format")
		}
		if (grepl("^function", expr)) {
			# internal defaults, defined in the function signature
			return(paste("``", expr, "``"))
		}
		if (!grepl("\\(.*\\)", expr)) {
			# default is a function, e.g. and::and
			return(sprintf("[%s()]", expr))
		}
		# default is a function call, maybe with arguments
		link <- sub("\\(.*\\)$", "", expr)
		return(sprintf("[%s][%s]", expr, link))
	})

	extras <- epoxy_inline_aliases

	values <- purrr::map_chr(names(args), function(label) {
		label <- c(label, names(extras[extras == label]))
		knitr::combine_words(label, before = "`{", after = " x}`", and = " or ")
	})

	glue(
		"@param {param} The function to apply to `x` when the template is {values}. ",
		"Default is {fn}.",
		param = names(args),
		fn = unname(args),
		values = values
	)
}
# nocov end
