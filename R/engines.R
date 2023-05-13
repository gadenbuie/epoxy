#' Use the epoxy knitr engines
#'
#' @description
#' Sets \pkg{epoxy}'s \pkg{knitr} engines for use by \pkg{knitr} in R Markdown
#' and other document formats powered by \pkg{knitr}. These engines are also
#' set up when loading \pkg{epoxy} with `library()`, so in general you will not
#' need to call this function explicitly.
#'
#' \pkg{epoxy} provides four \pkg{knitr} engines:
#'
#' * `epoxy` uses default \pkg{glue} syntax, e.g. `{var}` for markdown outputs
#' * `epoxy_html` uses double brace syntax, e.g. `{{var}}` for HTML outputs
#' * `epoxy_latex` uses double angle brackets syntax, e.g. `<<var>>` for LaTeX
#'   outputs
#' * `whisker` uses the \pkg{whisker} package which provides an R-based
#'   implementation of the [mustache](https://mustache.github.io/) templating
#'   language.
#'
#' For historical reasons, aliases for the HTML and LaTeX engines are also
#' created: `glue_html` and `glue_latex`. You may opt into a third alias —
#' `glue` for the `epoxy` engine — by calling `use_epoxy_glue_engine()`, but
#' note that this will most likely overwrite the `glue` engine provided by the
#' \pkg{glue} package.
#'
#' @examplesIf interactive()
#' use_epoxy_knitr_engines()
#'
#' @param use_glue_engine If `TRUE` (default `FALSE`), uses \pkg{epoxy}'s `glue`
#'   engine, most likely overwriting the `glue` engine provided by \pkg{glue}.
#'
#' @return Silently sets \pkg{epoxy}'s knitr engines and invisible returns
#'   [knitr::knit_engines] as they were prior to the function call.
#'
#' @export
use_epoxy_knitr_engines <- function(use_glue_engine = FALSE) {
	old <- knitr::knit_engines$get()

	knitr::knit_engines$set(
		epoxy         = knitr_engine_epoxy,
		"epoxy_html"  = knitr_engine_epoxy_html,
		"glue_html"   = knitr_engine_epoxy_html,
		"epoxy_latex" = knitr_engine_epoxy_latex,
		"glue_latex"  = knitr_engine_epoxy_latex,
		"whisker"     = knitr_engine_whisker
	)

	if (isTRUE(use_glue_engine)) {
		use_epoxy_glue_engine()
	}

	invisible(old)
}

#' @describeIn use_epoxy_knitr_engines Use \pkg{epoxy}'s `epoxy` engine as
#'   the `glue` engine.
#' @export
use_epoxy_glue_engine <- function() {
	old <- knitr::knit_engines$get()
	knitr::knit_engines$set(glue = knitr_engine_epoxy)
	.globals$use_epoxy_glue_engine <- TRUE
	invisible(old)
}

#' Epoxy string interpolation
#'
#' @description
#' The functions power the knitr chunk engines and are wrappers around
#' [glue::glue()], with a few extra conveniences provided by \pkg{epoxy}. Each
#' of these functions can be called directly or used as a knitr chunk engine
#' where the chunk text is handled as if it were a string passed into the
#' function version. When used as a knitr chunk engine, the function arguments
#' can be passed in as chunk options.
#'
#' All of `epoxy()`, `epoxy_html()` and `epoxy_latex()` use
#' [epoxy_transform_inline()] by default. This transformer brings a concise
#' inline-formatting syntax that you can read more about in
#' `?epoxy_transform_inline`.
#'
#' `epoxy_html()` also includes an inline transformation syntax that makes it
#' easier to wrap the expression text in HTML elements with a specific ID or
#' a set of classes. Learn more about this syntax in `?epoxy_transform_html`.
#'
#' @example man/examples/epoxy.R
#'
#' @param .data A data set
#' @param .transformer A transformer function or transformer chain created with
#'   [epoxy_transform()]. Alternatively, a character vector of epoxy transformer
#'   names, e.g. `c("bold", "collapse")` or a list of epoxy transformers, e.g.
#'   `list(epoxy_transform_bold(), epoxy_transform_collapse())`.
#'
#'   In \pkg{epoxy}, you'll most likely want to use the defaults or consult
#'   [epoxy_transform()] for more information. See also [glue::glue()] for more
#'   information on transformers.
#' @param .style `r lifecycle::badge("deprecated")` Please use `.transformer`
#'   instead.
#### Inlined from https://github.com/tidyverse/glue/blob/main/R/glue.R#L15-L18
#### to avoid https://github.com/r-lib/roxygen2/issues/1355
#' @param .open \[`character(1)`: \sQuote{\\\{}]\cr The opening delimiter around
#'   the template variable or expression. Doubling the full delimiter escapes
#'   it.
#' @param .close \[`character(1)`: \sQuote{\\\}}]\cr The closing delimiter
#'   around the template variable or expression. Doubling the full delimiter
#'   escapes it.
####
#' @inheritParams glue::glue
#'
#' @return Returns a transformed string, using `glue::glue()` but with the
#'   additional transformers provided to the `.transformer` argument of
#'   `epoxy()`.
#'
#' @describeIn epoxy super `glue()`
#' @export
epoxy <- function(
	...,
	.data = NULL,
	.sep = "",
	.envir = parent.frame(),
	.open = "{",
	.close = "}",
	.na = "",
	.null = "",
	.comment = character(),
	.literal = FALSE,
	.trim = FALSE,
	.transformer = NULL,
	.style = lifecycle::deprecated()
) {
	if (lifecycle::is_present(.style)) {
		lifecycle::deprecate_soft(
			when = "0.1.0",
			what = "epoxy(.style = )",
			with = "epoxy(.transformer = )"
		)
	} else if (identical(.style, quote(expr = ))) { # rlang::is_missing()
		.style <- NULL
	}

	glue_env <- .envir
	if (!is.null(.data)) {
		glue_env <- new.env(parent = .envir)
		assign("$", epoxy_data_subset, envir = glue_env)
	}

	opts_transformer <- list(
		.transformer = .transformer,
		# TODO(lifecycle): .style was deprecated 2023-05
		epoxy_style = .style
	)

	old_opts <- options("epoxy:::private" = list(.open = .open, .close = .close))
	on.exit(old_opts, add = TRUE)

	glue_data(
		.x = .data,
		...,
		.sep     = .sep,
		.envir   = glue_env,
		.open    = .open,
		.close   = .close,
		.na      = .na,
		.null    = .null,
		.comment = .comment,
		.literal = .literal,
		.trim    = .trim,
		.transformer = epoxy_options_get_transformer(opts_transformer)
	)
}

knitr_engine_epoxy <- function(options) {
	deprecate_glue_engine_prefix(options)
	deprecate_epoxy_style_chunk_option(options)

	out <- if (isTRUE(options$eval)) {
		options <- deprecate_glue_data_chunk_option(options)
		code <- paste(options$code, collapse = "\n")

		epoxy(
			code,
			.data        = options[["data"]],
			.sep         = "",
			.envir       = options[[".envir"]]   %||% knitr::knit_global(),
			.open        = options[[".open"]]    %||% "{",
			.close       = options[[".close"]]   %||% "}",
			.na          = options[[".na"]]      %||% "",
			.null        = options[[".null"]]    %||% "",
			.trim        = options[[".trim"]]    %||% FALSE,
			.comment     = options[[".comment"]] %||% "",
			.literal     = options[[".literal"]] %||% FALSE,
			.transformer = epoxy_options_get_transformer(options)
		)
	}

	options$results <- "asis"
	options$echo <- knitr_chunk_option_echo(options)
	knitr::engine_output(options, options$code, out)
}

#' @describeIn epoxy super `glue()` for HTML
#' @export
epoxy_html <- function(
	...,
	.data = NULL,
	.sep = "",
	.envir = parent.frame(),
	.open = "{{",
	.close = "}}",
	.na = "",
	.null = "",
	.comment = "",
	.literal = FALSE,
	.trim = FALSE,
	.transformer = NULL
) {
	res <-
		with_options(
			list(epoxy.engine = "html"),
			epoxy(
				...,
				.data = .data,
				.sep = .sep,
				.envir = .envir,
				.open = .open,
				.close = .close,
				.na = .na,
				.null = .null,
				.comment = .comment,
				.literal = .literal,
				.trim = .trim,
				.transformer = .transformer
			)
		)
	html_chr(res)
}

knitr_engine_epoxy_html <- function(options) {
	deprecate_glue_engine_prefix(options)
	deprecate_epoxy_style_chunk_option(options)

	out <- NULL
	if (isTRUE(options$eval) && is_htmlish_output()) {
		options <- deprecate_glue_data_chunk_option(options)
		code <- paste(options$code, collapse = "\n")

		out <- epoxy(
			code,
			.data        = options[["data"]],
			.sep         = "",
			.envir       = options[[".envir"]]   %||% knitr::knit_global(),
			.open        = options[[".open"]]    %||% "{{",
			.close       = options[[".close"]]   %||% "}}",
			.na          = options[[".na"]]      %||% "",
			.null        = options[[".null"]]    %||% "",
			.trim        = options[[".trim"]]    %||% FALSE,
			.comment     = options[[".comment"]] %||% "",
			.literal     = options[[".literal"]] %||% FALSE,
			.transformer = epoxy_options_get_transformer(options)
		)

		out <- glue_collapse(out, sep = "\n")

		if (isTRUE(options$html_raw %||% TRUE)) {
			# use pandoc's raw html block by default, but this isn't always available
			# so it can be disabled with the html_raw chunk option.
			out <- paste0('```{=html}\n', out, "\n```")
		}
	}
	options$results <- "asis"
	options$echo <- knitr_chunk_option_echo(options)
	knitr::engine_output(options, options$code, out)
}

#' @describeIn epoxy super `glue()` for LaTeX
#' @export
epoxy_latex <- function(
	...,
	.data = NULL,
	.sep = "",
	.envir = parent.frame(),
	.open = "<",
	.close = ">",
	.na = "",
	.null = "",
	.comment = "",
	.literal = FALSE,
	.trim = FALSE,
	.transformer = NULL
) {
	with_options(
		list(epoxy.engine = "latex"),
		epoxy(
			...,
			.data = .data,
			.sep = .sep,
			.envir = .envir,
			.open = .open,
			.close = .close,
			.na = .na,
			.null = .null,
			.comment = .comment,
			.literal = .literal,
			.trim = .trim,
			.transformer = .transformer
		)
	)
}

knitr_engine_epoxy_latex <- function(options) {
	deprecate_glue_engine_prefix(options)
	deprecate_epoxy_style_chunk_option(options)

	out <- NULL
	if (isTRUE(options$eval)) {
		options <- deprecate_glue_data_chunk_option(options)
		code <- paste(options$code, collapse = "\n")

		out <- epoxy(
			code,
			.data        = options[["data"]],
			.sep         = "",
			.envir       = options[[".envir"]]   %||% knitr::knit_global(),
			.open        = options[[".open"]]    %||% "<",
			.close       = options[[".close"]]   %||% ">",
			.na          = options[[".na"]]      %||% "",
			.null        = options[[".null"]]    %||% "",
			.trim        = options[[".trim"]]    %||% FALSE,
			.comment     = options[[".comment"]] %||% "",
			.literal     = options[[".literal"]] %||% FALSE,
			.transformer = epoxy_options_get_transformer(options)
		)

		out <- glue_collapse(out, sep = "\n")

		if (isTRUE(options$latex_raw %||% TRUE)) {
			# use pandoc's raw latex block by default, but allow it to be disabled
			out <- paste0("```{=latex}\n", out, "\n```")
		}
	}

	options$results <- "asis"
	options$echo <- knitr_chunk_option_echo(options)
	knitr::engine_output(options, options$code, out)
}

knitr_engine_whisker <- function(options) {
	out <- if (isTRUE(options$eval)) {
		options <- deprecate_glue_data_chunk_option(options)
		code <- paste(options$code, collapse = "\n")
		code <- if (!is.null(options[["data"]])) {
			if (isTRUE(options[["data_asis"]])) {
				whisker::whisker.render(code, data = options[["data"]])
			} else {
				vapply(
					prep_whisker_data(options[["data"]]),
					function(d) {
						whisker::whisker.render(code, data = d)
					},
					character(1)
				)
			}
		} else {
			whisker::whisker.render(code, options[[".envir"]] %||% knitr::knit_global())
		}
		code <- glue_collapse(code, sep = "\n")
		if (isTRUE(options$html_raw %||% FALSE)) {
			# use pandoc's raw html block by default, but this isn't always available
			# so it can be disabled with the html_raw chunk option.
			code <- paste0("\n```{=html}\n", code, "\n```")
		}
		code
	}
	options$results <- "asis"
	options$echo <- knitr_chunk_option_echo(options)
	knitr::engine_output(options, options$code, out)
}

prep_whisker_data <- function(x) {
	if (!is.list(x) && !inherits(x, "list")) {
		stop("data must be a list or a list-alike", call. = FALSE)
	}
	if (is.null(names(x)) || !all(nzchar(names(x)))) {
		stop("data must be a named list or list-alike", call. = FALSE)
	}
	x_len <- vapply(x, length, integer(1))
	x_null <- vapply(x, is.null, logical(1))
	if (length(unique(x_len[!x_null])) != 1 && !all(x_len[!x_null] > 0)) {
		stop("data must be the same length: ", paste(x_len[!x_null], collapse = ", "), call. = FALSE)
	}

	# turn list(a = 1:2, b = 3:4)
	# into list(list(a = 1, b = 3), list(a = 2, b = 4))
	lapply(seq_len(max(x_len)), function(i) lapply(x, function(y) y[[i]]))
}

epoxy_data_subset <- function(x, y) {
	y <- substitute(y)
	x <- lapply(x, function(.x) base::`[[`(.x, y))
	x_len_1 <- vapply(x, function(x) length(x) == 1, logical(1))
	if (all(x_len_1)) unlist(x) else x
}

epoxy_options_get_transformer <- function(options) {
	transformer <- options[[".transformer"]] %||%
		# for backwards compatibility continue to check `epoxy_style` chunk option
		options[["epoxy_style"]]

	if (is.null(transformer)) {
		return(epoxy_default_transformer())
	}

	if (rlang::is_function(transformer)) {
		return(transformer)
	}

	if (rlang::is_vector(transformer) || rlang::is_list(transformer)) {
		return(epoxy_transform(!!!transformer))
	}

	epoxy_default_transformer()
}

epoxy_default_transformer <- function() {
	engine_pick(
		md = getOption("epoxy.transformer_default.md", NULL),
		html = getOption("epoxy.transformer_default.html", NULL),
		latex = getOption("epoxy.transformer_default.latex", NULL)
	) %||%
		engine_pick(
			md = epoxy_transform("inline"),
			html = epoxy_transform("inline", "html")
		)
}

knitr_chunk_option_echo <- function(options) {
	# is echo set locally on the chunk?
	chunk_opts <- attr(knitr::knit_code$get(options$label), "chunk_opts")
	# if not, follow `.echo` or default to FALSE
	chunk_opts[["echo"]] %||% options[[".echo"]] %||% FALSE
}

deprecate_glue_data_chunk_option <- function(options) {
	if ("glue_data" %in% names(options)) {
		lifecycle::deprecate_stop(
			when = "0.0.2",
			what = I("The `glue_data` chunk option"),
			with = I("the `data` chunk option")
		)
	}
	options
}

deprecate_epoxy_style_chunk_option <- function(options) {
	if (is.null(options[["epoxy_style"]])) return()

	lifecycle::deprecate_soft(
		when = "0.1.0",
		what = "epoxy(.style =)",
		details = c(
			"The corresponding `epoxy_style` chunk option is also deprecated.",
			"i" = "Please rename the chunk option to use `.transformer` instead."
		)
	)
}

deprecate_glue_engine_prefix <- function(options) {
	requested_glue_engine <- isTRUE(.globals$use_epoxy_glue_engine)

	if (!requested_glue_engine && identical(options$engine, "glue")) {
		lifecycle::deprecate_soft(
			when = "0.0.3",
			what = I("The epoxy-provided `glue` engine"),
			with = I("the `epoxy` engine"),
			details = c(
				i = "The `glue` engine is now provided by the {glue} package."
			)
		)
	}

	if (options$engine %in% c("glue_latex", "glue_html")) {
		engine <- options$engine
		suggested <- sub("^glue", "epoxy", engine)
		lifecycle::deprecate_soft(
			when = "0.0.3",
			what = I(glue("The `{engine}` knitr engine")),
			with = I(glue("the `{suggested}` engine"))
		)
	}
}
