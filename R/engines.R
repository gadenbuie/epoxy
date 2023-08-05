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

prefer_dotted_data_option <- function(options) {
	if (!"data" %in% names(options)) {
		return(options)
	}
	both_provided <- ".data" %in% names(options)

	lifecycle::deprecate_warn(
		when = "0.2.0",
		what = I("The `data` chunk option"),
		with = I("the `.data` option (note the leading dot)"),
		details = if (both_provided) {
			"Both `data` and `.data` were provided. The `.data` option will be used."
		}
	)

	if (both_provided) {
		return(options)
	}

	# rename "data" to ".data"
	names(options)[which(names(options) == "data")] <- ".data"
	options
}

eval_epoxy_engine <- function(fn, code, options) {
	defaults <- formals(fn)
	exclude <- c("...", ".data", ".style", ".transformer")
	defaults <- defaults[setdiff(names(defaults), exclude)]
	defaults <- lapply(defaults, rlang::eval_bare, env = environment(fn))
	defaults$.envir <- knitr::knit_global()

	chunk_opt_names <- c("data", ".data", names(defaults))
	chunk_opts <- options[intersect(chunk_opt_names, names(options))]

	chunk_opts <- prefer_dotted_data_option(chunk_opts)

	args <- purrr::list_assign(defaults, !!!chunk_opts)
	args$.transformer <- epoxy_options_get_transformer(options)

	out <- rlang::exec(fn, code, !!!args)
	glue_collapse(out, sep = "\n")
}

knitr_engine_epoxy <- function(options) {
	deprecate_glue_engine_prefix(options)
	deprecate_epoxy_style_chunk_option(options)

	out <- if (isTRUE(options$eval)) {
		options <- deprecate_glue_data_chunk_option(options)
		code <- paste(options$code, collapse = "\n")

		out <- eval_epoxy_engine(epoxy, code, options)
	}

	options$results <- "asis"
	options$output <- "asis"
	options$echo <- knitr_chunk_option_echo(options)
	knitr::engine_output(options, options$code, out)
}

knitr_engine_epoxy_html <- function(options) {
	deprecate_glue_engine_prefix(options)
	deprecate_epoxy_style_chunk_option(options)

	out <- NULL
	if (isTRUE(options$eval) && is_htmlish_output()) {
		options <- deprecate_glue_data_chunk_option(options)
		code <- paste(options$code, collapse = "\n")

		out <- eval_epoxy_engine(epoxy_html, code, options)

		if (isTRUE(options$html_raw %||% TRUE)) {
			# use pandoc's raw html block by default, but this isn't always available
			# so it can be disabled with the html_raw chunk option.
			out <- paste0("```{=html}\n", out, "\n```")
		}
	}
	options$results <- "asis"
	options$output <- "asis"
	options$echo <- knitr_chunk_option_echo(options)
	knitr::engine_output(options, options$code, out)
}

knitr_engine_epoxy_latex <- function(options) {
	deprecate_glue_engine_prefix(options)
	deprecate_epoxy_style_chunk_option(options)

	out <- NULL
	if (isTRUE(options$eval)) {
		options <- deprecate_glue_data_chunk_option(options)
		code <- paste(options$code, collapse = "\n")

		out <- eval_epoxy_engine(epoxy_latex, code, options)

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
		options <- prefer_dotted_data_option(options)

		if (
			!is.null(options[[".data"]]) &&
				isTRUE(options[["data_asis"]]) &&
				!inherits(options[[".data"]], "AsIs")
		) {
			options[[".data"]] <- I(options[[".data"]])
		}

		code <- epoxy_mustache(
			!!!options[["code"]],
			.data = options[[".data"]],
			.sep = "\n",
			.vectorized = options[[".vectorized"]] %||%
				inherits(options[[".data"]], "data.frame"),
			.partials = options[[".partials"]]
		)

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

	# turn list(a = 1:2, b = 3:4, c = 5)
	# into list(list(a = 1, b = 3, c = 5), list(a = 2, b = 4, c = 5))
	lapply(seq_len(max(x_len)), function(i) lapply(x, function(y) {
		y[[if (length(y) == 1) 1 else i]]
	}))
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
	if (is.null(options[["epoxy_style"]])) {
		return()
	}

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
