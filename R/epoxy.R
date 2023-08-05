#' Epoxy string interpolation
#'
#' @description
#' These functions power the knitr chunk engines and are wrappers around
#' [glue::glue()], with a few extra conveniences provided by \pkg{epoxy}.
#'
#' * `epoxy()` is super `glue::glue()`.
#' * `epoxy_html()` is super `glue::glue()` with HTML-specific defaults.
#' * `epoxy_latex()` is super `glue::glue()` with LaTeX-specific defaults.
#'
#' Each of these functions can be called directly or used as a knitr chunk
#' engine where the chunk text is handled as if it were a string passed into the
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
#' @seealso
#'   * [use_epoxy_knitr_engines()] for knitr engines powered by these epoxy
#'     functions.
#'   * [epoxy_mustache()] for more powerful templating needs when you don't
#'     need epoxy's inline formatting syntax.
#'
#' @rdname epoxy
#' @name epoxy
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
		assign(".data", .data, envir = glue_env)
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


#' @rdname epoxy
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
		with_epoxy_engine(
			"html",
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


#' @rdname epoxy
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
	with_epoxy_engine(
		"latex",
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

with_epoxy_engine <- function(engine, expr) {
	with_options(
		list(epoxy.engine = engine_validate_alias(engine)),
		expr
	)
}

epoxy_data_subset <- function(x, y) {
	y <- substitute(y)
	exact <- inherits(x, "tbl_df")

	if (identical(deparse(substitute(x)), ".data")) {
		return(base::`[[`(x, y, exact = exact))
	}

	ret <- tryCatch(base::`[[`(x, y, exact = exact), error = function(...) NULL)
	if (!is.null(ret)) return(ret)

	z <- lapply(x, function(.x) base::`[[`(.x, y, exact = exact))
	z_len_1 <- vapply(z, function(z) length(z) == 1, logical(1))
	if (all(z_len_1)) unlist(z) else z
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
