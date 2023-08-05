#' Mustache-style string interpolation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' A wrapper around the [mustache templating
#' language](http://mustache.github.io/), provided by the
#' [whisker](https://cran.r-project.org/package=whisker) package. Under the
#' hood, `epoxy_mustache()` uses [whisker::whisker.render()] to render the
#' template, but adds a few conveniences:
#'
#' * The template can be passed in `...` as a single string, several strings or
#'   as a vector of strings. If multiple strings are passed, they are collapsed
#'   with `.sep` (`"\n"` by default).
#'
#' * `epoxy_mustache()` can be vectorized over the items in the `.data`
#'   argument. If `.data` is a data frame, vectorization is turned on by default
#'   so that you can  iterate over the rows of the data frame. The output will
#'   be a character vector of the same length as the number of rows in the data
#'   frame.
#'
#' @examples man/examples/epoxy_mustache.R
#'
#' @param ... A string or a vector of strings containing the template(s). Refer
#'   to the [mustache documentation](http://mustache.github.io/) for an overview
#'   of the syntax. If multiple strings are passed, they are collapsed with
#'   `.sep` (`"\n"` by default).
#' @param .data A data frame or a list. If `.data` is a data frame,
#'   `epoxy_mustache()` will transform the data frame so that the template can
#'   be applied to each row of the data frame. To avoid this transformation,
#'   wrap the `.data` value in `I()`.
#' @param .sep The separator to use when collapsing multiple strings passed in
#'   `...` into a single template. Defaults to `"\n"`.
#' @param .vectorized If `TRUE` , `epoxy_mustache()` will vectorize over the
#'   items in `.data`. In other words, each item or row of `.data` will be used
#'   to render the template once. By default, `.vectorized` is set to `TRUE` if
#'   `.data` is a data frame and `FALSE` otherwise.
#' @param .partials A named list with partial templates. See
#'   [whisker::whisker.render()] or the [mustache
#'   documentation](http://mustache.github.io/mustache.5.html#Partials) for
#'   details.
#'
#' @return A character vector of length 1 if `.vectorized` is `FALSE` or a
#'   character vector of the same length as the number of rows or items in
#'   `.data` if `.vectorized` is `TRUE`.
#'
#' @family Mustache-style template functions
#' @export
epoxy_mustache <- function(
	...,
	.data = parent.frame(),
	.sep = "\n",
	.vectorized = inherits(.data, "data.frame"),
	.partials = list()
) {
	dots <- list_split_named(rlang::list2(...))

	if (length(dots$named)) {
		rlang::abort("Named arguments are not supported in `epoxy_mustache()`.")
	}

	template <- paste(dots$unnamed, collapse = .sep)

	is_data_asis <- inherits(.data, "AsIs")

	whisker_render <- purrr::partial(
		whisker::whisker.render,
		template = template,
		partials = .partials,
		strict = TRUE
	)

	if (!isTRUE(.vectorized)) {
		return(as_glue_chr(whisker_render(data = .data)))
	}

	if (!is_data_asis) {
		.data <- prep_whisker_data(.data)
	}

	as_glue_chr(purrr::map_chr(.data, whisker_render))
}
