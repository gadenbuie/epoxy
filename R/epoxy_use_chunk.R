#' Reuse a Template Chunk
#'
#' @description
#' Reuse a chunk
#'
#' ## Use in R Markdown or Quarto
#'
#' ``````
#' ```{epoxy movie-release}
#' {.emph title} was released in {year}.
#' ```
#'
#' ```{r}
#' # Re-using the template we defined above
#' epoxy_use_chunk(bechdel[1, ], "movie-release")
#' ```
#'
#' ```{r}
#' # Using in a dplyr pipeline
#' bechdel |>
#'   dplyr::filter(year == 1989) |>
#'   epoxy_use_chunk("movie-release")
#' ```
#' ``````
#'
#' Or you can even use it inline:
#'
#' ``````
#' > `r epoxy_use_chunk(bechdel[2, ], "movie-release")`
#' ``````
#'
#' ## Template Options
#'
#' When rendering a template, `epoxy_use_chunk()` will inherit the options
#' set in a number of different ways. The final template options are determined
#' in the following order, ranked by importance. Options set in a higher-ranked
#' location will override options set in a lower-ranked location.
#'
#' 1. The arguments passed to `epoxy_use_chunk()`, such as `.data` or any
#'    arguments passed in the `...`. These options always have preference over
#'    options set anywhere else.
#'
#' 2. The chunk options from the template chunk. These options typically are
#'    relevant to the template itself, such as the engine used or the opening
#'    and closing delimiters.
#'
#' 3. The current knitr options for the chunk where the template is rendered.
#'    In most cases, this will be the global knitr options, but you could still
#'    override these options locally in the chunk where you render the template.
#'
#' @param label The chunk label, i.e. the human-readable name of the chunk.
#' @inheritDotParams epoxy
#' @inheritParams epoxy
#'
#' @return A character string of the rendered template based on the `label`
#'   chunk. The results are marked as `"asis"` output so that they are treated
#'   as regular text rather than being displayed as code results.
#'
#' @family Templating functions
#' @export
epoxy_use_chunk <- function(.data = NULL, label, ...) {
	if (!rlang::is_string(label)) {
		rlang::abort("`label` must be a string")
	}

	if (!label %in% knitr::all_labels()) {
		rlang::abort(paste0('Unknown chunk label "', label, '"'))
	}

	template <- knitr_chunk_get(label)

	# For options, we want to apply options in this order:
	# 0. `.data` from this fn and `eval` from this chunk
	# 1. Options from this function call in the ...
	# 2. Options specifically on the calling chunk
	# 2. Options from the chunk in the template
	# 3. Global knitr options in the current environment
	opts_fn <- rlang::list2(eval = TRUE, ...)
	if (!is.null(.data)) opts_fn[[".data"]] <- .data

	opts_global <- knitr::opts_current$get()
	opts_current <- knitr_chunk_specific_options()

  # global << template << current << function
	opts <- opts_global
	opts <- purrr::list_assign(opts, !!!template$opts)
	opts <- purrr::list_assign(opts, !!!opts_current)
	opts <- purrr::list_assign(opts, !!!purrr::compact(opts_fn))

	fn <- switch(
		template$opts$engine,
		epoxy = epoxy,
		epoxy_html = epoxy_html,
		epoxy_latex = epoxy_latex,
		mustache = epoxy_mustache,
		whisker = epoxy_whisker,
		glue = epoxy_glue,
		glue_html = epoxy_html,
		glue_latex = epoxy_latex,
		epoxy
	)

	call <- rlang::call2(
		"eval_epoxy_engine",
		fn = fn,
		code = template$code,
		options = opts
	)

	knitr::asis_output(eval(call))
}
