#' Reuse a Template Chunk
#'
#' @description
#' Reuse a template from another chunk or file. By calling `epoxy_use_chunk()`
#' in an R chunk or inline R expression, you can reuse a template defined in
#' another chunk in your document.
#'
#' Alternatively, you can store the template in a separate file and use
#' `epoxy_use_file()` to reuse it. When stored in a file, the template file can
#' contain YAML front matter (following the [same rules as pandoc
#' documents](https://pandoc.org/MANUAL.html#extension-yaml_metadata_block))
#' with options that should be applied when calling an epoxy function. The
#' specific function called by `epoxy_use_file()` can be set via the `engine`
#' option in the YAML front matter; the default is [epoxy()].
#'
#' @section Use in R Markdown or Quarto:
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
#' ``````markdown
#' It's hard to believe that
#' `r epoxy_use_chunk(bechdel[2, ], "movie-release")`.
#' ``````
#'
#' It's hard to believe that
#' _Back to the Future Part II_ was released in 1989.
#'
#' The same template could also be stored in a file, e.g. `movie-release.md`:
#'
#' ```markdown
#' ---
#' engine: epoxy
#' ---
#'
#' {.emph title} was released in {year}.
#' ```
#'
#' The YAML front matter is used in template files to set options for the
#' template. You can use the `engine` option to choose the epoxy function to be
#' applied to the template, e.g. `engine: epoxy_html` or `engine: epoxy_latex`.
#' By default, `engine: epoxy` is assumed unless otherwise specified.
#'
#' @section Template Options:
#'
#' When rendering a template, `epoxy_use_chunk()` and `epoxy_use_file()` will
#' inherit the options set in a number of different ways. The final template
#' options are determined in the following order, ranked by importance. Options
#' set in a higher-ranked location will override options set in a lower-ranked
#' location.
#'
#' 1. The arguments passed to `epoxy_use_chunk()`, such as `.data` or any
#'    arguments passed in the `...`. These options always have preference over
#'    options set anywhere else.
#'
#' 1. The chunk options from the chunk where `epoxy_use_chunk()` or
#'    `epoxy_use_file()` is called.
#'
#' 1. The chunk options from the template chunk or file. These options typically
#'    are relevant to the template itself, such as the engine used or the
#'    opening and closing delimiters.
#'
#' 1. Global knitr chunk options for the document. You can set these with
#'    `knitr::opts_chunk$set()`, see `?knitr::opts_chunk` for more information.
#'
#' @param label The chunk label, i.e. the human-readable name, of the chunk
#'   containing the template string. This chunk should be an `epoxy`,
#'   `epoxy_html` or other epoxy-provided chunk type and it must have a label.
#'   `epoxy_use_chunk()` will apply the options from this chunk to the template,
#'   giving preference to arguments in `epoxy_use_chunk()` or the chunk options
#'   where it is called. See the "Template Options" section for more details.
#' @inheritDotParams epoxy
#' @inheritParams epoxy
#'
#' @return A character string of the rendered template based on the `label`
#'   chunk. The results are marked as `"asis"` output so that they are treated
#'   as regular text rather than being displayed as code results.
#'
#' @family Templating functions
#' @name epoxy_use
#' @export
epoxy_use_chunk <- function(.data = NULL, label, ...) {
	if (!rlang::is_string(label)) {
		rlang::abort("`label` must be a string")
	}

	if (!label %in% knitr::all_labels()) {
		rlang::abort(paste0('Unknown chunk label "', label, '"'))
	}

	template <- knitr_chunk_get(label)

	epoxy_use_template(
		template$code,
		.data = .data,
		...,
		options = template$opts
	)
}

#' @param file The template file, i.e. a plain text file, containing the
#'   template. An `.md` or `.txt` file extension is recommended. In addition to
#'   the template, the file may also contain YAML front matter containing
#'   options that are used when rendering the template via [epoxy()].
#'
#' @rdname epoxy_use
#' @export
epoxy_use_file <- function(.data = NULL, file, ...) {
	if (!file.exists(file)) {
		rlang::abort(paste0("File '", file, "' does not exist"))
	}

	options <- rmarkdown::yaml_front_matter(file)
	template <- read_body_without_yaml(file)

	epoxy_use_template(
		template,
		.data = .data,
		...,
		options = options
	)
}

read_body_without_yaml <- function(path) {
	x <- readLines(path)
	x_trimmed <- trimws(x)

	if (!any(nzchar(x_trimmed))) {
		rlang::abort(paste0("File '", path, "' is empty"))
	}

	idx_nzchar <- which(nzchar(x_trimmed))[1]
	idx_start <- grep("^---$", x_trimmed)

	if (length(idx_start)) idx_start <- idx_start[1]

	if (length(idx_start) == 0 || idx_nzchar < idx_start) {
		return(paste(x, collapse = "\n"))
	}

	idx_end <- grep("^([-]{3}|[.]{3})$", x_trimmed)
	if (!length(idx_end)) {
		return(paste(x, collapse = "\n"))
	}

	idx_end <- idx_end[idx_end > idx_start][1]

	idx_body <- which(nzchar(x_trimmed))
	idx_body <- idx_body[idx_body > idx_end][1]

	x <- x[-2:-idx_body + 1]
	paste(x, collapse = "\n")
}

epoxy_use_template <- function(
	template,
	.data = NULL,
	...,
	options = list(),
	engine = NULL
) {
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
	opts <- purrr::list_assign(opts, !!!options)
	opts <- purrr::list_assign(opts, !!!opts_current)
	opts <- purrr::list_assign(opts, !!!purrr::compact(opts_fn))

	engine <- engine %||% options$engine %||% "epoxy"

	fn <- switch(
		engine,
		epoxy = epoxy,
		html = ,
		epoxy_html = epoxy_html,
		latex = ,
		epoxy_latex = epoxy_latex,
		mustache = ,
		whisker = epoxy_mustache,
		glue = epoxy,
		glue_html = epoxy_html,
		glue_latex = epoxy_latex,
		{
			rlang::warn(c(
				glue("Unexpected engine '{engine}', defaulting to `epoxy()`."),
				"i" = "Set an epoxy knitr engine in the chunk or file.",
			))
			epoxy
		}
	)

	call <- rlang::call2(
		"eval_epoxy_engine",
		fn = fn,
		code = template,
		options = opts
	)

	with_epoxy_engine(
		engine,
		knitr::asis_output(eval(call))
	)
}
