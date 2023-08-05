`%||%` <- function(x, y) {
	if (is.null(x)) y else x
}

str_extract <- function(text, pattern) {
	m <- regexpr(pattern, text)
	x <- regmatches(text, m, invert = NA)
	x <- vapply(x, `[`, 2L, FUN.VALUE = character(1))
	x[is.na(x)] <- ""
	x
}

str_extract_all <- function(text, pattern) {
	m <- gregexpr(pattern, text, perl = TRUE)
	regmatches(text, m)
}

str_count <- function(text, pattern) {
	m <- gregexpr(pattern, text, perl = TRUE)
	x <- regmatches(text, m)
	vapply(x, length, integer(1))
}

is_htmlish_output <- function(exclude = NULL) { # nocov start
	if (isTRUE(rmarkdown::metadata$always_allow_html)) {
		return(TRUE)
	}

	fmt <- knitr::opts_knit$get("rmarkdown.pandoc.to")

	if (is.null(fmt) || !nzchar(fmt)) {
		return(TRUE)
	}

	fmt <- sub("[+-].+$", "", fmt)
	if (grepl("^markdown", fmt)) {
		fmt <- "markdown"
	}

	fmt_htmlish <- c(
		"markdown", "gfm",
		"epub", "epub2", "epub3",
		"html", "html4", "html5",
		"revealjs", "s5", "slideous", "slidy"
	)
	fmt_htmlish <- setdiff(fmt_htmlish, exclude)
	fmt %in% fmt_htmlish
} # nocov end

is_tag <- function(x) inherits(x, c("shiny.tag", "shiny.tag.list", "html"))

with_options <- function(opts, expr) {
	old <- options(opts)
	on.exit(options(old))
	force(expr)
}

escape_html <- function(x) {
	htmltools::htmlEscape(x, attribute = FALSE)
}

list_split_named <- function(l) {
	if (is.null(names(l))) {
		return(list(unnamed = l, named = NULL))
	}

	list(
		unnamed = l[!nzchar(names(l))],
		named = l[nzchar(names(l))]
	)
}

discard_null <- function(x) {
	x[!vapply(x, is.null, logical(1))]
}

as_glue_chr <- function(x) {
	class(x) <- c("glue", class(x))
	x
}
