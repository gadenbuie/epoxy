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
  if (isTRUE(rmarkdown::metadata$always_allow_html)) return(TRUE)

  fmt <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  fmt <- sub("[+-].+$", "", fmt)
  fmt_htmlish <- c(
    "markdown", "epub", "html", "html4", "html5", "revealjs",
    "s5", "slideous", "slidy", "gfm"
  )
  fmt_htmlish <- setdiff(fmt_htmlish, exclude)
  fmt %in% fmt_htmlish
} # nocov end

collapse_space <- function(...) {
  paste(..., collapse = " ")
}

is_tag <- function(x) inherits(x, "shiny.tag")

with_options <- function(opts, expr) {
  old <- options(opts)
  on.exit(options(old))
  force(expr)
}
