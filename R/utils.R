`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

in_knitr <- function() {
  !is.null(knitr::current_input())
}

abort <- function(..., .envir = parent.frame()) {
  stop(glue(..., .envir = .envir), call. = FALSE)
}

str_extract <- function(text, pattern) {
  m <- regexpr(pattern, text)
  x <- regmatches(text, m, invert = NA)
  x <- vapply(x, `[`, 2L, FUN.VALUE = character(1))
  x[is.na(x)] <- ""
  x
}

is_htmlish_output <- function(exclude = NULL) {
  fmt <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  fmt_htmlish <- c("markdown", "epub", "html", "html4", "html5", "revealjs",
                   "s5", "slideous", "slidy", "gfm")
  fmt_htmlish <- setdiff(fmt_htmlish, exclude)
  fmt %in% fmt_htmlish
}

collapse_space <- function(...) {
  paste(..., collapse = " ")
}

is_tag <- function(x) inherits(x, c("shiny.tag", "shiny.tag.list"))

file_pkg <- function(...) system.file(..., package = "epoxy", mustWork = TRUE)

pkg_version <- function() {
  x <- read.dcf(file_pkg("DESCRIPTION"))
  if (!"Version" %in% colnames(x)) return(NA)
  unname(x[, "Version"])
}
