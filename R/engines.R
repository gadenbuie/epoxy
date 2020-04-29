#' @export
knitr_engine_glue <- function(options) {
  out <- if (isTRUE(options$eval)) {
    code <- paste(options$code, collapse = "\n")
    glue::glue_data(
      .x = options[["glue_data"]],
      code,
      .envir = parent.frame(2),
      .open = options[[".open"]] %||% "{",
      .close = options[[".close"]] %||% "}",
      .na = options[[".na"]] %||% "",
      .trim = options[[".trim"]] %||% FALSE,
      .transformer = options[[".transformer"]] %||% glue::identity_transformer
    )
  }
  options$results <- "asis"
  options$echo <- options[[".echo"]] %||% FALSE
  knitr::engine_output(options, options$code, out)
}
