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

#' @export
knitr_engine_glue_html <- function(options) {
  out <- if (isTRUE(options$eval) && is_htmlish_output()) {
    code <- paste(options$code, collapse = "\n")
    code <- glue::glue_data(
      .x = options[["glue_data"]],
      code,
      .envir = parent.frame(2),
      .open = options[[".open"]] %||% "{",
      .close = options[[".close"]] %||% "}",
      .na = options[[".na"]] %||% "",
      .trim = options[[".trim"]] %||% FALSE,
      .transformer = options[[".transformer"]] %||% glue::identity_transformer
    )
    if (isTRUE(options$html_raw %||% TRUE)) {
      # use pandoc's raw html block by default, but this isn't always available
      # so it can be disabled with the html_raw chunk option.
      code <- paste0('\n```{=html}\n', code, "\n```")
    }
    code
  }
  options$results <- "asis"
  options$echo <- options[[".echo"]] %||% FALSE
  knitr::engine_output(options, options$code, out)
}
