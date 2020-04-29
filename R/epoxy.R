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
epoxy_style_wrap <- function(before = "**", after = "**") {
  function(text, envir) {
    paste0(before, glue::identity_transformer(text, envir), after)
  }
}

#' @export
epoxy_style_bold <- function() {
  epoxy_style_wrap("**", "**")
}

#' @export
epoxy_style_italic <- function() {
  epoxy_style_wrap("_", "_")
}

#' @export
epoxy_style_code <- function() {
  epoxy_style_wrap("`", "`")
}
