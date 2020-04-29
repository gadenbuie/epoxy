epoxy_set_knitr_engines <- function() {
  knitr::knit_engines$set(
    glue = knitr_engine_glue,
    "glue_html" = knitr_engine_glue_html,
    "glue_latex" = knitr_engine_glue_latex
  )
}

#' @export
knitr_engine_glue <- function(options) {
  out <- if (isTRUE(options$eval)) {
    code <- paste(options$code, collapse = "\n")
    glue_data(
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
    browser()
    code <- paste(options$code, collapse = "\n")
    code <- glue_data(
      .x = options[["glue_data"]],
      code,
      .envir = parent.frame(2),
      .open = options[[".open"]] %||% "{",
      .close = options[[".close"]] %||% "}",
      .na = options[[".na"]] %||% "",
      .trim = options[[".trim"]] %||% FALSE,
      .transformer = options[[".transformer"]] %||% glue::identity_transformer
    )
    code <- glue_collapse(code, sep = "\n")
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

#' @export
knitr_engine_glue_latex <- function(options) {
  out <- if (isTRUE(options$eval) && knitr::is_latex_output()) {
    code <- paste(options$code, collapse = "\n")
    code <- glue_data(
      .x = options[["glue_data"]],
      code,
      .envir = parent.frame(2),
      .open = options[[".open"]] %||% "<",
      .close = options[[".close"]] %||% ">",
      .na = options[[".na"]] %||% "",
      .trim = options[[".trim"]] %||% FALSE,
      .transformer = options[[".transformer"]] %||% glue::identity_transformer
    )
    paste0('\n```{=latex}\n', glue_collapse(code, sep = "\n"), "\n```")
  }
  options$results <- "asis"
  options$echo <- options[[".echo"]] %||% FALSE
  knitr::engine_output(options, options$code, out)
}
