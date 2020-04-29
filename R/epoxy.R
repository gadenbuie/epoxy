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

#' @export
epoxy_style_collapse <- function() {
  function(text, envir) {
    collapse_fn <-
      switch(
        str_extract(text, "[*&|]$"),
        "*" = collapse("[*]$", sep = ", ", last = ""),
        "&" = collapse("[&]$", sep = ", ", last = " and "),
        "|" = collapse("[|]$", sep = ", ", last = " or "),
        glue::identity_transformer
      )
    collapse_fn(text, envir)
  }
}

collapse <- function(regexp = "[*]$", sep = ", ", width = Inf, last = "") {
  opts <- knitr::opts_current$get("glue_collapse") %||% list()
  opts$sep <- opts$sep %||% sep
  opts$width <- opts$width %||% width
  opts$last <- opts$last %||% last
  function(text, envir) {
    text <- sub(regexp, "", text)
    res <- glue::identity_transformer(text, envir)
    glue::glue_collapse(res, sep = opts$sep, width = opts$width, last = opts$last)
  }
}
