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
epoxy_style_collapse <- function(
  sep = ", ",
  last = "",
  last_and = " and ",
  last_or = " or ",
  sep_and = sep,
  sep_or = sep
) {
  function(text, envir) {
    collapse_fn <-
      switch(
        str_extract(text, "[*&|]$"),
        "*" = collapse("[*]$", sep = sep,     last = last),
        "&" = collapse("[&]$", sep = sep_and, last = last_and),
        "|" = collapse("[|]$", sep = sep_or,  last = last_or),
        glue::identity_transformer
      )
    collapse_fn(text, envir)
  }
}

collapse <- function(regexp = "[*]$", sep = ", ", width = Inf, last = "") {
  function(text, envir) {
    text <- sub(regexp, "", text)
    res <- glue::identity_transformer(text, envir)
    glue_collapse(res, sep = sep, width = width, last = last)
  }
}
