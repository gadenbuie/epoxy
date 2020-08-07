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

# Overwrites `$` in the context of a glue chunk where `glue_data` was provided.
# The new `$` is vectorized to be equivalent to `purrr::map(ll, "name")`.
# epoxy_data_subset <- function(text, envir) {
#   map_index <- function(x, y) {
#     y <- substitute(y)
#     x <- lapply(x, function(.x) base::`[[`(.x, y))
#     x_len_1 <- vapply(x, function(x) length(x) == 1, logical(1))
#     if (all(x_len_1)) unlist(x) else x
#   }
#   assign("$", envir = envir, map_index)
#   glue::identity_transformer(text, envir)
# }

expoxy_style <- function(...) {
  dots <- list(...)
  are_closures <- vapply(dots, is.function, logical(1))

  function(text, envir) {
    Reduce(
      x = dots
    )
  }
}
