#' epoxy Style Transformers
#'
#' These transformers provide additional automatic formatting for the template
#' strings. They are designed to be used with the `.transformer` chunk option of
#' in `epoxy` chunks. You can use `epoxy_style()` to chain several transformers
#' together.
#'
#' @examples
#' glue::glue("{letters[1:3]&}", .transformer = epoxy_style("bold", "collapse"))
#' glue::glue("{letters[1:3]&}", .transformer = epoxy_style("collapse", "bold"))
#'
#' @param ... A list of style functions, e.g. `epoxy_style_bold` or the name of
#'   a style function, e.g. `"bold"`, or a call to a style function, e.g.
#'   `epoxy_style_bold()`. `epoxy_style()` chains the style functions together,
#'   applying the styles from left to right.
#'
#'   For example, `epoxy_style("bold", "collapse")` results in replaced strings
#'   that are emboldened _and then_ collapsed, e.g. `**a** and **b**`. On the
#'   other hand, `epoxy_style("collapse", "bold")`  will collapse the vector
#'   _and then_ embolden the entire string.
#' @param transformer The transformer to apply to the replacement string. This
#'   argument is used for chaining the transformer functions. By providing a
#'   function to this argument you can apply an additional transformation after
#'   the current transformation. In nearly all cases, you can let
#'   `epoxy_style()` handle this for you. The chain ends when
#'   [glue::identity_transformer()] is used as the `transformer`.
#'
#' @return A function of `text` and `envir` suitable for the `.transformer`
#'   argument of [glue::glue()].
#'
#' @export
epoxy_style <- function(...) {
  parent_env <- rlang::caller_env()
  dots <- rlang::enexprs(...)

  dots <- purrr::modify_if(dots, rlang::is_call, close_over_transformer, parent_env)
  dots <- purrr::modify_if(dots, rlang::is_symbol, rlang::eval_bare, parent_env)
  dots <- purrr::modify_if(dots, is.character, pick_style)

  purrr::reduce(dots, function(x, y) {
    if (is.null(x)) return(y())
    y(transformer = x)
  }, .init = NULL)
}

pick_style <- function(style) {
  fn_name <- glue("epoxy_style_{style}")
  rlang::as_function(fn_name)
}

close_over_transformer <- function(expr, env) {
  rlang::new_function(
    rlang::pairlist2(transformer = ),
    rlang::call_modify(expr, transformer = rlang::sym("transformer")),
    env
  )
}

#' @describeIn epoxy_style Wrap variables
#' @param before,after In `epoxy_style_wrap()`, the characters to be added
#'   before and after variables in the template string.
#' @export
epoxy_style_wrap <- function(before = "**", after = "**", transformer = glue::identity_transformer) {
  function(text, envir) {
    paste0(before, transformer(text, envir), after)
  }
}

#' @describeIn epoxy_style Embolden variables using markdown `**` syntax
#' @export
epoxy_style_bold <- function(transformer = glue::identity_transformer) {
  epoxy_style_wrap("**", "**", transformer = transformer)
}

#' @describeIn epoxy_style Italicize variables using markdown `_` syntax
#' @export
epoxy_style_italic <- function(transformer = glue::identity_transformer) {
  epoxy_style_wrap("_", "_", transformer = transformer)
}

#' @describeIn epoxy_style Code format variables using markdown backtick syntax
#' @export
epoxy_style_code <- function(transformer = glue::identity_transformer) {
  epoxy_style_wrap("`", "`", transformer = transformer)
}

#' @describeIn epoxy_style Collapse vector variables.
#' @param sep,sep_and,sep_or The separator to use when joining the vector
#'   elements when the variable ends in `*`, `&`, or `|` respectively. By
#'   default, these are all `", "`.
#' @param last,last_and,last_or Additional text added after `sep` before the
#'   last element when the variable ends in `*`, `&`, or `|` respectively.
#' @export
epoxy_style_collapse <- function(
  sep = ", ",
  last = "",
  last_and = " and ",
  last_or = " or ",
  sep_and = sep,
  sep_or = sep,
  transformer = glue::identity_transformer
) {
  collapse <- function(regexp = "[*]$", sep = ", ", width = Inf, last = "") {
    function(text, envir) {
      text <- sub(regexp, "", text)
      res <- transformer(text, envir)
      glue_collapse(res, sep = sep, width = width, last = last)
    }
  }

  function(text, envir) {
    collapse_fn <-
      switch(
        str_extract(text, "[*&|]$"),
        "*" = collapse("[*]$", sep = sep,     last = last),
        "&" = collapse("[&]$", sep = sep_and, last = last_and),
        "|" = collapse("[|]$", sep = sep_or,  last = last_or),
        transformer
      )
    collapse_fn(text, envir)
  }
}
