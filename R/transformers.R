#' epoxy Style Transformers
#'
#' These transformers provide additional automatic formatting for the template
#' strings. They are designed to be used with the `.transformer` chunk option of
#' in `epoxy` chunks. You can use `epoxy_style()` to chain several transformers
#' together. `epoxy_style()` and individual \pkg{epoxy} style functions can be
#' used in `epoxy`, `epoxy_html` and `epoxy_latex` chunks and will choose the
#' correct syntax for each.
#'
#' @section Output-specific styling:
#'   The `epoxy_style_` functions will attempt to use the correct syntax for
#'   styling the replacement text for markdown, HTML and LaTeX. This choice is
#'   driven by the chunk engine where the styling function is used. The `epoxy`
#'   engine corresponds to markdown, `epoxy_html` to HTML, and `epoxy_latex` to
#'   LaTeX.
#'
#'   Automatic syntax selection only works when the epoxy style functions are used
#'   with epoxy knitr engines and during the knitr rendering process. When
#'   used outside of this context, you can choose the desired syntax by setting
#'   the `syntax` to one of `"markdown"`, `"html"` or `"latex"`.
#'
#' @examples
#' glue::glue("{letters[1:3]&}", .transformer = epoxy_style("bold", "collapse"))
#' glue::glue("{letters[1:3]&}", .transformer = epoxy_style("collapse", "bold"))
#'
#' # In an epoxy_html chunk...
#' # Note that you don't have to set `syntax = "html"`, it just knows
#' glue::glue(
#'   "{letters[1:3]&}",
#'   .transformer = epoxy_style("bold", "collapse", syntax = "html")
#' )
#'
#' # Or in an epoxy_latex chunk...
#' glue::glue(
#'   "{letters[1:3]&}",
#'   .transformer = epoxy_style("bold", "collapse", syntax = "latex")
#' )
#'
#' # Other Transfomers ----
#'
#' # Apply `format()` to all replacements
# number <- 1.234234234
# glue::glue(
#   "{number}",
#   .transformer = epoxy_style_format(digits = 4)
# )
#
# # Apply _any_ function to all replacements
# glue::glue(
#   "{number}",
#   .transformer = epoxy_style_apply(round, digits = 0)
# )
#
# glue::glue(
#   "{number}",
#   .transformer = epoxy_style(
#     epoxy_style_apply(~ .x * 100),
#     epoxy_style_apply(round, digits = 2),
#     epoxy_style_apply(~ paste0(.x, "%"))
#   )
# )
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
#'
#'   In `epoxy_style_apply()`, the `...` are passed to the underlying call the
#'   underlying function call.
#' @param syntax One of `"markdown"` (or `"md"`), `"html"`, or `"latex"`. The
#'   default is chosen based on the engine of the chunk where the style function
#'   is called, or according to the option `epoxy.engine`. Caution: invalid
#'   options are silently ignored, falling back to markdown syntax.
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
#' @family epoxy-style glue transformers
#' @export
epoxy_style <- function(..., syntax = NULL) {
  parent_env <- rlang::caller_env()
  dots <- rlang::enexprs(...)

  dots <- purrr::modify_if(dots, rlang::is_call, close_over_transformer, parent_env)
  dots <- purrr::modify_if(dots, rlang::is_symbol, rlang::eval_bare, parent_env)
  dots <- purrr::modify_if(dots, is.character, pick_style)

  with_options(
    list(epoxy.engine = syntax),
    purrr::reduce(dots, function(x, y) {
      if (is.null(x)) return(y())
      y(transformer = x)
    }, .init = NULL)
  )
}

pick_style <- function(style) {
  fn_name <- glue("epoxy_style_{style}")
  tryCatch(
    rlang::as_function(fn_name),
    error = function(err) {
      msg <- glue("`epoxy_style_{style}()` doesn't exist.")
      info <- glue("`{style}` doesn't correspond to an {{epoxy}} function.")
      rlang::abort(c(msg, x = info))
    }
  )
}

close_over_transformer <- function(expr, env) {
  rlang::new_function(
    rlang::pairlist2(transformer = glue::identity_transformer),
    rlang::call_modify(expr, transformer = rlang::sym("transformer")),
    env
  )
}

#' @describeIn epoxy_style Wrap variables
#' @param before,after In `epoxy_style_wrap()`, the characters to be added
#'   before and after variables in the template string.
#' @export
epoxy_style_wrap <- function(
  before = "**",
  after = before,
  syntax = NULL,
  transformer = glue::identity_transformer
) {
  if (!is.null(getOption("epoxy.engine", NULL))) {
    force(list(before, after))
  }
  if (!is.null(syntax)) {
    with_options(
      list(epoxy.engine = syntax),
      list(before, after)
    )
  }
  function(text, envir) {
    paste0(before, transformer(text, envir), after)
  }
}

#' @describeIn epoxy_style Embolden variables using `**` in markdown, `<strong>`
#'   in HTML, or `\textbf{}` in LaTeX
#' @export
epoxy_style_bold <- function(syntax = NULL, transformer = glue::identity_transformer) {
  epoxy_style_wrap(
    before = default_for_engine("**", "<strong>", "\\textbf{"),
    after = default_for_engine("**", "</strong>", "}"),
    syntax = syntax,
    transformer = transformer
  )
}

#' @describeIn epoxy_style Italicize variables using `_` in markdown, `<em>` in
#'   HTML, or `\emph{}` in LaTeX
#' @export
epoxy_style_italic <- function(syntax = NULL, transformer = glue::identity_transformer) {
  epoxy_style_wrap(
    before = default_for_engine("_", "<em>", "\\emph{"),
    after = default_for_engine("_", "</em>", "}"),
    syntax = syntax,
    transformer = transformer
  )
}

#' @describeIn epoxy_style Apply a function to all replacement expressions
#' @param .f A function, function name or [purrr::map()]-style inline function.
#' @export
epoxy_style_apply <- function(
  .f = identity,
  ...,
  transformer = glue::identity_transformer
) {
  .f <- purrr::partial(purrr::as_mapper(.f, ...), ...)
  function(text, envir) {
    # text <- eval(parse(text = text, keep.source = FALSE), envir)
    .f(transformer(text, envir))
  }
}

#' @describeIn epoxy_style Code format variables using ` `` ` in markdown,
#'   `<code>` in HTML, or `\texttt{}` in LaTeX
#' @export
epoxy_style_code <- function(syntax = NULL, transformer = glue::identity_transformer) {
  epoxy_style_wrap(
    before = default_for_engine("`", "<code>", "\\texttt{"),
    after = default_for_engine("`", "</code>", "}"),
    syntax = syntax,
    transformer = transformer
  )
}

default_for_engine <- function(md, html, latex) {
  engine <- getOption("epoxy.engine", NULL) %||%
    knitr::opts_current$get("engine")

  if (is.null(engine)) {
    return(md)
  }

  switch(
    engine,
    md = ,
    markdown = ,
    glue = ,
    epoxy = md,
    html = ,
    glue_html = ,
    epoxy_html = html,
    latex = ,
    glue_latex = ,
    epoxy_latex = latex,
    md
  )
}

#' @describeIn epoxy_style Collapse vector variables
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
