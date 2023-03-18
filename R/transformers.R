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
#'
#'   In `epoxy_style_collapse()`, the `...` are ignored.
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
#' @param sep,last The separator to use when joining the vector elements when
#'   the expression ends with a `*`. Elements are separated by `sep`, except for
#'   the last two elements, which use `last`.
#' @param language In `epoxy_style_collapse()`, `language` is passed to
#'   [and::and()] or [and::or()] to choose the correct and/or phrase and spacing
#'   for the `language`. By default, will follow the system language. See
#'   [and::and_languages] for supported languages.
#' @export
epoxy_style_collapse <- function(
  sep = ", ",
  last = sep,
  language = NULL,
  ...,
  transformer = glue::identity_transformer
) {
  collapse <- function(regexp = "[*]$", sep = ", ", width = Inf, last = "") {
    function(text, envir) {
      text <- sub(regexp, "", text)
      res <- transformer(text, envir)
      glue_collapse(res, sep = sep, width = width, last = last)
    }
  }

  and_or <- function(and = "and") {
    function(text, envir) {
      conjoin <- if (and == "and") {
        text <- sub("[&]$", "", text)
        and::and
      } else {
        text <- sub("[|]$", "", text)
        and::or
      }
      text <- transformer(text, envir)
      conjoin(text, language = language)
    }
  }

  function(text, envir) {
    text <- trimws(text)
    collapse_fn <-
      switch(
        str_extract(text, "[*&|]$"),
        "*" = collapse("[*]$", sep = sep, last = last),
        "&" = and_or("and"),
        "|" = and_or("or"),
        transformer
      )
    collapse_fn(text, envir)
  }
}
