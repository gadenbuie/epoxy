#' Epoxy Inline Style Transformer
#'
#' @description This epoxy style is heavily inspired by the inline formatters in
#' the [cli package](https://cli.r-lib.org). The syntax is quite similar, but
#' \pkg{epoxy}'s syntax is slightly different to accommodate reporting use
#' cases.
#'
#' With the inline styles, you can include a keyword, prefixed with a dot (`.`)
#' that is used to format the template variable in place.
#'
#' ```{r}
#' epoxy("It cost {.dollar 123456}.", .style = "inline")
#' ```
#'
#' The formatters, e.g. `dollar` in the example above, can be customized using
#' the arguments of `epoxy_style_inline()`. Pass a customized
#' [scales::label_dollar()] to `dollar` to achieve a different style.
#'
#' ```{r}
#' dollars_nzd <- scales::label_dollar(suffix = " NZD")
#'
#' epoxy(
#'   "It cost {.dollar 123456}.",
#'   .style = epoxy_style_inline(dollar = dollars_nzd)
#' )
#' ```
#'
#' Note that, unlike
#' [inline markup with cli](https://cli.r-lib.org/reference/inline-markup.html),
#' the text within the template variable, other than the keyword, is treated as
#' an R expression.
#'
#' ```{r}
#' money <- 123456
#' epoxy("It cost {.dollar money}.", .style = "inline")
#' ```
#'
#' You can also nest inline markup expressions.
#'
#' ```{r}
#' money <- c(123.456, 234.567)
#' epoxy("It will cost either {.or {.dollar money}}.", .style = "inline")
#' ```
#'
#' Finally, you can provide your own function that is applied to the evaluated
#' expression.
#'
#' ```{r}
#' set.seed(4242)
#'
#' epoxy(
#'   "Here are three random percentages: {.and {.pct {.runif 3}}}.",
#'   .style = epoxy_style_inline(
#'     runif = function(n) sort(runif(n))
#'   )
#' )
#' ```
#'
#' @param ... Additional named inline transformers. The evaluated expression
#'   from the template expression is passed as the first argument to the
#'   function.
#' @inheritParams epoxy
#' @eval roxy_inline_params()
#'
#' @inherit epoxy_style return
#' @family epoxy-style glue transformers
#' @export
epoxy_style_inline <- function(
  ...,
  transformer = glue::identity_transformer,
  and         = and::and,
  or          = and::or,
  inc         = sort,
  dec         = function(x) sort(x, decreasing = TRUE),
  bytes       = scales::label_bytes(),
  date        = function(x) format(x, format = "%F"),
  time        = function(x) format(x, format = "%T"),
  datetime    = function(x) format(x, format = "%F %T"),
  dollar      = scales::label_dollar(prefix = engine_pick("$", "$", "\\$")),
  number      = scales::label_number(),
  comma       = scales::label_comma(),
  ordinal     = scales::label_ordinal(),
  percent     = scales::label_percent(suffix = engine_pick("%", "%", "\\%")),
  pvalue      = scales::label_pvalue(),
  scientific  = scales::label_scientific(),
  uppercase   = toupper,
  lowercase   = tolower,
  titlecase   = tools::toTitleCase
) {
  force(transformer)
  comma_numeric <- comma
  comma <- function(x) {
    if (is.character(x)) return(paste(x, collapse = ", "))
    comma_numeric(x)
  }

  dots <- rlang::dots_list(...)
  if (!all(nzchar(rlang::names2(dots)))) {
    rlang::abort("All functions provided in `...` must be named.")
  }
  dots <- purrr::keep(dots, rlang::is_function)

  self <- function(text, envir) {
    if (detect_wrapped_delims(text)) {
      # we might need to remove one layer of evaluation
      text <- remove_outer_delims(text)
    }
    text <- text_orig <- trimws(text)
    "!DEBUG inline {text: `text`}"

    # https://github.com/r-lib/cli/blob/8d8a211c/R/inline.R#L241
    inline_regex <- "(?s)^[.]([-[:alnum:]_]+)[[:space:]]+(.*)"

    if (!grepl(inline_regex, text, perl = TRUE)) {
      return(transformer(text, envir))
    }

    class <- sub(inline_regex, "\\1", text, perl = TRUE)
    text_sans_class <- sub(inline_regex, "\\2", text, perl = TRUE)

    text <- remove_outer_delims(text_sans_class)

    # if (isTRUE(attr(text, "is_inner_expr"))) {
    #   attr(text, "is_inner_expr") <- NULL
    text <- self(text, envir)
    # }

    maybe_custom_class <- function(text) {
      if (class %in% rlang::names2(dots)) {
        dots[[class]](text)
      } else {
        # if this isn't a known inline class, then we pass the original template
        # text to the next transformer, who might know what to do with it.
        "!DEBUG inline was unmatched"
        tryCatch(
          transformer(text_orig, envir),
          error = function(err_og) {
            tryCatch(
              transformer(text_sans_class, envir),
              error = function(err_sc) {
                rlang::abort(
                  glue("Could not evaluate text '{text_orig}`"),
                  parent = err_og
                )
              }
            )
          }
        )
      }
    }

    switch(
      class,
      and = and(text),
      or = or(text),
      bold = ,
      strong     = epoxy_bold(text),
      italic = ,
      emph       = epoxy_italic(text),
      code       = epoxy_code(text),
      bytes      = bytes(text),
      date       = date(text),
      time       = time(text),
      dttm = ,
      datetime   = datetime(text),
      dollar     = dollar(text),
      num = ,
      number     = number(text),
      comma      = comma(text),
      ordinal    = ordinal(text),
      pct = ,
      percent    = percent(text),
      pvalue     = pvalue(text),
      scientific = scientific(text),
      uc = ,
      uppercase  = uppercase(text),
      lc = ,
      lowercase  = lowercase(text),
      tc = ,
      titlecase  = titlecase(text),
      inc = inc(text),
      dec = dec(text),
      maybe_custom_class(text)
    )
  }

  self
}

detect_wrapped_delims <- function(text) {
  delims <- getOption("epoxy:::private", list())
  open <- delims$.open %||% "{"
  close <- delims$.close %||% "}"
  open <- gsub("([}{])", "\\\\\\1", open)
  close <- gsub("([}{])", "\\\\\\1", close)

  text <- trimws(text)

  if (!grepl(paste0("^", open), text)) return(FALSE)
  if (!grepl(paste0(close, "$"), text)) return(FALSE)
  TRUE
}

remove_outer_delims <- function(text) {
  delims <- getOption("epoxy:::private", list())
  open <- delims$.open %||% "{"
  close <- delims$.close %||% "}"

  if (!grepl(open, text, fixed = TRUE)) {
    return(text)
  }

  text <- strsplit(text, open, fixed = TRUE)[[1]][-1]
  text <- paste(text, collapse = open)
  text <- strsplit(text, close, fixed = TRUE)[[1]]
  # collapse removes the final closer for us
  text <- paste(text, collapse = close)
  attr(text, "is_inner_expr") <- TRUE
  text
}

epoxy_bold <- function(text) {
  before <- engine_pick("**", "<strong>", "\\textbf{")
  after <- engine_pick("**", "</strong>", "}")
  paste0(before, text, after)
}

epoxy_italic <- function(text) {
  before <- engine_pick("_", "<em>", "\\emph{")
  after <- engine_pick("_", "</em>", "}")
  paste0(before, text, after)
}

epoxy_code <- function(text) {
  before <- engine_pick("`", "<code>", "\\texttt{")
  after <- engine_pick("`", "</code>", "}")
  paste0(before, text, after)
}

# nocov start
roxy_inline_params <- function() {
  args <- rlang::fn_fmls(epoxy_style_inline)
  args <- args[setdiff(names(args), c("...", "transformer"))]
  args <- purrr::map(args, rlang::expr_text)
  args <- purrr::map_chr(args, function(expr) {
    if (grepl("^function", expr)) {
      return(paste0("`", expr, "`"))
    }
    expr <- sub("\\(.+\\)$", "()", expr)
    if (grepl("\\(\\)$", expr)) {
      return(expr)
    }
    paste0("[", expr, "()]")
  })

  extras <- c(
    bold   = "strong",
    italic = "emph",
    dttm   = "datetime",
    num    = "number",
    pct    = "percent",
    uc     = "uppercase",
    lo     = "lowercase",
    tc     = "titlecase"
  )

  values <- purrr::map_chr(names(args), function(label) {
    label <- c(label, names(extras[extras == label]))
    knitr::combine_words(label, before = '`{.', after = " x}`", and = " or ")
  })

  glue(
    "@param {param} The function to apply to `x` when the template is {values}. ",
    "Default is {fn}.",
    param = names(args),
    fn = unname(args),
    values = values
  )
}
# nocov end
