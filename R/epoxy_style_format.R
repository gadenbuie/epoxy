#' Style replacements with inline formatting
#'
#' @description
#' Makes available all of the `label_` functions from the \pkg{scales} package
#' within an inline format function, `fmt()`. To apply formatting, wrap embraced
#' expressions in `fmt(expr, label)`, where `label` is the name of a labeller
#' function from \pkg{scales} (with or without the leading `label_`), e.g.
#' `"dollar"` or `"label_dollar"` for [scales::label_dollar()]. Short `label`
#' forms exist for some labellers, e.g. `"$"` for `"dollar"`. See the full
#' listing in the table below.
#'
#' ```{r child="man/fragments/airbnb-example.Rmd"}
#' ```
#'
#' ### `fmt()` labels for formatters
#'
#' ```{r echo=FALSE}
#' labellers <- labellers_summarize()
#' knitr::kable(labellers[, 2:1], row.names = FALSE, col.names = c("`label`", "Applies formatting with"))
#' ```
#'
#' @examples
#' revenue <- 0.2123
#' sales <- 42000.134
#' glue::glue(
#'   '{fmt(revenue, "%")} of revenue generates {fmt(sales, "$")} in profits.',
#'   .transformer = epoxy_style_format()
#' )
#'
#' # To set labeller options, provide the label calls
#' glue::glue(
#'   '{fmt(revenue, "%")} of revenue generates {fmt(sales, "$")} in profits.',
#'   .transformer = epoxy_style_format(
#'     percent = scales::label_percent(accuracy = 0.1),
#'     dollar = scales::label_dollar(accuracy = 10)
#'   )
#' )
#'
#' # Add your own formatting functions
#' search <- "why are cats scared of cucumbers"
#' glue::glue(
#'   '<https://example.com?q={fmt(search, "url")}>',
#'   .transformer = epoxy_style_format(
#'     url = utils::URLencode
#'   )
#' )
#'
#' @param bytes The function to apply to when `label` is `"bytes"`. Default is
#'   [scales::label_bytes()].
#' @param date The function to apply to when `label` is `"date" or "d"`. Default
#'   is [scales::label_date()].
#' @param date_short The function to apply to when `label` is `"date_short" or
#'   "ds"`. Default is [scales::label_date_short()].
#' @param time The function to apply to when `label` is `"time" or "dt"`.
#'   Default is [scales::label_time()].
#' @param dollar The function to apply to when `label` is `"dollar" or "$"`.
#'   Default is [scales::label_dollar()].
#' @param log The function to apply to when `label` is `"log"`. Default is
#'   [scales::label_log()].
#' @param number The function to apply to when `label` is `"number" or "#"`.
#'   Default is [scales::label_number()].
#' @param comma The function to apply to when `label` is `"comma" or ","`.
#'   Default is [scales::label_comma()].
#' @param number_auto The function to apply to when `label` is `"number_auto",
#'   "a", or "auto"`. Default is [scales::label_number_auto()].
#' @param ordinal The function to apply to when `label` is `"ordinal" or "o"`.
#'   Default is [scales::label_ordinal()].
#' @param parse The function to apply to when `label` is `"parse"`. Default is
#'   [scales::label_parse()].
#' @param math The function to apply to when `label` is `"math"`. Default is
#'   [scales::label_math()].
#' @param percent The function to apply to when `label` is `"percent", "pct", or
#'   "%"`. Default is [scales::label_percent()].
#' @param pvalue The function to apply to when `label` is `"pvalue" or "p"`.
#'   Default is [scales::label_pvalue()].
#' @param scientific The function to apply to when `label` is `"scientific" or
#'   "si"`. Default is [scales::label_scientific()].
#' @param wrap The function to apply to when `label` is `"wrap"`. Default is
#'   [scales::label_wrap()].
#' @param uppercase The function to apply to when `label` is `"uppercase" or
#'   "uc"`. Default is [toupper()].
#' @param lowercase The function to apply to when `label` is `"lowercase" or
#'   "lc"`. Default is [tolower()].
#' @param titlecase The function to apply to when `label` is `"titlecase" or
#'   "tc"`. Default is [tools::toTitleCase()].
#' @param ... Additional formatting functions as named arguments. The name of
#'   the argument in `...` determines the `label` value associated with the
#'   formatter in `fmt()`.
#'
#'   For example, providing `url = utils::URLencode` would allow you to apply
#'   URL-encoding formatting using `fmt(expr, "url")`.
#' @inheritParams epoxy_style
#'
#' @family epoxy-style glue transformers
#' @export
epoxy_style_format  <- function(
  bytes       = scales::label_bytes(),
  date        = scales::label_date(),
  date_short  = scales::label_date_short(),
  time        = scales::label_time(),
  dollar      = scales::label_dollar(),
  log         = scales::label_log(),
  number      = scales::label_number(),
  comma       = scales::label_comma(),
  number_auto = scales::label_number_auto(),
  ordinal     = scales::label_ordinal(),
  parse       = scales::label_parse(),
  math        = scales::label_math(),
  percent     = scales::label_percent(),
  pvalue      = scales::label_pvalue(),
  scientific  = scales::label_scientific(),
  wrap        = scales::label_wrap(width = 80),
  uppercase   = toupper,
  lowercase   = tolower,
  titlecase   = tools::toTitleCase,
  ...,
  transformer = glue::identity_transformer
) {
  rlang::check_installed("scales")

  labellers <- labellers_list(...)
  fmt <- labeller_factory(labellers)

  function(text, envir) {
    fmt_envir <- rlang::new_environment(list(fmt = fmt), parent = envir)
    transformer(text, fmt_envir)
  }
}

labellers_names <- function() {
  labeller_arg_names <- rlang::fn_fmls_names(epoxy_style_format)
  setdiff(labeller_arg_names, c("...", "transformer"))
}

labellers_list <- function(env = rlang::caller_env(), ...) {
  dots <- list(...)
  labeller_names <- c(labellers_names(), names(dots))
  defaults <- rlang::fn_fmls(epoxy_style_format)[labellers_names()]
  defaults <- purrr::map(defaults, eval)

  purrr::map(
    purrr::set_names(labeller_names),
    function(name) {
      if (rlang::has_name(dots, name)) {
        dots[[name]]
      } else if (rlang::env_has(env, name)) {
        rlang::env_get(env, name)
      } else {
        defaults[[name]]
      }
    }
  )
}

labellers_extras <- function() {
  c(
    "$"    = "dollar",
    "#"    = "number",
    ","    = "comma",
    "d"    = "date",
    "ds"   = "date_short",
    "dt"   = "time",
    "a"    = "number_auto",
    "auto" = "number_auto",
    "o"    = "ordinal",
    "pct"  = "percent",
    "%"    = "percent",
    "p"    = "pvalue",
    "si"   = "scientific",
    "uc"   = "uppercase",
    "lc"   = "lowercase",
    "tc"   = "titlecase"
  )
}

labeller_factory <- function(labellers = labellers_list()) {
  force(labellers)

  labellers_extras <- labellers_extras()
  default_names <- labellers_names()

  match_label <- function(label) {
    call <- rlang::caller_call()
    label <- sub("^label_", "", label)

    if (label %in% names(labellers_extras)) {
      labellers_extras[label]
    } else if (rlang::has_name(labellers, label)) {
      label
    } else {
      tryCatch(
        match.arg(label, labellers_names()),
        error = function(err) {
          rlang::abort(
            glue("Unknown format `label` '{label}'"),
            call = call
          )
        }
      )
    }
  }

  function(text, label, ...) {
    if (length(label) != 1) {
      rlang::abort("`label` must be a single character string")
    }

    label <- match_label(label)

    labellers[[label]](text, ...)
  }
}

labellers_summarize <- function() {
  tools::toTitleCase("for rcmdcheck")

  args <- rlang::fn_fmls(epoxy_style_format)[labellers_names()]
  args <- purrr::modify_if(args, rlang::is_symbol, rlang::expr_text)
  args <- purrr::modify_if(args, rlang::is_call_simple, rlang::call_name)
  args <- purrr::modify_if(args, purrr::negate(rlang::is_string), rlang::expr_text)

  extras <- labellers_extras()
  extras_fns <- args[unname(extras)]
  labels <- c(names(extras), names(args))

  fns <- unlist(c(unname(extras_fns), unname(args)))
  fns[grepl("^label_", fns)] <- paste0("scales::", fns[grepl("^label_", fns)])
  fns <- sprintf("[%s()]", fns)

  labellers <- data.frame(
    applies = fns,
    label = labels
  )

  labellers <- stats::aggregate(
    labellers[-1],
    list(applies = labellers$applies),
    function(label) {
      if (grepl("^\\[", label[1])) {
        return(label[1])
      }
      paste0('`"', label, '"`', collapse = ", ")
    }
  )

  rownames(labellers) <- labellers$applies
  labellers[fns[-seq_along(extras)], ]
}

labellers_params <- function() {
  args <- rlang::fn_fmls(epoxy_style_format)[labellers_names()]
  args <- purrr::map(args, rlang::expr_text)
  args <- purrr::map_chr(args, function(expr) {
    expr <- sub("\\(.+\\)$", "()", expr)
    if (grepl("\\(\\)$", expr)) {
      return(expr)
    }
    paste0(expr, "()")
  })

  extras <- labellers_extras()

  values <- purrr::map_chr(names(args), function(label) {
    label <- c(label, names(extras[extras == label]))
    knitr::combine_words(label, before = '"', and = " or ")
  })

  glue(
    "#' @param {param} The function to apply to when `label` is `{values}`. Default is [{fn}].",
    param = names(args),
    fn = unname(args),
    values = values
  )
}
