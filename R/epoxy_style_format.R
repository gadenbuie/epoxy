#' Style replacements with inline formatting
#'
#' @description `r lifecycle::badge('questioning')`
#'
#' _This function was an experimental attempt at inline formatting, but is
#' likely to be removed from \pkg{epoxy} in favor of [epoxy_style_inline()]._
#'
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
#' @example man/examples/epoxy_style_format.R
#'
#' @eval roxy_labellers_params()
#' @param ... Additional formatting functions as named arguments. The name of
#'   the argument in `...` determines the `label` value associated with the
#'   formatter in `fmt()`.
#'
#'   For example, providing `url = utils::URLencode` would allow you to apply
#'   URL-encoding formatting using `fmt(expr, "url")`.
#' @inheritParams epoxy_style
#'
#' @inherit epoxy_style return
#'
#' @family epoxy-style glue transformers
#' @export
epoxy_style_format  <- function(
  bytes       = scales::label_bytes(),
  date        = function(x) format(x, format = "%F"),
  time        = function(x) format(x, format = "%T"),
  datetime    = function(x) format(x, format = "%F %T"),
  dollar      = scales::label_dollar(),
  log         = scales::label_log(),
  number      = scales::label_number(),
  comma       = scales::label_comma(),
  number_auto = scales::label_number_auto(),
  ordinal     = scales::label_ordinal(),
  parse       = scales::label_parse(),
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
    "!DEBUG format {text: `text`}"
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
    "dt"   = "time",
    "dttm" = "datetime",
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

# nocov start
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

  aggregate <- get("aggregate", envir = asNamespace("stats"), inherits = FALSE)

  labellers <- aggregate(
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

roxy_labellers_params <- function() {
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
    "@param {param} The function to apply to when `label` is `{values}`. Default is [{fn}].",
    param = names(args),
    fn = unname(args),
    values = values
  )
}
# nocov end
