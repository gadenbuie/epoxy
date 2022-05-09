#' Style replacements with inline formatting
#'
#' @description
#' Makes available all of the `label_` functions from the \pkg{scales} package
#' within an inline format function, `fmt()`. To apply formatting, wrap embraced
#' expressions in `fmt(expr, label)`, where `label` is the name of a labeller
#' function from \pkg{scales} (with or without the leading `label_`), e.g.
#' `"dollar"` or `"label_dollar"` for [scales::label_dollar()]. Short `label`
#' forms exist for some labellers, e.g. `"$"` for `"dollar"`.
#'
#' ```{r child="man/fragments/airbnb-example.Rmd"}
#' ```
#'
#' ### `fmt()` labellers
#'
#' ```{r echo=FALSE}
#' args <- rlang::fn_fmls_names(epoxy_style_format)
#' args <- setdiff(args, "transformer")
#' extras <- labeller_extras()
#' labellers <- data.frame(
#'   applies = sprintf(
#'     "[scales::label_%s()]",
#'     c(unname(extras), args)
#'   ),
#'   label = c(names(extras), args)
#' )
#' labellers <- labellers[order(labellers$applies, labellers$label), ]
#' labellers <- aggregate(
#'   labellers,
#'   list(applies = labellers$applies),
#'   function(label) {
#'     if (grepl("^\\[", label[1])) {
#'       return(label[1])
#'     }
#'     paste0('`"', label, '"`', collapse = ", ")
#'   }
#' )
#' knitr::kable(labellers[, 3:2], row.names = FALSE, col.names = c("`label`", "Applies formatting with"))
#' ```
#'
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
  transformer = glue::identity_transformer
) {
  rlang::check_installed("scales")

  labellers <- labellers_list()
  fmt <- labeller_factory(labellers)

  function(text, envir) {
    fmt_envir <- rlang::new_environment(list(fmt = fmt), parent = envir)
    transformer(text, fmt_envir)
  }
}

labellers_names <- function() {
  labeller_arg_names <- rlang::fn_fmls_names(epoxy_style_format)
  setdiff(labeller_arg_names, "transformer")
}

labellers_list <- function(env = rlang::caller_env(), ...) {
  labeller_names <- labellers_names()
  defaults <- rlang::fn_fmls(epoxy_style_format)[labellers_names()]
  defaults <- purrr::map(defaults, eval)
  dots <- list(...)

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

labeller_extras <- function() {
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
    "si"   = "scientific"
  )
}

labeller_factory <- function(labellers = labellers_list()) {
  force(labellers)

  labeller_extras <- labeller_extras()

  function(text, label, ...) {
    if (length(label) != 1) {
      rlang::abort("`label` must be a single character string")
    }

    label <- sub("^label_", "", label)

    if (label %in% names(labeller_extras)) {
      label <- labeller_extras[label]
    } else {
      label <- tryCatch(
        match.arg(label, labellers_names()),
        error = function(err) {
          rlang::abort(
            glue("Unknown `scale` '{scale}'")
          )
        }
      )
    }

    labellers[[label]](text, ...)
  }
}
