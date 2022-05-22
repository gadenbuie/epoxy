#' Use the epoxy knitr engines
#'
#' @description
#' Sets \pkg{epoxy}'s \pkg{knitr} engines for use by \pkg{knitr} in R Markdown
#' and other document formats powered by \pkg{knitr}. These engines are also
#' set up when loading \pkg{epoxy} with `library()`, so in general you will not
#' need to call this function explicitly.
#'
#' \pkg{epoxy} provides four \pkg{knitr} engines:
#'
#' * `epoxy` uses default \pkg{glue} syntax, e.g. `{var}` for markdown outputs
#' * `epoxy_html` uses double brace syntax, e.g. `{{var}}` for HTML outputs
#' * `epoxy_latex` uses double angle brackets syntax, e.g. `<<var>>` for LaTeX
#'   outputs
#' * `whisker` uses the \pkg{whisker} package which provides an R-based
#'   implementation of the [mustache](https://mustache.github.io/) templating
#'   language.
#'
#' For historical reasons, alias for the HTML and LaTeX engines are aliased are
#' also created: `glue_html` and `glue_latex`. You may opt into a third alias —
#' `glue` for the `epoxy` engine — by calling `use_epoxy_glue_engine()`, but
#' note that this will most likely overwrite the `glue` engine provided by the
#' \pkg{glue} package.
#'
#' @param use_glue_engine If `TRUE` (default `FALSE`), uses \pkg{epoxy}'s `glue`
#'   engine, most likely overwriting the `glue` engine provided by \pkg{glue}.
#'
#' @return Silently sets \pkg{epoxy}'s knitr engines and invisible returns
#'   [knitr::knit_engines] as they were prior to the function call.
#'
#' @export
use_epoxy_knitr_engines <- function(use_glue_engine = FALSE) {
  old <- knitr::knit_engines$get()

  knitr::knit_engines$set(
    epoxy         = knitr_engine_epoxy,
    "epoxy_html"  = knitr_engine_epoxy_html,
    "glue_html"   = knitr_engine_epoxy_html,
    "epoxy_latex" = knitr_engine_epoxy_latex,
    "glue_latex"  = knitr_engine_epoxy_latex,
    "whisker"     = knitr_engine_whisker
  )

  if (isTRUE(use_glue_engine)) {
    use_epoxy_glue_engine()
  }

  invisible(old)
}

#' @describeIn use_epoxy_knitr_engines Use \pkg{epoxy}'s `epoxy` engine as
#'   the `glue` engine.
#' @export
use_epoxy_glue_engine <- function() {
  old <- knitr::knit_engines$get()
  knitr::knit_engines$set(glue = knitr_engine_epoxy)
  invisible(old)
}

#' Epoxy string interpolation
#'
#' The functions power the knitr chunk engines and are wrappers around
#' [glue::glue()], with a few extra conveniences provided by \pkg{epoxy}.
#'
#' @param .data A data set
#' @param .style For [epoxy_style()]
#' @inheritParams glue::glue
#' @export
epoxy <- function(
  ...,
  .data = NULL,
  .style = NULL,
  .sep = "",
  .envir = parent.frame(),
  .open = "{",
  .close = "}",
  .na = "",
  .null = "",
  .comment = "#",
  .literal = FALSE,
  .trim = FALSE,
  .transformer = NULL
) {

  glue_env <- .envir
  if (!is.null(.data)) {
    glue_env <- new.env(parent = .envir)
    assign("$", epoxy_data_subset, envir = glue_env)
  }

  opts_transformer <- list(
    epoxy_style = .style,
    .transformer = .transformer
  )

  glue_data(
    .x = .data,
    ...,
    .sep     = .sep,
    .envir   = glue_env,
    .open    = .open,
    .close   = .close,
    .na      = .na,
    .null    = .null,
    .comment = .comment,
    .literal = .literal,
    .trim    = .trim,
    .transformer = epoxy_options_get_transformer(opts_transformer)
  )
}

knitr_engine_epoxy <- function(options) {
  deprecate_glue_engine_prefix(options)

  out <- if (isTRUE(options$eval)) {
    options <- deprecate_glue_data_chunk_option(options)
    code <- paste(options$code, collapse = "\n")

    epoxy(
      code,
      .data        = options[["data"]],
      .style       = options[["epoxy_style"]],
      .sep         = "",
      .envir       = options[[".envir"]]   %||% knitr::knit_global(),
      .open        = options[[".open"]]    %||% "{",
      .close       = options[[".close"]]   %||% "}",
      .na          = options[[".na"]]      %||% "",
      .null        = options[[".null"]]    %||% "",
      .trim        = options[[".trim"]]    %||% FALSE,
      .comment     = options[[".comment"]] %||% "#",
      .literal     = options[[".literal"]] %||% FALSE,
      .transformer = options[[".transformer"]]
    )
  }

  options$results <- "asis"
  options$echo <- options[[".echo"]] %||% FALSE
  knitr::engine_output(options, options$code, out)
}

#' @describeIn epoxy epoxy for HTML
#' @export
epoxy_html <- function(
  ...,
  .data = NULL,
  .style = NULL,
  .sep = "",
  .envir = parent.frame(),
  .open = "{{",
  .close = "}}",
  .na = "",
  .null = "",
  .comment = "#",
  .literal = FALSE,
  .trim = FALSE,
  .transformer = NULL
) {
  epoxy(
    ...,
    .data = .data,
    .style = .style,
    .sep = .sep,
    .envir = .envir,
    .open = .open,
    .close = .close,
    .na = .na,
    .null = .null,
    .comment = .comment,
    .literal = .literal,
    .trim = .trim,
    .transformer = .transformer
  )
}

knitr_engine_epoxy_html <- function(options) {
  deprecate_glue_engine_prefix(options)

  out <- NULL
  if (isTRUE(options$eval) && is_htmlish_output()) {
    options <- deprecate_glue_data_chunk_option(options)
    code <- paste(options$code, collapse = "\n")

    out <- epoxy(
      code,
      .data        = options[["data"]],
      .style       = options[["epoxy_style"]],
      .sep         = "",
      .envir       = options[[".envir"]]   %||% knitr::knit_global(),
      .open        = options[[".open"]]    %||% "{{",
      .close       = options[[".close"]]   %||% "}}",
      .na          = options[[".na"]]      %||% "",
      .null        = options[[".null"]]    %||% "",
      .trim        = options[[".trim"]]    %||% FALSE,
      .comment     = options[[".comment"]] %||% "#",
      .literal     = options[[".literal"]] %||% FALSE,
      .transformer = options[[".transformer"]]
    )

    out <- glue_collapse(out, sep = "\n")

    if (isTRUE(options$html_raw %||% TRUE)) {
      # use pandoc's raw html block by default, but this isn't always available
      # so it can be disabled with the html_raw chunk option.
      out <- paste0('\n```{=html}\n', out, "\n```")
    }

    out
  }
  options$results <- "asis"
  options$echo <- options[[".echo"]] %||% FALSE
  knitr::engine_output(options, options$code, out)
}

#' @describeIn epoxy epoxy for LaTeX
#' @export
epoxy_latex <- function(
  ...,
  .data = NULL,
  .style = NULL,
  .sep = "",
  .envir = parent.frame(),
  .open = "<",
  .close = ">",
  .na = "",
  .null = "",
  .comment = "#",
  .literal = FALSE,
  .trim = FALSE,
  .transformer = NULL
) {
  epoxy(
    ...,
    .data = .data,
    .style = .style,
    .sep = .sep,
    .envir = .envir,
    .open = .open,
    .close = .close,
    .na = .na,
    .null = .null,
    .comment = .comment,
    .literal = .literal,
    .trim = .trim,
    .transformer = .transformer
  )
}

knitr_engine_epoxy_latex <- function(options) {
  deprecate_glue_engine_prefix(options)

  out <- NULL
  if (isTRUE(options$eval)) {
    options <- deprecate_glue_data_chunk_option(options)
    code <- paste(options$code, collapse = "\n")

    out <- epoxy(
      code,
      .data        = options[["data"]],
      .style       = options[["epoxy_style"]],
      .sep         = "",
      .envir       = options[[".envir"]]   %||% knitr::knit_global(),
      .open        = options[[".open"]]    %||% "<",
      .close       = options[[".close"]]   %||% ">",
      .na          = options[[".na"]]      %||% "",
      .null        = options[[".null"]]    %||% "",
      .trim        = options[[".trim"]]    %||% FALSE,
      .comment     = options[[".comment"]] %||% "#",
      .literal     = options[[".literal"]] %||% FALSE,
      .transformer = options[[".transformer"]]
    )
  }
  options$results <- "asis"
  options$echo <- options[[".echo"]] %||% FALSE
  knitr::engine_output(options, options$code, out)
}

knitr_engine_whisker <- function(options) {
  out <- if (isTRUE(options$eval)) {
    options <- deprecate_glue_data_chunk_option(options)
    code <- paste(options$code, collapse = "\n")
    code <- if (!is.null(options[["data"]])) {
      if (isTRUE(options[["data_asis"]])) {
        whisker::whisker.render(code, data = options[["data"]])
      } else {
        vapply(
          prep_whisker_data(options[["data"]]),
          function(d) {
            whisker::whisker.render(code, data = d)
          },
          character(1)
        )
      }
    } else {
      whisker::whisker.render(code, options[[".envir"]] %||% knitr::knit_global())
    }
    code <- glue_collapse(code, sep = "\n")
    if (isTRUE(options$html_raw %||% FALSE)) {
      # use pandoc's raw html block by default, but this isn't always available
      # so it can be disabled with the html_raw chunk option.
      code <- paste0('\n```{=html}\n', code, "\n```")
    }
    code
  }
  options$results <- "asis"
  options$echo <- options[[".echo"]] %||% FALSE
  knitr::engine_output(options, options$code, out)
}

prep_whisker_data <- function(x) {
  if (!is.list(x) && !inherits(x, "list")) {
    stop("data must be a list or a list-alike", call. = FALSE)
  }
  if (is.null(names(x)) || !all(nzchar(names(x)))) {
    stop("data must be a named list or list-alike", call. = FALSE)
  }
  x_len <- vapply(x, length, integer(1))
  x_null <- vapply(x, is.null, logical(1))
  if (length(unique(x_len[!x_null])) != 1 && !all(x_len[!x_null] > 0)) {
    stop("data must be the same length: ", paste(x_len[!x_null], collapse = ", "), call. = FALSE)
  }

  # turn list(a = 1:2, b = 3:4)
  # into list(list(a = 1, b = 3), list(a = 2, b = 4))
  lapply(seq_len(max(x_len)), function(i) lapply(x, function(y) y[[i]]))
}

epoxy_data_subset <- function(x, y) {
  y <- substitute(y)
  x <- lapply(x, function(.x) base::`[[`(.x, y))
  x_len_1 <- vapply(x, function(x) length(x) == 1, logical(1))
  if (all(x_len_1)) unlist(x) else x
}

epoxy_options_get_transformer <- function(options) {
  style <- options[["epoxy_style"]]
  if (is.vector(style) || is.list(style)) {
    return(epoxy_style(!!!style))
  }
  style %||% options[[".transformer"]] %||% epoxy_style("collapse", "format")
}

deprecate_glue_data_chunk_option <- function(options) {
  # FIXME: remove eventually!
  if ("glue_data" %in% names(options)) {
    if (!"data" %in% names(options)) {
      options$data <- options$glue_data
    }
    warning(
      "The `glue_data` chunk option has been deprecated. ",
      "Please use the `data` chunk option instead.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  options
}

deprecate_glue_engine_prefix <- local({
  has_warned <- list()
  function(options) {
    if (options$engine == "glue" || grepl("glue_", options$engine)) {
      if (isTRUE(has_warned[[options$engine]])) {
        return(invisible())
      } else {
        has_warned[[options$engine]] <<- TRUE
      }
      suggested <- sub("glue_?", "epoxy", options$engine)
      rlang::warn(c(
        sprintf("The `%s` engine from epoxy is deprecated. ", options$engine),
        "i" = sprintf("Please use the `%s` engine instead.", suggested)
      ))
    }
  }
})
