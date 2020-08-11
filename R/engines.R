epoxy_set_knitr_engines <- function() {
  knitr::knit_engines$set(
    glue = knitr_engine_glue,
    "glue_html" = knitr_engine_glue_html,
    "glue_latex" = knitr_engine_glue_latex,
    "whisker" = knitr_engine_whisker
  )
}

#' @export
knitr_engine_glue <- function(options) {
  out <- if (isTRUE(options$eval)) {
    options <- deprecate_glue_data_chunk_option(options)
    code <- paste(options$code, collapse = "\n")
    glue_env <- new.env(parent = options[[".envir"]] %||% knitr::knit_global())
    if (!is.null(options[["data"]])) {
      assign("$", epoxy_data_subset, envir = glue_env)
    }
    glue_data(
      .x = options[["data"]],
      code,
      .envir = glue_env,
      .open = options[[".open"]] %||% "{",
      .close = options[[".close"]] %||% "}",
      .na = options[[".na"]] %||% "",
      .trim = options[[".trim"]] %||% FALSE,
      .transformer = options[[".transformer"]] %||% glue::identity_transformer
    )
  }
  options$results <- "asis"
  options$echo <- options[[".echo"]] %||% FALSE
  knitr::engine_output(options, options$code, out)
}

#' @export
knitr_engine_glue_html <- function(options) {
  out <- if (isTRUE(options$eval) && is_htmlish_output()) {
    options <- deprecate_glue_data_chunk_option(options)
    code <- paste(options$code, collapse = "\n")
    glue_env <- new.env(parent = options[[".envir"]] %||% knitr::knit_global())
    if (!is.null(options[["data"]])) {
      assign("$", epoxy_data_subset, envir = glue_env)
    }
    code <- glue_data(
      .x = options[["data"]],
      code,
      .envir = glue_env,
      .open = options[[".open"]] %||% "{{",
      .close = options[[".close"]] %||% "}}",
      .na = options[[".na"]] %||% "",
      .trim = options[[".trim"]] %||% FALSE,
      .transformer = options[[".transformer"]] %||% glue::identity_transformer
    )
    code <- glue_collapse(code, sep = "\n")
    if (isTRUE(options$html_raw %||% TRUE)) {
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

#' @export
knitr_engine_glue_latex <- function(options) {
  out <- if (isTRUE(options$eval)) {
    options <- deprecate_glue_data_chunk_option(options)
    code <- paste(options$code, collapse = "\n")
    glue_env <- new.env(parent = options[[".envir"]] %||% knitr::knit_global())
    if (!is.null(options[["data"]])) {
      assign("$", epoxy_data_subset, envir = glue_env)
    }
    glue_data(
      .x = options[["data"]],
      code,
      .envir = glue_env,
      .open = options[[".open"]] %||% "<",
      .close = options[[".close"]] %||% ">",
      .na = options[[".na"]] %||% "",
      .trim = options[[".trim"]] %||% FALSE,
      .transformer = options[[".transformer"]] %||% glue::identity_transformer
    )
  }
  options$results <- "asis"
  options$echo <- options[[".echo"]] %||% FALSE
  knitr::engine_output(options, options$code, out)
}

#' @export
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
