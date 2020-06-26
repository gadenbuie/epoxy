
epoxyInlineClickChoice <- function(
  inputId,
  label,
  choices = '',
  selected = NULL,
  hover_color = NULL,
  focus_color = NULL
) {
  choices <- as.character(choices)
  if (is.null(selected) || !selected %in% choices) selected <- choices[1]
  htmltools::tagList(
    styleEpoxyInlineClickChoice(inputId, hover_color, focus_color),
    htmltools::tags$span(
      id = inputId,
      class = "epoxy-inline-clickChoice-input",
      title = label,
      tabindex = 0,
      `data-choices` = jsonlite::toJSON(choices),
      selected
    ),
    epoxy_dependency_clickChoice()
  )
}

updateEpoxyClickChoice <- function(
  inputId,
  choices = NULL,
  selected = NULL,
  session = shiny::getDefaultReactiveDomain()
) {
  if (!is.null(selected) && length(selected) > 1) {
    rlang::abort("`selected` must be a length-1 string.")
  }
  session$sendInputMessage(inputId, list(value = selected, choices = choices))
}

pick_selected <- function(ch, s) {
  ch_has_names <- !is.null(names(ch))
  if (is.null(s)) {
    val <- unname(ch[[1]])
    name <- if (ch_has_names) {
      names(ch)[[1]]
    } else val
    return(list(value = val, name = name))
  }

  if (ch_has_names) {
    if (s %in% names(ch)) {
      return(list(value = ch[[s]], name = s))
    }
  }

  if (s %in% ch) {
    name <- if (ch_has_names) names(ch)[[which(s == ch)]] else s
    return(list(value = s, name = name))
  }

  list(value = ch[[1]], name = if (ch_has_names) names(ch)[[1]] else ch[[1]])
}

data_clickChoice <- function(id, x) {
  x <- list(values = unname(x), names = names(x) %||% x)
  jsonlite::toJSON(x, auto_unbox = TRUE)

}

styleEpoxyInlineClickChoice <- function(id, hover_color = NULL, focus_color = NULL) {
  if (is.null(hover_color) && is.null(focus_color)) return(NULL)

  hover_color <- if (!is.null(hover_color)) {
    glue("  --epoxy-inline-clickChoice-color-hover: {hover_color};\n", .trim = FALSE)
  } else ""
  focus_color <- if(!is.null(focus_color)) {
    glue("  --epoxy-inline-clickChoice-color-focus: {focus_color};\n", .trime = FALSE)
  } else ""

  htmltools::tags$head(
    htmltools::tags$style(
      htmltools::HTML(glue('[id="{id}"] {{\n{hover_color}{focus_color}\n}}'))
    )
  )
}

epoxy_dependency_clickChoice <- function() {
  htmltools::htmlDependency(
    name = "epoxy-clickChoice",
    version = pkg_version(),
    package = "epoxy",
    src = "shiny",
    script = "input-epoxy-clickChoice.js",
    stylesheet = "input-epoxy-clickChoice.css",
    all_files = FALSE
  )
}
