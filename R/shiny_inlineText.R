
epoxyInlineText <- function(inputId, label, value = "", hover_color = NULL, focus_color = NULL) {
  htmltools::tagList(
    styleEpoxyInlineText(inputId, hover_color, focus_color),
    htmltools::tags$span(
      class = "epoxy-inline-text-input",
      htmltools::tags$input(
        type = "text",
        id = inputId,
        name = inputId,
        value = value
      ),
      if (!is.null(label)) htmltools::tags$label(`for` = inputId, label)
    ),
    epoxy_dependency_text()
  )
}

updateEpoxyInlineText <- function(inputId, value, session = shiny::getDefaultReactiveDomain()) {
  stopifnot(is.character(value))
  stopifnot(length(value) <= 1)
  if (!length(value)) value <- ""
  session$sendInputMessage(inputId, value)
}

styleEpoxyInlineText <- function(id, hover_color = NULL, focus_color = NULL) {
  if (is.null(hover_color) && is.null(focus_color)) return(NULL)

  hover_color <- if (!is.null(hover_color)) {
    glue("  --epoxy-inline-text-color-hover: {hover_color};\n", .trim = FALSE)
  } else ""
  focus_color <- if(!is.null(focus_color)) {
    glue("  --epoxy-inline-text-color-focus: {focus_color};\n", .trime = FALSE)
  } else ""

  htmltools::tags$head(
    htmltools::tags$style(
      htmltools::HTML(glue('[id="{id}"], label[for="{id}"] {{\n{hover_color}{focus_color}\n}}'))
    )
  )
}

epoxy_dependency_text <- function() {
  htmltools::htmlDependency(
    name = "epoxy-text",
    version = pkg_version(),
    package = "epoxy",
    src = "shiny",
    script = "input-epoxy-text.js",
    stylesheet = "epoxy-text.css",
    all_files = FALSE
  )
}
