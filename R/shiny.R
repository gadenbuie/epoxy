#' Epoxy HTML Output for Shiny
#'
#' Expermimental. An glue-like output for Shiny. `epoxyHTML()` lets you use
#' placeholders in your HTML such as `"{{height}}"`, that are provided values
#' from the server by giving `renderEpoxyHTML()` a `height` value.
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   h2("epoxyHTML demo"),
#'   epoxy:::epoxyHTML(
#'     'test',
#'     fluidRow(
#'       tags$div(
#'         class = "col-xs-4",
#'         selectInput("thing", "What is this {{color}} thing?", c("apple", "banana", "coconut", "dolphin"))
#'       ),
#'       tags$div(
#'         class = "col-xs-4",
#'         selectInput("color", "What color is the {{thing}}?", c("red", "blue", "black", "green", "yellow"))
#'       ),
#'       tags$div(
#'         class = "col-xs-4",
#'         sliderInput("height", "How tall is the {{color}} {{thing}}?", value = 5, min = 0, max = 10, step = 0.1, post = "ft")
#'       )
#'     ),
#'     tags$p(class = "big", "The {{color}} {{thing}} is {{height}} feet tall."),
#'     thing = "THING",
#'     color = "COLOR",
#'     height = "HEIGHT",
#'     .class_item = "inner"
#'   ),
#'   tags$style(HTML(
#'     '.big { font-size: 1.5em; }
#'     .inner:not(.epoxy-item__placeholder) { background-color: rgba(254, 233, 105, 0.5)}
#'     .epoxy-item__placeholder { color: #999999; }'
#'   ))
#' )
#'
#' server <- function(input, output, session) {
#'   output$test <- epoxy:::renderEpoxyHTML(
#'     thing = input$thing,
#'     color = input$color,
#'     height = input$height
#'   )
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @param .id The output id
#' @param ... UI elements or text (that will be treated as HTML), containing
#'   template variables. Use named values to provide initial placeholder values.
#' @param .class Classes added to the output div, in addition to `.epoxy-html`
#' @param .class_item Classes added to the `.container` wrapping each template
#'   variable.
#' @param .container The HTML element used for each template item, by default
#'   `<span>`.
#' @param .placeholder Default placeholder if a template variable placeholder
#'   isn't provided.
#' @inheritParams glue::glue
#'
#' @seealso renderEpoxyHTML
#' @return An HTML object.
#' @export
epoxyHTML <- function(
  .id,
  ...,
  .class = NULL,
  .class_item = NULL,
  .container = "div",
  .placeholder = "",
  .sep = "",
  .open = "{{",
  .close = "}}",
  .na = "",
  .trim = FALSE
) {
  match.arg(.container, names(htmltools::tags))

  dots <- list(...)
  dots$.placeholder = .placeholder
  dots$.transformer = transformer_span(.class_item)
  dots$.na = .na
  dots$.sep = .sep
  dots$.trim = .trim
  dots$.open = .open
  dots$.close = .close

  tags <- purrr::keep(dots, is_tag)
  deps <- if (length(tags)) {
    purrr::flatten(purrr::map(tags, htmltools::findDependencies))
  }

  dots <- purrr::map_if(dots, ~ inherits(.x, "shiny.tag"), format)

  res <- rlang::eval_bare(rlang::call2(glue::glue, !!!dots))

  out <- htmltools::tags[[.container]](
    id = .id,
    class = collapse_space(c("epoxy-html", .class)),
    htmltools::HTML(res),
    htmltools::htmlDependency(
      name = "epoxy",
      version = "0.0.1",
      package = "epoxy",
      src = "srcjs",
      script = "output-epoxy.js",
      all_files = FALSE
    )
  )
  if (!is.null(deps) && length(deps)) {
    htmltools::attachDependencies(out, deps)
  } else {
    out
  }
}

transformer_js_literal <- function(text, envir) {
  paste0("${", text, "}")
}

transformer_span <- function(class = NULL) {
  class <- collapse_space(c("epoxy-item__placeholder", class))
  function(text, envir) {
    placeholder <- tryCatch(
      eval(parse(text = text), envir = envir),
      error = function(...) get(".placeholder", envir = envir)
    )
    htmltools::tags$span(
      class = class,
      `data-epoxy-item` = text,
      placeholder
    )
  }
}

#' Render Epoxy Output
#'
#' Server-side render function used to provide values for template items. Use
#' named values matching the template variable names in the associated
#' `epoxyHTML()`.
#'
#' @param ... Named values corresponding to the template variables created with
#'   the associated [epoxyHTML()] UI element.
#' @param env The environment in which to evaluate the `...`
#' @param outputArgs A list of arguments to be passed through to the implicit
#'   call to [epoxyHTML()] when `renderEpoxyHTML` is used in an interactive R
#'   Markdown document.
#'
#' @seealso epoxyHTML
#' @export
renderEpoxyHTML <- function(..., env = parent.frame(), outputArgs = list()) {
  installExprFunction(list(...), "func", env, quoted = FALSE)
  createRenderFunction(
    func,
    function(value, session, name, ...) {
      value <- as.list(value)
      stopifnot(!is.null(names(value)))
      value
    },
    epoxyHTML,
    outputArgs
  )
}
