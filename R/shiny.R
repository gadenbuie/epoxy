#' Epoxy HTML Output for Shiny
#'
#' A glue-like output for Shiny. `ui_epoxy_html()` lets you use placeholders in your
#' HTML such as `"{{first_name}}"`, that are provided values from the server by
#' giving `render_epoxy()` a `first_name` value.
#'
#' @section HTML Markup: By default, placeholders are inserted into a `<span>`
#' element in your UI, with the classes specified in `.class_item`.
#'
#' `ui_epoxy_html()` also supports an HTML markup syntax similar to
#' [pug](https://pughtml.com/what-is-pug-html) (an HTML preprocessor). With the
#' markup syntax, `"{{h3.example.basic#basic-three demo}}"` creates a `demo`
#' placeholder inside an `<h3 id="basic-three" class="example basic"></h3>` tag.
#'
#' The placeholder template string follows the pattern `{{<markup> <name>}}`.
#' The markup syntax comes first, separated from the placeholder name by a
#' space. The HTML element is first, followed by classes prefixed with `.` or
#' and ID prefixed with `#`. The template markup can contain only one element
#' and one ID, but many classes can be specified.
#'
#' @examplesIf rlang::is_installed("shiny")
#' library(shiny)
#'
#' ui <- fluidPage(
#'   h2("ui_epoxy_html demo"),
#'   ui_epoxy_html(
#'     .id = 'example',
#'     .class_item = "inner",
#'     fluidRow(
#'       tags$div(
#'         class = "col-xs-4",
#'         selectInput(
#'           inputId = "thing",
#'           label = "What is this {{color}} thing?",
#'           choices = c("apple", "banana", "coconut", "dolphin")
#'         )
#'       ),
#'       tags$div(
#'         class = "col-xs-4",
#'         selectInput(
#'           inputId = "color",
#'           label = "What color is the {{thing}}?",
#'           c("red", "blue", "black", "green", "yellow")
#'         )
#'       ),
#'       tags$div(
#'         class = "col-xs-4",
#'         sliderInput(
#'           inputId = "height",
#'           label = "How tall is the {{color}} {{thing}}?",
#'           value = 5,
#'           min = 0,
#'           max = 10,
#'           step = 0.1,
#'           post = "ft"
#'         )
#'       )
#'     ),
#'     tags$p(class = "big", "The {{color}} {{thing}} is {{height}} feet tall."),
#'     # Default values for placeholders above.
#'     thing = "THING",
#'     color = "COLOR",
#'     height = "HEIGHT"
#'   ),
#'   tags$style(HTML(
#'     '.big { font-size: 1.5em; }
#'      .inner { background-color: rgba(254, 233, 105, 0.5);}
#'      .epoxy-item__placeholder { color: #999999; background-color: unset; }'
#'   ))
#' )
#'
#' server <- function(input, output, session) {
#'   output$example <- render_epoxy(
#'     thing = input$thing,
#'     color = input$color,
#'     height = input$height
#'   )
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @param .id The output id
#' @param ... UI elements or text (that will be treated as HTML), containing
#'   template variables. Use named values to provide initial placeholder values.
#' @param .class Classes added to the output div, in addition to `.epoxy-html`
#' @param .class_item Classes added to the `.container` wrapping each template
#'   variable.
#' @param .container The name of the HTML element to be used for the output
#'   element, by default `"div"`.
#' @param .container_item The name of the HTML element to be used for each
#'   template item, by default `"span"`.
#' @param .placeholder Default placeholder if a template variable placeholder
#'   isn't provided.
#' @inheritParams epoxy
#' @inheritParams glue::glue
#'
#' @seealso [ui_epoxy_mustache()], [render_epoxy()]
#' @return An HTML object.
#' @export
ui_epoxy_html <- function(
  .id,
  ...,
  .class = NULL,
  .class_item = NULL,
  .container = "div",
  .container_item = "span",
  .placeholder = "",
  .sep = "",
  .open = "{{",
  .close = "}}",
  .na = "",
  .null = "",
  .literal = FALSE,
  .trim = FALSE
) {
  .container <- match.arg(.container, names(htmltools::tags))
  .container_item <- match.arg(.container_item, names(htmltools::tags))

  dots <- list(...)
  dots$.placeholder = .placeholder
  dots$.transformer = epoxyHTML_transformer(.class_item, .container_item)
  dots$.na = .na
  dots$.sep = .sep
  dots$.null = .null
  dots$.trim = .trim
  dots$.open = .open %||% "{{"
  dots$.close = .close %||% "}}"
  # disable # as comment so we can use it for id syntax (requires glue >= 1.5)
  dots$.comment <- character()
  dots$.literal = .literal
  dots$.envir = new.env(parent = emptyenv())

  tags <- purrr::keep(dots, is_tag)
  deps <- if (length(tags)) {
    purrr::flatten(purrr::map(tags, htmltools::findDependencies))
  }

  dots <- purrr::map_if(dots, ~ inherits(.x, "shiny.tag"), format)

  res <- rlang::eval_bare(rlang::call2(glue::glue, !!!dots))

  out <- htmltools::tags[[.container]](
    id = .id,
    class = "epoxy-html epoxy-init",
    class = .class,
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

#' @describeIn ui_epoxy_html `r lifecycle::badge('deprecated')` Deprecated
#'   alias, please use `ui_epoxy_html()`.
#' @export
epoxyHTML <- function(.id, ...) {
  lifecycle::deprecate_soft(
    "0.1.0", "epoxyHTML()", "ui_epoxy_html()",
    details = "`epoxyHTML()` was renamed. Please use the new name at your earliest convenience."
  )
  ui_epoxy_html(.id, ...)
}

transformer_js_literal <- function(text, envir) {
  paste0("${", text, "}")
}

epoxyHTML_transformer <- function(
  class = NULL,
  element = "span"
) {
  function(text, envir) {
    '!DEBUG epoxyHTML {text: "`text`"}'
    markup <- parse_html_markup(text)
    placeholder <- rlang::env_get(
      markup$item,
      env = envir,
      inherit = TRUE,
      default = get(".placeholder", envir = envir, inherits = FALSE)
    )
    tag_name <- markup$element
    if (is.null(tag_name)) tag_name <- element
    htmltools::tag(
      tag_name,
      list(
        class = "epoxy-item__placeholder",
        class = class,
        class = markup$class,
        id = markup$id,
        `data-epoxy-item` = markup$item,
        htmltools::HTML(placeholder)
      )
    )
  }
}

#' Epoxy HTML Mustache Template
#'
#' A Shiny output that uses [mustache templating](https://mustache.github.io/)
#' to render HTML. Mustache is a powerful template language with minimal
#' internal logic. The advantage of `ui_epoxy_mustache()` is that all parts
#' of the HTML can be templated -- including element attributes -- whereas
#' [ui_epoxy_html()] requires that the dynamic template variables appear in the text
#' portion of the UI.
#'
#' @examplesIf rlang::is_installed("shiny")
#' library(shiny)
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     style = "max-width: 600px; margin: 0 auto",
#'     column(
#'       width = 6,
#'       ui_epoxy_mustache(
#'         id = "template",
#'         h2(class = "{{heading_class}}", "Hello, {{name}}!"),
#'         "{{#fruits}}",
#'         p("Your favorite fruits are..."),
#'         tags$ul(HTML("{{#fruit}}<li>{{.}}</li>{{/fruit}}")),
#'         "{{/fruits}}",
#'         "{{^fruits}}<p>Do you have any favorite fruits?</p>{{/fruits}}"
#'       )
#'     ),
#'     column(
#'       width = 6,
#'       h2("Inputs"),
#'       textInput("name", "Your name"),
#'       textInput("fruits", "Favorite fruits", placeholder = "apple, banana"),
#'       helpText("Enter a comma-separated list of fruits.")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   user_name <- reactive({
#'     if (!nzchar(input$name)) return("user")
#'     input$name
#'   })
#'
#'   fruits <- reactive({
#'     if (!nzchar(input$fruits)) return(NULL)
#'     list(fruit = strsplit(input$fruits, "\\s*,\\s*")[[1]])
#'   })
#'
#'   output$template <- render_epoxy(
#'     name = user_name(),
#'     heading_class = if (user_name() != "user") "text-success",
#'     fruits = fruits()
#'   )
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
#'
#' @param id The ID of the output.
#' @param ... Character strings of HTML or [htmltools::tags]. All elements
#'   should be unnamed.
#' @param sep The separator used to concatenate elements in `...`.
#' @param container A character tag name, e.g. `"div"` or `"span"`, or a
#'   function that returns an [htmltools::tag()].
#'
#' @return Returns a Shiny output UI element.
#'
#' @seealso [ui_epoxy_html()], [render_epoxy()]
#' @export
ui_epoxy_mustache <- function(
  id,
  ...,
  sep = "",
  container = "div"
) {
  rlang::check_dots_unnamed()

  if (is.character(container)) {
    tag_name <- container
    container <- function(...) htmltools::tags[[tag_name]](...)
  }

  dots <- rlang::list2(...)
  if (!length(dots)) return(NULL)

  tags <- purrr::keep(dots, is_tag)
  deps <- purrr::flatten(purrr::map(tags, htmltools::findDependencies))

  dots <- purrr::map_if(dots, is_tag, format)
  dots <- purrr::flatten_chr(dots)

  if (!purrr::every(dots, is.character)) {
    rlang::abort("All template elements in `...` must be characters or htmltools tags.")
  }

  out <- container(
    id = id,
    class = "epoxy-mustache",
    `data-epoxy-template` = paste(dots, collapse = sep),
    epoxy_mustache_dependencies()
  )

  if (!is.null(deps) && length(deps)) {
    htmltools::attachDependencies(out, deps)
  } else {
    out
  }
}

#' @describeIn ui_epoxy_mustache An alias for `ui_epoxy_mustache()`, provided
#'   because R users are more familiar with this syntax via the \pkg{whisker}
#'   package.
#' @export
ui_epoxy_whisker <- ui_epoxy_mustache

epoxy_mustache_dependencies <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "mustache",
      package = "epoxy",
      version = "4.2.0",
      src = "lib/mustache",
      script = "mustache.min.js",
      all_files = FALSE
    ),
    htmltools::htmlDependency(
      name = "epoxy-mustache",
      package = "epoxy",
      version = "0.0.1",
      src = "srcjs",
      script = "output-epoxy-mustache.js",
      all_files = FALSE
    )
  )
}


#' Render Epoxy Output
#'
#' Server-side render function used to provide values for template items. Use
#' named values matching the template variable names in the associated
#' [ui_epoxy_html()] or [ui_epoxy_mustache()]. When the values are updated by
#' the app, `render_epoxy()` will update the values shown in the app's UI.
#'
#' @examplesIf rlang::is_installed("shiny")
#' # This small app shows the current time using `ui_epoxy_html()`
#' # to provide the HTML template and `render_epoxy()` to
#' # update the current time every second.
#'
#' ui <- shiny::fluidPage(
#'   shiny::h2("Current Time"),
#'   ui_epoxy_html(
#'     "time",
#'     shiny::p("The current time is {{strong time}}.")
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   current_time <- shiny::reactive({
#'     shiny::invalidateLater(1000)
#'     strftime(Sys.time(), "%F %T")
#'   })
#'
#'   output$time <- render_epoxy(time = current_time())
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
#'
#' @param ... Named values corresponding to the template variables created with
#'   the associated [ui_epoxy_html()] UI element.
#' @param .list A named list or a [shiny::reactiveValues()] list with names
#'   corresponding to the template variables created with the associated
#'   [ui_epoxy_html()] UI element.
#' @param env The environment in which to evaluate the `...`
#' @param outputArgs A list of arguments to be passed through to the implicit
#'   call to [ui_epoxy_html()] when `render_epoxy` is used in an interactive R
#'   Markdown document.
#' @param outputFunc Either [ui_epoxy_html()] or [ui_epoxy_mustache()], i.e. the
#'   UI function to be paired with this output. This is only used when calling
#'   `render_epoxy()` in an Shiny runtime R Markdown document and when you
#'   are only providing the output without an explicit, corresponding UI
#'   element.
#'
#' @return A server-side Shiny render function that should be assigned to
#'   Shiny's `output` object and named to match the `.id` of the corresponding
#'   [ui_epoxy_html()] call.
#'
#' @seealso [ui_epoxy_html()]
#' @export
render_epoxy <- function(
  ...,
  .list = NULL,
  env = parent.frame(),
  outputFunc = ui_epoxy_html,
  outputArgs = list()
) {
  rlang::check_installed("shiny")

  epoxyPrepare <- function(..., .list = NULL) {
    if (!is.null(.list)) {
      if (inherits(.list, "reactivevalues")) {
        .list <- shiny::reactiveValuesToList(.list)
      }
      if (!is.list(.list)) {
        stop("`.list` must be a list", call. = FALSE)
      }
      if (is.null(names(.list))) {
        stop("`.list` must be a named list", call. = FALSE)
      }
    }
    lapply(c(list(...), .list), format_tags)
  }
  shiny::installExprFunction(
    name = "epoxyPrepare",
    quoted = FALSE,
    expr = epoxyPrepare(..., .list = .list)
  )
  shiny::createRenderFunction(
    func = epoxyPrepare,
    transform = function(value, session, name, ...) {
      value <- as.list(value)
      stopifnot(!is.null(names(value)))
      value
    },
    outputFunc = outputFunc,
    outputArgs = outputArgs
  )
}

#' @describeIn render_epoxy `r lifecycle::badge('deprecated')` Deprecated alias,
#'   please use `render_epoxy()`.
#' @export
renderEpoxyHTML <- function(..., env = parent.frame()) {
  lifecycle::deprecate_soft("0.1.0", "renderEpoxyHTML()", "render_epoxy()")
  render_epoxy(..., env = env)
}

format_tags <- function(x) {
  if (!inherits(x, c("shiny.tag", "shiny.tag.list"))) {
    return(x)
  }
  format(x)
}
