#' Epoxy HTML Output for Shiny
#'
#' Expermimental. An glue-like output for Shiny. `epoxyHTML()` lets you use
#' placeholders in your HTML such as `"{{height}}"`, that are provided values
#' from the server by giving `renderEpoxyHTML()` a `height` value.
#'
#' @section HTML Markup: By default, placeholders are inserted into a `<span>`
#' element in your UI, with the classes specified in `.class_item`.
#'
#' `epoxyHTML()` also supports an HTML markup syntax similar to
#' [pug](https://pughtml.com/what-is-pug-html) (an HTML preprocessor). With the
#' markup syntax, `"{{h3.example.basic%basic-three demo}}"` creates a `demo`
#' placeholder inside an `<h3 id="basic-three" class="example basic"></h3>` tag.
#'
#' The placeholder template string follows the pattern `{{<markup> <name>}}`.
#' The markup syntax comes first, separated from the placeholder name by a
#' space. The HTML element is first, followed by classes prefixed with `.` or
#' and ID prefixed with `#`. The template markup can contain only one element
#' and one ID, but many classes can be specified.
#'
#' @examplesIf rlang::is_installed("shiny")
#' ui <- shiny::fluidPage(
#'   shiny::h2("epoxyHTML demo"),
#'   epoxyHTML(
#'     .id = 'test',
#'     .class_item = "inner",
#'     shiny::fluidRow(
#'       shiny::tags$div(
#'         class = "col-xs-4",
#'         shiny::selectInput(
#'           inputId = "thing",
#'           label = "What is this {{color}} thing?",
#'           choices = c("apple", "banana", "coconut", "dolphin")
#'         )
#'       ),
#'       shiny::tags$div(
#'         class = "col-xs-4",
#'         shiny::selectInput(
#'           inputId = "color",
#'           label = "What color is the {{thing}}?",
#'           c("red", "blue", "black", "green", "yellow")
#'         )
#'       ),
#'       shiny::tags$div(
#'         class = "col-xs-4",
#'         shiny::sliderInput(
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
#'     shiny::tags$p(class = "big", "The {{color}} {{thing}} is {{height}} feet tall."),
#'     # Default values for placeholders above.
#'     thing = "THING",
#'     color = "COLOR",
#'     height = "HEIGHT"
#'   ),
#'   shiny::tags$style(shiny::HTML(
#'     '.big { font-size: 1.5em; }
#'     .inner:not(.epoxy-item__placeholder) { background-color: rgba(254, 233, 105, 0.5)}
#'     .epoxy-item__placeholder { color: #999999; }'
#'   ))
#' )
#'
#' server <- function(input, output, session) {
#'   output$test <- renderEpoxyHTML(
#'     thing = input$thing,
#'     color = input$color,
#'     height = input$height
#'   )
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
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
#' @param .container_item The name of the HTML element to be used for each template item,
#'   by default `"span"`.
#' @param .placeholder Default placeholder if a template variable placeholder
#'   isn't provided.
#' @param .open Opening template variable delimiter
#' @param .close Closing template variable delimiter
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
  rlang::check_installed("stringr")

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
    class = collapse_space(c("epoxy-html epoxy-init", .class)),
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

epoxyHTML_transformer <- function(
  class = NULL,
  element = "span"
) {
  class <- collapse_space(c("epoxy-item__placeholder", class))

  function(text, envir) {
    markup <- parse_html_markup(text)
    placeholder <- rlang::env_get(
      markup$item,
      env = envir,
      inherit = TRUE,
      default = get(".placeholder", envir = envir, inherits = FALSE)
    )
    tag_name <- markup$element
    if (is.null(tag_name)) tag_name <- element
    if (!is.null(markup$class)) {
      class <- collapse_space(class, markup$class)
    }
    htmltools::tag(
      tag_name,
      list(
        class = class,
        id = markup$id,
        `data-epoxy-item` = markup$item,
        htmltools::HTML(placeholder)
      )
    )
  }
}

parse_html_markup <- function(x) {
  x_og <- x
  x <- trimws(x)
  if (sum(grepl(" ", x)) == 0) {
    return(list(item = x))
  }
  x <- strsplit(x, " ")[[1]]
  if (length(x) != 2) {
    # TODO: better error message
    rlang::abort(glue::glue('Bad markup: "{x_og}"'))
  }
  item_id <- x[2]
  rgx_markup <- "(([#%. ]|^)[[:alnum:]_-]+)"
  m <- stringr::str_extract_all(x[1], rgx_markup)[[1]]
  if (!length(m)) return(list(item = item_id))
  out <- list(item = item_id)
  for (m_part in m) {
    if (grepl("^[.]", m_part)) {
      out$class <- c(out$class, sub("^[.]", "", m_part))
    } else if (grepl("^[#%]", m_part)) {
      if (!is.null(out$id)) {
        rlang::abort("Multiple IDs were specified, please specify only one ID.")
      }
      out$id <- sub("^[#%]", "", m_part)
    } else {
      if (!is.null(out$element)) {
        rlang::abort("Multiple elements were specified, please specify only one element.")
      }
      if (!m_part %in% names(htmltools::tags)) {
        rlang::abort(glue::glue("Uknown tag used in markup: `{m_part}`"))
      }
      out$element <- m_part
    }
  }
  if (!is.null(out$class)) {
    out$class <- paste(out$class, collapse = " ")
  }
  out
}

#' Render Epoxy Output
#'
#' Server-side render function used to provide values for template items. Use
#' named values matching the template variable names in the associated
#' `epoxyHTML()`. When the values are updated by the app, `renderEpoxyHTML()`
#' will update the values shown in the app's UI.
#'
#' @examples
#' # This small app shows the current time using `epoxyHTML()`
#' # to provide the HTML template and `renderEpoxyHTML()` to
#' # update the current time every second.
#'
#' ui <- shiny::fluidPage(
#'   shiny::h2("Current Time"),
#'   epoxyHTML(
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
#'   output$time <- renderEpoxyHTML(time = current_time())
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
#'
#' @param ... Named values corresponding to the template variables created with
#'   the associated [epoxyHTML()] UI element.
#' @param .list A named list or a [shiny::reactiveValues()] list with names
#'   corresponding to the template variables created with the associated
#'   [epoxyHTML()] UI element.
#' @param env The environment in which to evaluate the `...`
#' @param outputArgs A list of arguments to be passed through to the implicit
#'   call to [epoxyHTML()] when `renderEpoxyHTML` is used in an interactive R
#'   Markdown document.
#'
#' @return A server-side Shiny render function that should be assigned to
#'   Shiny's `output` object and named to match the `.id` of the corresponding
#'   [epoxyHTML()] call.
#'
#' @seealso [epoxyHTML()]
#' @export
renderEpoxyHTML <- function(..., .list = NULL, env = parent.frame(), outputArgs = list()) {
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
    epoxyPrepare,
    function(value, session, name, ...) {
      value <- as.list(value)
      stopifnot(!is.null(names(value)))
      value
    },
    epoxyHTML,
    outputArgs
  )
}

format_tags <- function(x) {
  if (!inherits(x, c("shiny.tag", "shiny.tag.list"))) {
    return(x)
  }
  format(x)
}
