epoxyHTML <- function(
  .id,
  ...,
  .class = NULL,
  .class_item = NULL,
  .container = "div",
  .placeholder = "",
  .sep = "",
  .start = "{",
  .end = "}",
  .na = "",
  .trim = FALSE
) {
  match.arg(.container, names(htmltools::tags))

  dots <- list(...)
  dots$.placeholder = .placeholder
  dots$.transformer = transformer_span(.class_item)
  dots$.na = .na
  dots$.sep = .sep
  dots$.end = .end
  dots$.trim = .trim
  dots$.start = .start

  dots <- purrr::map_if(dots, ~ inherits(.x, "shiny.tag"), format)

  x <- rlang::eval_bare(rlang::call2(glue::glue, !!!dots))

  htmltools::tags[[.container]](
    id = .id,
    class = collapse_space(c("epoxy-html", .class)),
    htmltools::HTML(x),
    htmltools::htmlDependency(
      name = "epoxy",
      version = "0.0.1",
      package = "epoxy",
      src = "srcjs",
      script = "output-epoxy.js",
      all_files = FALSE
    )
  )
}

transformer_js_literal <- function(text, envir) {
  paste0("${", text, "}")
}

transformer_span <- function(class = NULL) {
  class <- collapse_space(c("epoxy-item__placeholder", class))
  function(text, envir) {
    placeholder <- if (exists(text, envir = envir)) {
      eval(parse(text = text), envir = envir)
    } else {
      get(".placeholder", envir = envir)
    }
    htmltools::tags$span(
      class = class,
      `data-epoxy-item` = text,
      placeholder
    )
  }
}

renderExpoxyHTML <- function(expr, env = parent.frame(), quoted = FALSE, outputArgs=list()) {
  installExprFunction(expr, "func", env, quoted)

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
