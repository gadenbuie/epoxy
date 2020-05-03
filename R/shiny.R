epoxy_html <- function(
  .id,
  ...,
  .placeholder = list(),
  .class = NULL,
  .class_item = NULL,
  .container = "div",
  .sep = "",
  .start = "{",
  .end = "}",
  .na = "",
  .trim = FALSE
) {
  match.arg(.container, names(htmltools::tags))

  env <- new.env()
  env$placeholder <- .placeholder
  x <- glue(
    ...,
    .envir = env,
    .transformer = transformer_span(.class_item),
    .sep = .sep,
    .start = .start,
    .end = .end,
    .na = .na,
    .trim = .trim
  )

  htmltools::tags[[.container]](
    id = .id,
    class = collapse_space(c("epoxy-html", .class)),
    htmltools::HTML(x)
  )
}

transformer_js_literal <- function(text, envir) {
  paste0("${", text, "}")
}

transformer_span <- function(class = NULL) {
  class <- collapse_space(c("epoxy-item--init", class))
  function(text, envir) {
    htmltools::tags$span(
      class = class,
      `data-expoxy-item` = text,
      eval(parse(text = paste0("placeholder[['", text, "']]")), envir = envir)
    )
  }
}
