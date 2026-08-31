# Epoxy HTML Mustache Template

A Shiny output that uses [mustache
templating](https://mustache.github.io/) to render HTML. Mustache is a
powerful template language with minimal internal logic. The advantage of
`ui_epoxy_mustache()` is that all parts of the HTML can be templated –
including element attributes – whereas
[`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
requires that the dynamic template variables appear in the text portion
of the UI. The downside is that the entire template is re-rendered (in
the browser), each time that updated data is sent from the server –
unlike
[`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md),
whose updates are specific to the parts of the data that have changed.

## Usage

``` r
ui_epoxy_mustache(
  id,
  ...,
  .file = NULL,
  .sep = "",
  .container = "epoxy-mustache"
)

ui_epoxy_whisker(
  id,
  ...,
  .file = NULL,
  .sep = "",
  .container = "epoxy-mustache"
)
```

## Arguments

- id:

  The ID of the output.

- ...:

  Character strings of HTML or
  [htmltools::tags](https://rstudio.github.io/htmltools/reference/builder.html).
  All elements should be unnamed.

- .file:

  A path to a template file. If provided, no other template lines should
  be included in `...`.

- .sep:

  The separator used to concatenate elements in `...`.

- .container:

  A character tag name, e.g. `"div"` or `"span"`, or a function that
  returns an
  [`htmltools::tag()`](https://rstudio.github.io/htmltools/reference/builder.html).

## Value

Returns a Shiny output UI element.

## Functions

- `ui_epoxy_whisker()`: An alias for `ui_epoxy_mustache()`, provided
  because R users are more familiar with this syntax via the whisker
  package.

## See also

[`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md),
[`render_epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/render_epoxy.md)

Other Mustache-style template functions:
[`epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_mustache.md)

## Examples

``` r
if (FALSE) { # rlang::is_installed("shiny") && rlang::is_interactive()
library(shiny)

ui <- fluidPage(
  fluidRow(
    style = "max-width: 600px; margin: 0 auto",
    column(
      width = 6,
      ui_epoxy_mustache(
        id = "template",
        h2(class = "{{heading_class}}", "Hello, {{name}}!"),
        "{{#favorites}}",
        p("Your favorite fruits are..."),
        tags$ul(HTML("{{#fruits}}<li>{{.}}</li>{{/fruits}}")),
        "{{/favorites}}",
        "{{^favorites}}<p>Do you have any favorite fruits?</p>{{/favorites}}"
      )
    ),
    column(
      width = 6,
      h2("Inputs"),
      textInput("name", "Your name"),
      textInput("fruits", "Favorite fruits", placeholder = "apple, banana"),
      helpText("Enter a comma-separated list of fruits.")
    )
  )
)

server <- function(input, output, session) {
  user_name <- reactive({
    if (!nzchar(input$name)) return("user")
    input$name
  })

  favorites <- reactive({
    if (identical(input$fruits, "123456")) {
      # Errors are equivalent to "empty" values,
      # the rest of the template will still render.
      stop("Bad fruits, bad!")
    }

    if (!nzchar(input$fruits)) return(NULL)
    list(fruits = strsplit(input$fruits, "\\s*,\\s*")[[1]])
  })

  output$template <- render_epoxy(
    name = user_name(),
    heading_class = if (user_name() != "user") "text-success",
    favorites = favorites()
  )
}

if (interactive()) {
  shiny::shinyApp(ui, server)
}
}
if (FALSE) { # rlang::is_interactive()
run_epoxy_example_app("ui_epoxy_mustache")
}
```
