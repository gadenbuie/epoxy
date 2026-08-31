# Render Epoxy Output

Server-side render function used to provide values for template items.
Use named values matching the template variable names in the associated
[`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
or
[`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_mustache.md).
When the values are updated by the app, `render_epoxy()` will update the
values shown in the app's UI.

## Usage

``` r
render_epoxy(
  ...,
  .list = NULL,
  env = parent.frame(),
  outputFunc = ui_epoxy_html,
  outputArgs = list()
)

renderEpoxyHTML(..., env = parent.frame())
```

## Arguments

- ...:

  Named values corresponding to the template variables created with the
  associated
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  UI element.

- .list:

  A named list or a
  [`shiny::reactiveValues()`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
  list with names corresponding to the template variables created with
  the associated
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  UI element.

- env:

  The environment in which to evaluate the `...`

- outputFunc:

  Either
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  or
  [`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_mustache.md),
  i.e. the UI function to be paired with this output. This is only used
  when calling `render_epoxy()` in an Shiny runtime R Markdown document
  and when you are only providing the output without an explicit,
  corresponding UI element.

- outputArgs:

  A list of arguments to be passed through to the implicit call to
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  when `render_epoxy` is used in an interactive R Markdown document.

## Value

A server-side Shiny render function that should be assigned to Shiny's
`output` object and named to match the `.id` of the corresponding
[`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
call.

## Functions

- `renderEpoxyHTML()`: **\[deprecated\]** Deprecated alias, please use
  `render_epoxy()`.

## See also

[`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md),
[`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_mustache.md)

## Examples

``` r
if (FALSE) { # rlang::is_installed("shiny") && rlang::is_interactive()
# This small app shows the current time using `ui_epoxy_html()`
# to provide the HTML template and `render_epoxy()` to
# update the current time every second.

ui <- shiny::fluidPage(
  shiny::h2("Current Time"),
  ui_epoxy_html(
    "time",
    shiny::p("The current time is {{strong time}}.")
  )
)

server <- function(input, output, session) {
  current_time <- shiny::reactive({
    shiny::invalidateLater(1000)
    strftime(Sys.time(), "%F %T")
  })

  output$time <- render_epoxy(time = current_time())
}

if (rlang::is_interactive()) {
  shiny::shinyApp(ui, server)
}
}
if (FALSE) { # rlang::is_interactive()
run_epoxy_example_app("render_epoxy")
}
```
