# Epoxy HTML Output for Shiny

A glue-like output for Shiny. `ui_epoxy_html()` lets you use
placeholders in your HTML such as `"{{first_name}}"`, that are provided
values from the server by giving
[`render_epoxy()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/render_epoxy.md)
a `first_name` value. Unlike
[`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/ui_epoxy_mustache.md),
updates are highly targeted: only the regions where the server-side data
have changed are updated in `ui_epoxy_html()`.

## Usage

``` r
ui_epoxy_html(
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
  .trim = FALSE,
  .aria_live = c("polite", "off", "assertive"),
  .aria_atomic = TRUE
)

epoxyHTML(.id, ...)
```

## Arguments

- .id:

  The output id

- ...:

  UI elements or text (that will be treated as HTML), containing
  template variables. Use named values to provide initial placeholder
  values.

- .class:

  Classes added to the output div, in addition to `.epoxy-html`

- .class_item:

  Classes added to the `.container` wrapping each template variable.

- .container:

  The name of the HTML element to be used for the output element, by
  default `"div"`.

- .container_item:

  The name of the HTML element to be used for each template item, by
  default `"span"`.

- .placeholder:

  Default placeholder if a template variable placeholder isn't provided.

- .sep:

  \[`character(1)`: ‘""’\]  
  Separator used to separate elements.

- .open:

  \[`character(1)`: ‘\\’\]  
  The opening delimiter around the template variable or expression.
  Doubling the full delimiter escapes it.

- .close:

  \[`character(1)`: ‘\\’\]  
  The closing delimiter around the template variable or expression.
  Doubling the full delimiter escapes it.

- .na:

  \[`character(1)`: ‘NA’\]  
  Value to replace `NA` values with. If `NULL` missing values are
  propagated, that is an `NA` result will cause `NA` output. Otherwise
  the value is replaced by the value of `.na`.

- .null:

  \[`character(1)`: ‘character()’\]  
  Value to replace NULL values with. If
  [`character()`](https://rdrr.io/r/base/character.html) whole output is
  [`character()`](https://rdrr.io/r/base/character.html). If `NULL` all
  NULL values are dropped (as in
  [`paste0()`](https://rdrr.io/r/base/paste.html)). Otherwise the value
  is replaced by the value of `.null`.

- .literal:

  \[`boolean(1)`: ‘FALSE’\]  
  Whether to treat single or double quotes, backticks, and comments as
  regular characters (vs. as syntactic elements), when parsing the
  expression string. Setting `.literal = TRUE` probably only makes sense
  in combination with a custom `.transformer`, as is the case with
  `glue_col()`. Regard this argument (especially, its name) as
  experimental.

- .trim:

  \[`logical(1)`: ‘TRUE’\]  
  Whether to trim the input template with
  [`trim()`](https://glue.tidyverse.org/reference/trim.html) or not.

- .aria_live, .aria_atomic:

  The
  [aria-live](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Attributes/aria-live)
  and
  [aria-atomic](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Attributes/aria-atomic)
  attribute values for the entire template region. By default, with
  `"polite"`, any updates within the region will be announced via screen
  readers.

  If your template includes changes in lots of disparate areas, it would
  be better to set `"aria-live" = "polite"` and `"aria-atomic" = "true"`
  on specific regions that should be announced together. Otherwise, the
  default is to announce the entire region within the `ui_epoxy_html()`
  whenever any of the values within change. In other words, set
  `.aria_live = "off"` and `.aria_atomic = NULL` on the
  `ui_epoxy_html()` parent item and then set `"aria-live" = "polite"`
  and `"aria-atomic" = "true"` on the parent containers of each region
  in the app that receives updates. `ui_epoxy_html()` does targeted
  updates, changing only the parts of the UI that have changed.

## Value

An HTML object.

## Functions

- `epoxyHTML()`: **\[deprecated\]** Deprecated alias, please use
  `ui_epoxy_html()`.

## HTML Markup

By default, placeholders are inserted into a `<span>` element in your
UI, with the classes specified in `.class_item`.

`ui_epoxy_html()` also supports an HTML markup syntax similar to
[pug](https://pughtml.com/what-is-pug-html) (an HTML preprocessor). As
an example, the markup syntax

    "{{h3.example.basic#basic-three demo}}"

creates a `demo` placeholder inside the following tag.

    <h3 id="basic-three" class="example basic"></h3>

The placeholder template string follows the pattern
`{{<markup> <name>}}`. The markup syntax comes first, separated from the
placeholder name by a space. The HTML element is first, followed by
classes prefixed with `.` or and ID prefixed with `#`. The template
markup can contain only one element and one ID, but many classes can be
specified.

By default, the placeholder is assumed to be text content and any HTML
in the sent to the placeholder will be escaped --- in other words if you
sent `"<strong>word</strong>"`, you'd see that exact literal text in
your app, rather than an emboldened **word**. To mark a placeholder as
safe to accept HTML, use `!!` before the placeholder, e.g.
`{{<markup> !!<name>}}`. So `{{h3 !!demo}}` will create an `<h3>` tag
that accepts HTML within it.

## See also

[`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/ui_epoxy_mustache.md),
[`render_epoxy()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/render_epoxy.md)

## Examples

``` r
library(shiny)

ui <- fluidPage(
  h2("ui_epoxy_html demo"),
  ui_epoxy_html(
    .id = "example",
    .class_item = "inner",
    fluidRow(
      tags$div(
        class = "col-xs-4",
        selectInput(
          inputId = "thing",
          label = "What is this {{color}} thing?",
          choices = c("apple", "banana", "coconut", "dolphin")
        )
      ),
      tags$div(
        class = "col-xs-4",
        selectInput(
          inputId = "color",
          label = "What color is the {{thing}}?",
          c("red", "blue", "black", "green", "yellow")
        )
      ),
      tags$div(
        class = "col-xs-4",
        sliderInput(
          inputId = "height",
          label = "How tall is the {{color}} {{thing}}?",
          value = 5,
          min = 0,
          max = 10,
          step = 0.1,
          post = "ft"
        )
      )
    ),
    tags$p(class = "big", "The {{color}} {{thing}} is {{height}} feet tall."),
    # Default values for placeholders above.
    thing = "THING",
    color = "COLOR",
    height = "HEIGHT"
  ),
  tags$style(HTML(
    ".big { font-size: 1.5em; }
     .inner { background-color: rgba(254, 233, 105, 0.5);}
     .epoxy-item__placeholder { color: #999999; background-color: unset; }"
  ))
)

server <- function(input, output, session) {
  output$example <- render_epoxy(
    thing = input$thing,
    color = input$color,
    height = input$height
  )
}

if (interactive()) {
  shinyApp(ui, server)
}
if (FALSE) { # rlang::is_interactive()
run_epoxy_example_app("ui_epoxy_html")
}
```
