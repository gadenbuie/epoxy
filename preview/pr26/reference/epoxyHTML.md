# Epoxy HTML Output for Shiny

Expermimental. An glue-like output for Shiny. `epoxyHTML()` lets you use
placeholders in your HTML such as `"{{height}}"`, that are provided
values from the server by giving
[`renderEpoxyHTML()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr26/reference/renderEpoxyHTML.md)
a `height` value.

## Usage

``` r
epoxyHTML(
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
  .trim = FALSE
)
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

  Opening template variable delimiter

- .close:

  Closing template variable delimiter

- .na:

  \[`character(1)`: ‘NA’\]  
  Value to replace `NA` values with. If `NULL` missing values are
  propagated, that is an `NA` result will cause `NA` output. Otherwise
  the value is replaced by the value of `.na`.

- .trim:

  \[`logical(1)`: ‘TRUE’\]  
  Whether to trim the input template with
  [`trim()`](https://glue.tidyverse.org/reference/trim.html) or not.

## Value

An HTML object.

## HTML Markup

By default, placeholders are inserted into a `<span>` element in your
UI, with the classes specified in `.class_item`.

`epoxyHTML()` also supports an HTML markup syntax similar to
[pug](https://pughtml.com/what-is-pug-html) (an HTML preprocessor). With
the markup syntax, `"{{h3.example.basic%basic-three demo}}"` creates a
`demo` placeholder inside an
`<h3 id="basic-three" class="example basic"></h3>` tag.

The placeholder template string follows the pattern
`{{<markup> <name>}}`. The markup syntax comes first, separated from the
placeholder name by a space. The HTML element is first, followed by
classes prefixed with `.` or and ID prefixed with `#`. The template
markup can contain only one element and one ID, but many classes can be
specified.

## See also

renderEpoxyHTML

## Examples

``` r
if (FALSE) {
library(shiny)

ui <- fluidPage(
  h2("epoxyHTML demo"),
  epoxy:::epoxyHTML(
    'test',
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
    thing = "THING",
    color = "COLOR",
    height = "HEIGHT",
    .class_item = "inner"
  ),
  tags$style(HTML(
    '.big { font-size: 1.5em; }
    .inner:not(.epoxy-item__placeholder) { background-color: rgba(254, 233, 105, 0.5)}
    .epoxy-item__placeholder { color: #999999; }'
  ))
)

server <- function(input, output, session) {
  output$test <- epoxy:::renderEpoxyHTML(
    thing = input$thing,
    color = input$color,
    height = input$height
  )
}

shinyApp(ui, server)
}
```
