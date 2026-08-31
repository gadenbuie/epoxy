# Epoxy Markdown Template for Shiny

Create reactive HTML from a Markdown template. `ui_epoxy_markdown()`
uses the same template syntax as
[`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/ui_epoxy_html.md),
but rather than requiring HTML inputs, you can write in markdown. The
template is first rendered from markdown to HTML using
[`pandoc::pandoc_convert()`](https://cderv.github.io/pandoc/reference/pandoc_convert.html)
(if pandoc is available) or
[`commonmark::markdown_html()`](https://docs.ropensci.org/commonmark/reference/commonmark.html)
otherwise.

## Usage

``` r
ui_epoxy_markdown(
  .id,
  ...,
  .markdown_fn = NULL,
  .markdown_args = list(),
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
```

## Arguments

- .id:

  The output id

- ...:

  Unnamed arguments are treated as lines of markdown text, and named
  arguments are treated as initial values for templated variables.

- .markdown_fn:

  The function used to convert the markdown to HTML. This function is
  passed the markdown text as a character vector for the first argument
  and any additional arguments from the list `.markdown_args`. By
  default, we use
  [`pandoc::pandoc_convert()`](https://cderv.github.io/pandoc/reference/pandoc_convert.html)
  if pandoc is available, otherwise we use
  [`commonmark::markdown_html()`](https://docs.ropensci.org/commonmark/reference/commonmark.html).

- .markdown_args:

  A list of arguments to pass to
  [`commonmark::markdown_html()`](https://docs.ropensci.org/commonmark/reference/commonmark.html).

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
  default is to announce the entire region within the
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/ui_epoxy_html.md)
  whenever any of the values within change. In other words, set
  `.aria_live = "off"` and `.aria_atomic = NULL` on the
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/ui_epoxy_html.md)
  parent item and then set `"aria-live" = "polite"` and
  `"aria-atomic" = "true"` on the parent containers of each region in
  the app that receives updates.
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/ui_epoxy_html.md)
  does targeted updates, changing only the parts of the UI that have
  changed.

## Value

An HTML object.

## See also

[`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/ui_epoxy_html.md),
[`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/ui_epoxy_mustache.md),
[`render_epoxy()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/render_epoxy.md)

## Examples

``` r
library(shiny)

# Shiny epoxy template functions don't support inline transformations,
# so we still have to do some prep work ourselves.
bechdel <- epoxy::bechdel

as_dollars <- scales::label_dollar(
  scale_cut = scales::cut_short_scale()
)
bechdel$budget <- as_dollars(bechdel$budget)
bechdel$domgross <- as_dollars(bechdel$domgross)

vowels <- c("a", "e", "i", "o", "u")
bechdel$genre  <- paste(
  ifelse(substr(tolower(bechdel$genre), 1, 1) %in% vowels, "an", "a"),
  tolower(bechdel$genre)
)

movie_ids <- rlang::set_names(
  bechdel$imdb_id,
  bechdel$title
)

ui <- fixedPage(
  fluidRow(
    column(
      width = 3,
      selectInput("movie", "Movie", movie_ids),
      uiOutput("poster")
    ),
    column(
      width = 9,
      ui_epoxy_markdown(
        .id = "about_movie",
        "
## {{title}}

**Released:** {{ year }} \\
**Rated:** {{ rated }} \\
**IMDB Rating:** {{ imdb_rating }}

_{{ title }}_ is {{ genre }} film released in {{ year }}.
It was filmed in {{ country }} with a budget of {{ budget }}
and made {{ domgross }} at the box office.
_{{ title }}_ recieved a Bechdel rating of **{{ bechdel_rating }}**
for the following plot:

> {{ plot }}
"
      )
    )
  )
)

server <- function(input, output, session) {
  movie <- reactive({
    bechdel[bechdel$imdb_id == input$movie, ]
  })

  output$about_movie <- render_epoxy(.list = movie())
  output$poster <- renderUI(
    img(
      src = movie()$poster,
      alt = paste0("Poster for ", movie()$title),
      style = "max-height: 400px; max-width: 100%; margin: 0 auto; display: block;"
    )
  )
}

if (interactive()) {
  shinyApp(ui, server)
}
if (FALSE) { # rlang::is_interactive()
run_epoxy_example_app("ui_epoxy_markdown")
}
```
