# Render Epoxy Output

Server-side render function used to provide values for template items.
Use named values matching the template variable names in the associated
[`epoxyHTML()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr26/reference/epoxyHTML.md).

## Usage

``` r
renderEpoxyHTML(..., .list = NULL, env = parent.frame(), outputArgs = list())
```

## Arguments

- ...:

  Named values corresponding to the template variables created with the
  associated
  [`epoxyHTML()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr26/reference/epoxyHTML.md)
  UI element.

- .list:

  A named list or a
  [`shiny::reactiveValues()`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
  list with names corresponding to the template variables created with
  the associated
  [`epoxyHTML()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr26/reference/epoxyHTML.md)
  UI element.

- env:

  The environment in which to evaluate the `...`

- outputArgs:

  A list of arguments to be passed through to the implicit call to
  [`epoxyHTML()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr26/reference/epoxyHTML.md)
  when `renderEpoxyHTML` is used in an interactive R Markdown document.

## See also

epoxyHTML
