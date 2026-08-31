# epoxy Transformers

These transformers provide additional automatic formatting for the
template strings. They are designed to be used with the `.transformer`
chunk option of in `epoxy` chunks. You can use `epoxy_transform()` to
chain several transformers together. `epoxy_transform()` and individual
epoxy transform functions can be used in `epoxy`, `epoxy_html` and
`epoxy_latex` chunks and will choose the correct engine for each.

## Usage

``` r
epoxy_transform(..., engine = NULL, syntax = lifecycle::deprecated())

epoxy_transform_get(engine = c("md", "html", "latex"))

epoxy_transform_set(..., engine = NULL, syntax = lifecycle::deprecated())
```

## Arguments

- ...:

  Transformer functions, e.g.
  [epoxy_transform_bold](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy_transform_one_shot.md)
  or the name of an epoxy transform function, e.g. `"bold"`, or a call
  to a transform function, e.g.
  [`epoxy_transform_bold()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy_transform_one_shot.md).
  `epoxy_transform()` chains the transformer functions together,
  applying the transformers in order from first to last.

  For example, `epoxy_transform("bold", "collapse")` results in replaced
  strings that are emboldened *and then* collapsed, e.g.
  `**a** and **b**`. On the other hand,
  `epoxy_transform("collapse", "bold")` will collapse the vector *and
  then* embolden the entire string.

  In
  [`epoxy_transform_apply()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy_transform_one_shot.md),
  the `...` are passed to the underlying call the underlying function
  call.

  In
  [`epoxy_transform_collapse()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy_transform_one_shot.md),
  the `...` are ignored.

- engine:

  One of `"markdown"` (or `"md"`), `"html"`, or `"latex"`. The default
  is chosen based on the engine of the chunk where the transform
  function is called, or according to the option `epoxy.engine`.
  Caution: invalid options are silently ignored, falling back to
  `"markdown"`.

- syntax:

  **\[deprecated\]** Use `engine` instead.

## Value

A function of `text` and `envir` suitable for the `.transformer`
argument of
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## Functions

- `epoxy_transform()`: Construct a chained transformer using epoxy
  transformers for use as a glue transformer. The resulting transformers
  can be passed to the `.transformer` argument of
  [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy.md)
  or [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `epoxy_transform_get()`: Get the default epoxy `.transformer` for all
  epoxy engines or for a subset of engines.

- `epoxy_transform_set()`: Set the default epoxy `.transformer` for all
  epoxy engines or for a subset of engines.

## Output-specific transformations

The `epoxy_transform_` functions will attempt to use the correct engine
for transforming the replacement text for markdown, HTML and LaTeX. This
choice is driven by the chunk engine where the transformer function is
used. The `epoxy` engine corresponds to markdown, `epoxy_html` to HTML,
and `epoxy_latex` to LaTeX.

Automatic engine selection only works when the epoxy transform functions
are used with epoxy knitr engines and during the knitr rendering
process. When used outside of this context, you can choose the desired
engine by setting the `engine` to one of `"markdown"`, `"html"` or
`"latex"`.

## See also

Other epoxy's glue transformers:
[`epoxy_transform_html`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy_transform_html.md)`()`,
[`epoxy_transform_inline`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy_transform_inline.md)`()`

## Examples

``` r
epoxy("{.strong {.and letters[1:3]}}")
#> **a, b and c**
epoxy("{.and {.strong letters[1:3]}}")
#> **a**, **b** and **c**

# If you used the development version of epoxy, the above is equivalent to:
epoxy("{letters[1:3]&}", .transformer = epoxy_transform("bold", "collapse"))
#> **a**, **b** and **c**
epoxy("{letters[1:3]&}", .transformer = epoxy_transform("collapse", "bold"))
#> **a, b and c**

# In an epoxy_html chunk...
epoxy_html("{{.strong {{.or letters[1:3] }} }}")
#> <span class="strong">a, b or c</span>

# Or in an epoxy_latex chunk...
epoxy_latex("<.and <.strong letters[1:3] >>")
#> \textbf{a}, \textbf{b} and \textbf{c}

# ---- Other Transformers ----

# Format numbers with an inline transformation
amount <- 123.4234234
epoxy("{.number amount}")
#> 123
epoxy(
  "{.number amount}",
  .transformer = epoxy_transform_inline(
    number = scales::label_number(accuracy = 0.01)
  )
)
#> Error in epoxy_transform_inline(number = scales::label_number(accuracy = 0.01)): Functions provided in `...` must be named with a leading dot (`.`).
#> ℹ Check: `number`.

# Apply _any_ function to all replacements
epoxy(
  "{amount} is the same as {amount}",
  .transformer = epoxy_transform_apply(round, digits = 0)
)
#> 123 is the same as 123

epoxy(
  "{amount} is the same as {amount}",
  .transformer = epoxy_transform(
    epoxy_transform_apply(~ .x * 100),
    epoxy_transform_apply(round, digits = 2),
    epoxy_transform_apply(~ paste0(.x, "%"))
  )
)
#> 12342.34% is the same as 12342.34%
```
