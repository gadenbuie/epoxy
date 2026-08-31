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

epoxy_transform_get(engine = c("md", "html", "latex"), inline = FALSE)

epoxy_transform_set(..., engine = NULL, syntax = lifecycle::deprecated())
```

## Arguments

- ...:

  Transformer functions, e.g.
  [epoxy_transform_bold](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md)
  or the name of an epoxy transform function, e.g. `"bold"`, or a call
  to a transform function, e.g.
  [`epoxy_transform_bold()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md).
  `epoxy_transform()` chains the transformer functions together,
  applying the transformers in order from first to last.

  For example, `epoxy_transform("bold", "collapse")` results in replaced
  strings that are emboldened *and then* collapsed, e.g.
  `**a** and **b**`. On the other hand,
  `epoxy_transform("collapse", "bold")` will collapse the vector *and
  then* embolden the entire string.

  In
  [`epoxy_transform_apply()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md),
  the `...` are passed to the underlying call the underlying function
  call.

  In
  [`epoxy_transform_collapse()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md),
  the `...` are ignored.

- engine:

  One of `"markdown"` (or `"md"`), `"html"`, or `"latex"`. The default
  is chosen based on the engine of the chunk where the transform
  function is called, or according to the option `epoxy.engine`.
  Caution: invalid options are silently ignored, falling back to
  `"markdown"`.

- syntax:

  **\[deprecated\]** Use `engine` instead.

- inline:

  In `epoxy_transform_get()`, whether to return the session-specific
  inline formatting functions for
  [`epoxy_transform_inline()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_inline.md).

## Value

A function of `text` and `envir` suitable for the `.transformer`
argument of
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## Functions

- `epoxy_transform()`: Construct a chained transformer using epoxy
  transformers for use as a glue transformer. The resulting transformers
  can be passed to the `.transformer` argument of
  [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
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

## Session-wide settings

To change the transformer used by
[`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md) and
the HTML and LaTeX variants, use `epoxy_transform_set()`. This function
takes the same values as `epoxy_transform()`, but makes them the default
transformer for any
[`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
calls that do not specify a transformer. By default, the setting is made
for all engines, but you can specify a single engine with the `engine`
argument.

Here's a small example that applies the
[bold](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md)
and
[collapse](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md)
transformers to all epoxy chunks:

    epoxy_transform_set("bold", "collapse")

Most often, you'll want to to update the default transformer to
customize the formatting functions used by the [inline
transformer](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_inline.md).
You can use `epoxy_transform_set()` to change settings of existing
formatting functions or to add new one. Pass the new function to an
argument with the dot-prefixed name.

In the next example I'm setting the `.dollar` transformation to use "K"
and "M" to abbreviate large numbers. I'm also adding my own
transformation that truncates long strings to fit in 8 characters.

    epoxy_transform_set(
      .dollar = scales::label_dollar(
        accuracy = 0.01,
        scale_cut = scales::cut_short_scale()
      ),
      .trunc8 = function(x) glue::glue_collapse(x, width = 8)
    )

    epoxy("{.dollar 12345678}")
    #> $12.34M
    epoxy("{.trunc8 12345678}")
    #> 12345...

Note that the `engine` argument can be used even with inline
tranformations, e.g. to apply a change only for HTML you can use
`engine = "html"`.

To unset the session defaults, you have two options:

1.  Unset everything by passing `NULL` to `epoxy_transform_set()`:

        epoxy_transform_set(NULL)

2.  Unset a single inline transformation by passing
    [`rlang::zap()`](https://rlang.r-lib.org/reference/zap.html) to the
    named argument:

        epoxy_transform_set(.dollar = rlang::zap())

Or you can provide new values to overwrite the current settings. And as
before, you can unset session defaults for a specific `engine`.

## See also

Other epoxy's glue transformers:
[`epoxy_transform_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_html.md),
[`epoxy_transform_inline()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_inline.md)

## Examples

``` r
epoxy("{.strong {.and letters[1:3]}}")
#> **a, b, and c**
epoxy("{.and {.strong letters[1:3]}}")
#> **a**, **b**, and **c**

# If you used the development version of epoxy, the above is equivalent to:
epoxy("{letters[1:3]&}", .transformer = epoxy_transform("bold", "collapse"))
#> **a**, **b**, and **c**
epoxy("{letters[1:3]&}", .transformer = epoxy_transform("collapse", "bold"))
#> **a, b, and c**

# In an epoxy_html chunk...
epoxy_html("{{.strong {{.or letters[1:3]}} }}")
#> <span class="strong">a, b, or c</span>

# Or in an epoxy_latex chunk...
epoxy_latex("<<.and <<.strong letters[1:3]>> >>")
#> \textbf{a}, \textbf{b}, and \textbf{c}

# ---- Other Transformers ----

# Format numbers with an inline transformation
amount <- 123.4234234
epoxy("{.number amount}")
#> 123
epoxy(
  "{.number amount}",
  .transformer = epoxy_transform_inline(
    .number = scales::label_number(accuracy = 0.01)
  )
)
#> 123.42

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
