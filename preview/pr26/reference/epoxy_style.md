# epoxy Style Transformers

These transformers provide additional automatic formatting for the
template strings. They are designed to be used with the `.transformer`
chunk option of in `epoxy` chunks. You can use `epoxy_style()` to chain
several transformers together.

## Usage

``` r
epoxy_style(...)

epoxy_style_wrap(
  before = "**",
  after = "**",
  transformer = glue::identity_transformer
)

epoxy_style_bold(transformer = glue::identity_transformer)

epoxy_style_italic(transformer = glue::identity_transformer)

epoxy_style_code(transformer = glue::identity_transformer)

epoxy_style_collapse(
  sep = ", ",
  last = "",
  last_and = " and ",
  last_or = " or ",
  sep_and = sep,
  sep_or = sep,
  transformer = glue::identity_transformer
)
```

## Arguments

- ...:

  A list of style functions, e.g. `epoxy_style_bold` or the name of a
  style function, e.g. `"bold"`, or a call to a style function, e.g.
  `epoxy_style_bold()`. `epoxy_style()` chains the style functions
  together, applying the styles from left to right.

  For example, `epoxy_style("bold", "collapse")` results in replaced
  strings that are emboldened *and then* collapsed, e.g.
  `**a** and **b**`. On the other hand,
  `epoxy_style("collapse", "bold")` will collapse the vector *and then*
  embolden the entire string.

- before, after:

  In `epoxy_style_wrap()`, the characters to be added before and after
  variables in the template string.

- transformer:

  The transformer to apply to the replacement string. This argument is
  used for chaining the transformer functions. By providing a function
  to this argument you can apply an additional transformation after the
  current transformation. In nearly all cases, you can let
  `epoxy_style()` handle this for you. The chain ends when
  [`glue::identity_transformer()`](https://glue.tidyverse.org/reference/identity_transformer.html)
  is used as the `transformer`.

- sep, sep_and, sep_or:

  The separator to use when joining the vector elements when the
  variable ends in `*`, `&`, or `|` respectively. By default, these are
  all `", "`.

- last, last_and, last_or:

  Additional text added after `sep` before the last element when the
  variable ends in `*`, `&`, or `|` respectively.

## Value

A function of `text` and `envir` suitable for the `.transformer`argument
of [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## Functions

- `epoxy_style_wrap`: Wrap variables

- `epoxy_style_bold`: Embolden variables using markdown `**` syntax

- `epoxy_style_italic`: Italicize variables using markdown `_` syntax

- `epoxy_style_code`: Code format variables using markdown backtick
  syntax

- `epoxy_style_collapse`: Collapse vector variables.

## Examples

``` r
glue::glue("{letters[1:3]&}", .transformer = epoxy_style("bold", "collapse"))
#> **a**, **b** and **c**
glue::glue("{letters[1:3]&}", .transformer = epoxy_style("collapse", "bold"))
#> **a, b and c**
```
