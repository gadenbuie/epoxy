# One-shot epoxy transformers

These transformers are useful for applying the same transformation to
every replacement in the template.

## Usage

``` r
epoxy_transform_wrap(
  before = "**",
  after = before,
  engine = NULL,
  transformer = glue::identity_transformer,
  syntax = lifecycle::deprecated()
)

epoxy_transform_bold(engine = NULL, transformer = glue::identity_transformer)

epoxy_transform_italic(engine = NULL, transformer = glue::identity_transformer)

epoxy_transform_apply(
  .f = identity,
  ...,
  transformer = glue::identity_transformer
)

epoxy_transform_code(engine = NULL, transformer = glue::identity_transformer)

epoxy_transform_collapse(
  sep = ", ",
  last = sep,
  language = NULL,
  ...,
  transformer = glue::identity_transformer
)
```

## Arguments

- before, after:

  In `epoxy_transform_wrap()`, the characters to be added before and
  after variables in the template string.

- engine:

  One of `"markdown"` (or `"md"`), `"html"`, or `"latex"`. The default
  is chosen based on the engine of the chunk where the transform
  function is called, or according to the option `epoxy.engine`.
  Caution: invalid options are silently ignored, falling back to
  `"markdown"`.

- transformer:

  The transformer to apply to the replacement string. This argument is
  used for chaining the transformer functions. By providing a function
  to this argument you can apply an additional transformation after the
  current transformation. In nearly all cases, you can let
  [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/epoxy_transform.md)
  handle this for you. The chain ends when
  [`glue::identity_transformer()`](https://glue.tidyverse.org/reference/identity_transformer.html)
  is used as the `transformer`.

- syntax:

  **\[deprecated\]** Use `engine` instead.

- .f:

  A function, function name or
  [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)-style
  inline function.

- ...:

  Transformer functions, e.g. epoxy_transform_bold or the name of an
  epoxy transform function, e.g. `"bold"`, or a call to a transform
  function, e.g. `epoxy_transform_bold()`.
  [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/epoxy_transform.md)
  chains the transformer functions together, applying the transformers
  in order from first to last.

  For example, `epoxy_transform("bold", "collapse")` results in replaced
  strings that are emboldened *and then* collapsed, e.g.
  `**a** and **b**`. On the other hand,
  `epoxy_transform("collapse", "bold")` will collapse the vector *and
  then* embolden the entire string.

  In `epoxy_transform_apply()`, the `...` are passed to the underlying
  call the underlying function call.

  In `epoxy_transform_collapse()`, the `...` are ignored.

- sep, last:

  The separator to use when joining the vector elements when the
  expression ends with a `*`. Elements are separated by `sep`, except
  for the last two elements, which use `last`.

- language:

  In `epoxy_transform_collapse()`, `language` is passed to
  [`and::and()`](https://pkg.rossellhayes.com/and/reference/and.html) or
  [`and::or()`](https://pkg.rossellhayes.com/and/reference/and.html) to
  choose the correct and/or phrase and spacing for the `language`. By
  default, will follow the system language. See
  [and::and_languages](https://pkg.rossellhayes.com/and/reference/and_languages.html)
  for supported languages.

## Value

A function of `text` and `envir` suitable for the `.transformer`
argument of
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## Functions

- `epoxy_transform_wrap()`: Wrap variables with text added before or
  after the inline expression.

- `epoxy_transform_bold()`: Embolden variables using `**` in markdown,
  `<strong>` in HTML, or `\textbf{}` in LaTeX.

- `epoxy_transform_italic()`: Italicize variables using `_` in markdown,
  `<em>` in HTML, or `\emph{}` in LaTeX.

- `epoxy_transform_apply()`: Apply a function to all replacement
  expressions.

- `epoxy_transform_code()`: Code format variables using ``` `` ``` in
  markdown, `<code>` in HTML, or `\texttt{}` in LaTeX.

- `epoxy_transform_collapse()`: Collapse vector variables with a
  succinct syntax (but see
  [`epoxy_transform_inline()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/epoxy_transform_inline.md)
  for a more readable option).

## Examples

``` r
abc <- c("a", "b", "c")

epoxy("{abc}", .transformer = epoxy_transform_wrap("'"))
#> 'a'
#> 'b'
#> 'c'

epoxy("{abc}", .transformer = epoxy_transform_bold())
#> **a**
#> **b**
#> **c**

epoxy("{abc}", .transformer = epoxy_transform_italic())
#> _a_
#> _b_
#> _c_

epoxy("{abc}", .transformer = epoxy_transform_code())
#> `a`
#> `b`
#> `c`

epoxy("{abc}", .transformer = epoxy_transform_apply(toupper))
#> A
#> B
#> C
```
