# Mustache-style string interpolation

**\[experimental\]** A wrapper around the [mustache templating
language](http://mustache.github.io/), provided by the
[whisker](https://cran.r-project.org/package=whisker) package. Under the
hood, `epoxy_mustache()` uses
[`whisker::whisker.render()`](https://rdrr.io/pkg/whisker/man/whisker.render.html)
to render the template, but adds a few conveniences:

- The template can be passed in `...` as a single string, several
  strings or as a vector of strings. If multiple strings are passed,
  they are collapsed with `.sep` (`"\n"` by default).

- `epoxy_mustache()` can be vectorized over the items in the `.data`
  argument. If `.data` is a data frame, vectorization is turned on by
  default so that you can iterate over the rows of the data frame. The
  output will be a character vector of the same length as the number of
  rows in the data frame.

## Usage

``` r
epoxy_mustache(
  ...,
  .data = parent.frame(),
  .sep = "\n",
  .vectorized = inherits(.data, "data.frame"),
  .partials = list()
)
```

## Arguments

- ...:

  A string or a vector of strings containing the template(s). Refer to
  the [mustache documentation](http://mustache.github.io/) for an
  overview of the syntax. If multiple strings are passed, they are
  collapsed with `.sep` (`"\n"` by default).

- .data:

  A data frame or a list. If `.data` is a data frame, `epoxy_mustache()`
  will transform the data frame so that the template can be applied to
  each row of the data frame. To avoid this transformation, wrap the
  `.data` value in [`I()`](https://rdrr.io/r/base/AsIs.html).

- .sep:

  The separator to use when collapsing multiple strings passed in `...`
  into a single template. Defaults to `"\n"`.

- .vectorized:

  If `TRUE` , `epoxy_mustache()` will vectorize over the items in
  `.data`. In other words, each item or row of `.data` will be used to
  render the template once. By default, `.vectorized` is set to `TRUE`
  if `.data` is a data frame and `FALSE` otherwise.

- .partials:

  A named list with partial templates. See
  [`whisker::whisker.render()`](https://rdrr.io/pkg/whisker/man/whisker.render.html)
  or the [mustache
  documentation](http://mustache.github.io/mustache.5.html#Partials) for
  details.

## Value

A character vector of length 1 if `.vectorized` is `FALSE` or a
character vector of the same length as the number of rows or items in
`.data` if `.vectorized` is `TRUE`.

## See also

Other Mustache-style template functions:
[`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_mustache.md)

## Examples

``` r
# The canonical mustache example
epoxy_mustache(
  "Hello {{name}}!",
  "You have just won {{value}} dollars!",
  "{{#in_ca}}",
  "Well, {{taxed_value}} dollars, after taxes.",
  "{{/in_ca}}",
  .data = list(
    name = "Chris",
    value = 10000,
    taxed_value = 10000 - (10000 * 0.4),
    in_ca = TRUE
  )
)
#> Hello Chris!
#> You have just won 10000 dollars!
#> Well, 6000 dollars, after taxes.
#> 

# Vectorized over the rows of .data
epoxy_mustache(
  "mpg: {{ mpg }}",
  "hp: {{ hp }}",
  "wt: {{ wt }}\n",
  .data = mtcars[1:2, ]
)
#> mpg: 21
#> hp: 110
#> wt: 2.62
#> 
#> mpg: 21
#> hp: 110
#> wt: 2.875
#> 

# Non-vectorized
epoxy_mustache(
  "mpg: {{ mpg }}",
  "hp: {{ hp }}",
  "wt: {{ wt }}",
  .data = mtcars[1:2, ],
  .vectorized = FALSE
)
#> mpg: 21,21
#> hp: 110,110
#> wt: 2.62,2.875

# With mustache partials
epoxy_mustache(
  "Hello {{name}}!",
  "{{> salutation }}",
  "You have just won {{value}} dollars!",
  "{{#in_ca}}",
  "Well, {{taxed_value}} dollars, after taxes.",
  "{{/in_ca}}",
  .partials = list(
    salutation = c("Hope you are well, {{name}}.")
  ),
  .sep = " ",
  .data = list(
    name = "Chris",
    value = 10000,
    taxed_value = 10000 - (10000 * 0.4),
    in_ca = TRUE
  )
)
#> Hello Chris! Hope you are well, Chris. You have just won 10000 dollars!  Well, 6000 dollars, after taxes. 

```
