# Epoxy Inline Transformer

This epoxy transformer is heavily inspired by the inline formatters in
the [cli package](https://cli.r-lib.org). The syntax is quite similar,
but epoxy's syntax is slightly different to accommodate reporting use
cases.

To transform a template expression inline, you include a keyword,
prefixed with a dot (`.`) that is used to format the value of the
template expression in place.

    epoxy("It cost {.dollar 123456}.", .transformer = "inline")
    #> It cost $123,456.

The formatters, e.g. `.dollar` in the example above, can be customized
using the arguments of `epoxy_transform_inline()`. Pass a customized
[`scales::label_dollar()`](https://scales.r-lib.org/reference/dollar_format.html)
to `.dollar` to achieve a different transformation.

    dollars_nzd <- scales::label_dollar(suffix = " NZD")

    epoxy(
      "It cost {.dollar 123456}.",
      .transformer = epoxy_transform_inline(.dollar = dollars_nzd)
    )
    #> It cost $123,456 NZD.

Note that, unlike [inline markup with
cli](https://cli.r-lib.org/reference/inline-markup.html), the text
within the template expression, other than the keyword, is treated as an
R expression.

    money <- 123456
    epoxy("It cost {.dollar money}.", .transformer = "inline")
    #> It cost $123,456.

You can also nest inline markup expressions.

    money <- c(123.456, 234.567)
    epoxy("It will cost either {.or {.dollar money}}.", .transformer = "inline")
    #> It will cost either $123.46 or $234.57.

Finally, you can provide your own functions that are applied to the
evaluated expression. In this example, I add a `.runif` inline formatter
that generates `n` random numbers (taken from the template expression)
and sorts them.

    set.seed(4242)

    epoxy(
      "Here are three random percentages: {.and {.pct {.runif 3}}}.",
      .transformer = epoxy_transform_inline(
        .runif = function(n) sort(runif(n))
      )
    )
    #> Here are three random percentages: 23%, 35%, and 99%.

## Usage

``` r
epoxy_transform_inline(
  ...,
  transformer = glue::identity_transformer,
  .and = and::and,
  .or = and::or,
  .incr = sort,
  .decr = function(x) sort(x, decreasing = TRUE),
  .bytes = scales::label_bytes(),
  .date = function(x) format(x, format = "%F"),
  .time = function(x) format(x, format = "%T"),
  .datetime = function(x) format(x, format = "%F %T"),
  .dollar = scales::label_dollar(prefix = engine_pick("$", "$", "\\$")),
  .number = scales::label_number(),
  .comma = scales::label_comma(),
  .ordinal = scales::label_ordinal(),
  .percent = scales::label_percent(suffix = engine_pick("%", "%", "\\%")),
  .pvalue = scales::label_pvalue(),
  .scientific = scales::label_scientific(),
  .uppercase = toupper,
  .lowercase = tolower,
  .titlecase = function(x) tools::toTitleCase(as.character(x)),
  .sentence = function(x) `substr<-`(x, 1, 1, toupper(substr(x, 1, 1))),
  .squote = function(x) sQuote(x, q = getOption("epoxy.fancy_quotes", FALSE)),
  .dquote = function(x) dQuote(x, q = getOption("epoxy.fancy_quotes", FALSE)),
  .strong = NULL,
  .emph = NULL,
  .code = NULL
)
```

## Arguments

- ...:

  Additional named inline transformers as functions taking at least one
  argument. The evaluated expression from the template expression is
  passed as the first argument to the function. The argument names must
  include the leading `.` to match the syntax used inline.

- transformer:

  The transformer to apply to the replacement string. This argument is
  used for chaining the transformer functions. By providing a function
  to this argument you can apply an additional transformation after the
  current transformation. In nearly all cases, you can let
  [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md)
  handle this for you. The chain ends when
  [`glue::identity_transformer()`](https://glue.tidyverse.org/reference/identity_transformer.html)
  is used as the `transformer`.

- .and:

  The function to apply to `x` when the template is `{.and x}`. Default
  is
  [`and::and()`](https://pkg.rossellhayes.com/and/reference/and.html).

- .or:

  The function to apply to `x` when the template is `{.or x}`. Default
  is [`and::or()`](https://pkg.rossellhayes.com/and/reference/and.html).

- .incr:

  The function to apply to `x` when the template is `{.incr x}`. Default
  is [`sort()`](https://rdrr.io/r/base/sort.html).

- .decr:

  The function to apply to `x` when the template is `{.decr x}`. Default
  is `function(x) sort(x, decreasing = TRUE)`.

- .bytes:

  The function to apply to `x` when the template is `{.bytes x}`.
  Default is
  [`scales::label_bytes()`](https://scales.r-lib.org/reference/label_bytes.html).

- .date:

  The function to apply to `x` when the template is `{.date x}`. Default
  is `function(x) format(x, format = "%F")`.

- .time:

  The function to apply to `x` when the template is `{.time x}`. Default
  is `function(x) format(x, format = "%T")`.

- .datetime:

  The function to apply to `x` when the template is `{.datetime x}` or
  `{.dttm x}`. Default is `function(x) format(x, format = "%F %T")`.

- .dollar:

  The function to apply to `x` when the template is `{.dollar x}`.
  Default is
  [`scales::label_dollar(prefix = engine_pick("$", "$", "\\$"))`](https://scales.r-lib.org/reference/dollar_format.html).

- .number:

  The function to apply to `x` when the template is `{.number x}` or
  `{.num x}`. Default is
  [`scales::label_number()`](https://scales.r-lib.org/reference/label_number.html).

- .comma:

  The function to apply to `x` when the template is `{.comma x}`.
  Default is
  [`scales::label_comma()`](https://scales.r-lib.org/reference/label_number.html).

- .ordinal:

  The function to apply to `x` when the template is `{.ordinal x}`.
  Default is
  [`scales::label_ordinal()`](https://scales.r-lib.org/reference/label_ordinal.html).

- .percent:

  The function to apply to `x` when the template is `{.percent x}` or
  `{.pct x}`. Default is
  [`scales::label_percent(suffix = engine_pick("%", "%", "\\%"))`](https://scales.r-lib.org/reference/label_percent.html).

- .pvalue:

  The function to apply to `x` when the template is `{.pvalue x}`.
  Default is
  [`scales::label_pvalue()`](https://scales.r-lib.org/reference/label_pvalue.html).

- .scientific:

  The function to apply to `x` when the template is `{.scientific x}`.
  Default is
  [`scales::label_scientific()`](https://scales.r-lib.org/reference/label_scientific.html).

- .uppercase:

  The function to apply to `x` when the template is `{.uppercase x}` or
  `{.uc x}`. Default is
  [`toupper()`](https://rdrr.io/r/base/chartr.html).

- .lowercase:

  The function to apply to `x` when the template is `{.lowercase x}` or
  `{.lc x}`. Default is
  [`tolower()`](https://rdrr.io/r/base/chartr.html).

- .titlecase:

  The function to apply to `x` when the template is `{.titlecase x}` or
  `{.tc x}`. Default is
  `function(x) tools::toTitleCase(as.character(x))`.

- .sentence:

  The function to apply to `x` when the template is `{.sentence x}` or
  `{.sc x}`. Default is
  `` function(x) `substr<-`(x, 1, 1, toupper(substr(x, 1, 1))) ``.

- .squote:

  The function to apply to `x` when the template is `{.squote x}`.
  Default is
  `function(x) sQuote(x, q = getOption("epoxy.fancy_quotes", FALSE))`.

- .dquote:

  The function to apply to `x` when the template is `{.dquote x}`.
  Default is
  `function(x) dQuote(x, q = getOption("epoxy.fancy_quotes", FALSE))`.

- .strong:

  The function to apply to `x` when the template is `{.strong x}` or
  `{.bold x}`. Default is chosen internally based on the output format.

- .emph:

  The function to apply to `x` when the template is `{.emph x}` or
  `{.italic x}`. Default is chosen internally based on the output
  format.

- .code:

  The function to apply to `x` when the template is `{.code x}`. Default
  is chosen internally based on the output format.

## Value

A function of `text` and `envir` suitable for the `.transformer`
argument of
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## See also

[`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md),
[`epoxy_transform_set()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md)

Other epoxy's glue transformers:
[`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md),
[`epoxy_transform_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_html.md)

## Examples

``` r
revenue <- 0.2123
sales <- 42000.134

# ---- Basic Example with Inline Formatting --------------------------------
epoxy(
  '{.pct revenue} of revenue generates {.dollar sales} in profits.'
)
#> 21% of revenue generates $42,000.13 in profits.

# With standard {glue} (`epoxy_transform_inline()` is a glue transformer)
glue::glue(
  '{.pct revenue} of revenue generates {.dollar sales} in profits.',
  .transformer = epoxy_transform_inline()
)
#> 21% of revenue generates $42,000.13 in profits.

# ---- Setting Inline Formatting Options ----------------------------------
# To set inline format options, provide `scales::label_*()` to the supporting
# epoxy_transform_inline arguments.
epoxy(
  '{.pct revenue} of revenue generates {.dollar sales} in profits.',
  .transformer = epoxy_transform_inline(
    .percent = scales::label_percent(accuracy = 0.1),
    .dollar = scales::label_dollar(accuracy = 10)
  )
)
#> 21.2% of revenue generates $42,000 in profits.

glue::glue(
  '{.pct revenue} of revenue generates {.dollar sales} in profits.',
  .transformer = epoxy_transform_inline(
    .percent = scales::label_percent(accuracy = 0.1),
    .dollar = scales::label_dollar(accuracy = 10)
  )
)
#> 21.2% of revenue generates $42,000 in profits.

# ---- Custom Inline Formatting ------------------------------------------
# Add your own formatting functions
search <- "why are cats scared of cucumbers"

epoxy_html(
  "https://example.com?q={{ .url_encode search }}>",
  .transformer = epoxy_transform_inline(
    .url_encode = utils::URLencode
  )
)
#> https://example.com?q=why%20are%20cats%20scared%20of%20cucumbers>
```
