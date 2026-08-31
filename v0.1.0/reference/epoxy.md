# Epoxy string interpolation

These functions power the knitr chunk engines and are wrappers around
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html), with a
few extra conveniences provided by epoxy.

- `epoxy()` is super
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `epoxy_html()` is super
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) with
  HTML-specific defaults.

- `epoxy_latex()` is super
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) with
  LaTeX-specific defaults.

Each of these functions can be called directly or used as a knitr chunk
engine where the chunk text is handled as if it were a string passed
into the function version. When used as a knitr chunk engine, the
function arguments can be passed in as chunk options.

All of `epoxy()`, `epoxy_html()` and `epoxy_latex()` use
[`epoxy_transform_inline()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/epoxy_transform_inline.md)
by default. This transformer brings a concise inline-formatting syntax
that you can read more about in
[`?epoxy_transform_inline`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/epoxy_transform_inline.md).

`epoxy_html()` also includes an inline transformation syntax that makes
it easier to wrap the expression text in HTML elements with a specific
ID or a set of classes. Learn more about this syntax in
[`?epoxy_transform_html`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/epoxy_transform_html.md).

## Usage

``` r
epoxy(
  ...,
  .data = NULL,
  .sep = "",
  .envir = parent.frame(),
  .open = "{",
  .close = "}",
  .na = "",
  .null = "",
  .comment = character(),
  .literal = FALSE,
  .trim = FALSE,
  .transformer = NULL,
  .style = lifecycle::deprecated()
)

epoxy_html(
  ...,
  .data = NULL,
  .sep = "",
  .envir = parent.frame(),
  .open = "{{",
  .close = "}}",
  .na = "",
  .null = "",
  .comment = "",
  .literal = FALSE,
  .trim = FALSE,
  .transformer = NULL
)

epoxy_latex(
  ...,
  .data = NULL,
  .sep = "",
  .envir = parent.frame(),
  .open = "<",
  .close = ">",
  .na = "",
  .null = "",
  .comment = "",
  .literal = FALSE,
  .trim = FALSE,
  .transformer = NULL
)
```

## Arguments

- ...:

  \[`expressions`\]  
  Unnamed arguments are taken to be expression string(s) to format.
  Multiple inputs are concatenated together before formatting. Named
  arguments are taken to be temporary variables available for
  substitution.

- .data:

  A data set

- .sep:

  \[`character(1)`: ‘""’\]  
  Separator used to separate elements.

- .envir:

  \[`environment`:
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)\]  
  Environment to evaluate each expression in. Expressions are evaluated
  from left to right. If `.x` is an environment, the expressions are
  evaluated in that environment and `.envir` is ignored. If `NULL` is
  passed, it is equivalent to
  [`emptyenv()`](https://rdrr.io/r/base/environment.html).

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

- .comment:

  \[`character(1)`: ‘#’\]  
  Value to use as the comment character.

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

- .transformer:

  A transformer function or transformer chain created with
  [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/epoxy_transform.md).
  Alternatively, a character vector of epoxy transformer names, e.g.
  `c("bold", "collapse")` or a list of epoxy transformers, e.g.
  `list(epoxy_transform_bold(), epoxy_transform_collapse())`.

  In epoxy, you'll most likely want to use the defaults or consult
  [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.0/reference/epoxy_transform.md)
  for more information. See also
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
  more information on transformers.

- .style:

  **\[deprecated\]** Please use `.transformer` instead.

## Value

Returns a transformed string, using
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) but
with the additional transformers provided to the `.transformer` argument
of `epoxy()`.

## Examples

``` r
movie <- bechdel[1, ]
movies <- bechdel[2:4, ]

epoxy("*{movie$title}* ({movie$year}) was directed by {movie$director}.")
#> *Inception* (2010) was directed by Christopher Nolan.
epoxy("- *{movies$title}* ({movies$year}) was directed by {movies$director}.")
#> - *Back to the Future Part II* (1989) was directed by Robert Zemeckis.
#> - *The Simpsons Movie* (2007) was directed by David Silverman.
#> - *Another Year* (2010) was directed by Mike Leigh.

epoxy("*{title}* ({year}) was directed by {director}.", .data = movie)
#> *Inception* (2010) was directed by Christopher Nolan.
epoxy("- *{title}* ({year}) was directed by {director}.", .data = movies)
#> - *Back to the Future Part II* (1989) was directed by Robert Zemeckis.
#> - *The Simpsons Movie* (2007) was directed by David Silverman.
#> - *Another Year* (2010) was directed by Mike Leigh.

epoxy(
  "{title} ({year}) was directed by {director}.",
  .data = movie,
  .transformer = "bold"
)
#> **Inception** (**2010**) was directed by **Christopher Nolan**.

epoxy(
  "I'd be happy to watch { title| }.",
  .data = movies,
  .transformer = c("italic", "collapse")
)
#> I'd be happy to watch _Back to the Future Part II_, _The Simpsons Movie_, or _Another Year_.

epoxy(
  "They were directed by { director& }.",
  .data = movies,
  .transformer = c("collapse", "bold")
)
#> They were directed by **Robert Zemeckis, David Silverman, and Mike Leigh**.

epoxy("The budget for *{title}* was {.dollar budget}.", .data = movie)
#> The budget for *Inception* was $160,000,000.

epoxy_html(
  "I'd be happy to watch {{ title| }}.",
  .data = movies,
  .transformer = c("italic", "collapse")
)
#> I'd be happy to watch <em>Back to the Future Part II</em>, <em>The Simpsons Movie</em>, or <em>Another Year</em>.

epoxy_latex(
  "I'd be happy to watch < title| >.",
  .data = movies,
  .transformer = c("italic", "collapse")
)
#> I'd be happy to watch \emph{Back to the Future Part II}, \emph{The Simpsons Movie}, or \emph{Another Year}.
```
