
<!-- README.md is generated from README.Rmd. Please edit that file -->
<h1 align="center">
<a href='http://pkg.garrickadenbuie.com/epoxy/'><img src='man/figures/logo.png' align="center" height="250" alt="epoxy logo" /></a><br/>
{epoxy}
</h1>
<p align="center">
<b>extra-strength <a href="https://glue.tidyverse.org">glue</a></b> for
scripts, reports, and apps.
</p>
<p align="center">
<!-- badges: start -->
<a href="https://CRAN.R-project.org/package=epoxy"><img src="https://www.r-pkg.org/badges/version/epoxy" alt="CRAN status" /></a>
<a href="https://gadenbuie.r-universe.dev"><img src="https://gadenbuie.r-universe.dev/badges/epoxy" alt="epoxy r-universe badge" /></a>
<a href="https://github.com/gadenbuie/epoxy/actions"><img src="https://github.com/gadenbuie/epoxy/workflows/R-CMD-check/badge.svg" alt="R-CMD-check" /></a>
<a href="https://app.codecov.io/gh/gadenbuie/epoxy?branch=main"><img src="https://codecov.io/gh/gadenbuie/epoxy/branch/main/graph/badge.svg" alt="Codecov test coverage" /></a>
<a href="https://github.com/gadenbuie/epoxy/blob/main/LICENSE.md" alt="MIT Licensed."><img src="https://img.shields.io/badge/License-MIT-blue.svg" /></a>
<!-- badges: end -->
</p>

## epoxy is super glue

### [In R Markdown and Quarto reports](https://pkg.garrickadenbuie.com/epoxy/articles/epoxy-report.html)

Use `epoxy` chunks for extra-strength inline syntax. Just
`library(epoxy)` in your [R Markdown](https://rmarkdown.rstudio.com) or
[Quarto](https://quarto.org) document to get started. All epoxy chunks
make it easy to transform values in place with a `{cli}`-inspired inline
syntax described in `?epoxy_transform_inline`.

### [In R scripts](https://pkg.garrickadenbuie.com/epoxy/articles/epoxy-script.html)

The same functions that power epoxy chunks are availble in three
flavors:

- `epoxy()` for markdown and general purpose outputs

- `epoxy_html()` for HTML outputs, with added support for HTML
  templating (see `?epoxy_transform_html`)

- `epoxy_latex()` for LaTeX reports

These functions are accompanied by a robust system for chained
glue-transformers powered by `epoxy_transform()`.

### [In Shiny apps](https://pkg.garrickadenbuie.com/epoxy/articles/epoxy-shiny.html)

`ui_epoxy_html()` makes it easy to update text or HTML dynamically,
anywhere in your [Shiny](https://shiny.posit.co/) app’s UI. For more
complicated situations, `ui_epoxy_mustache()` lets you turn any Shiny UI
into a template that leverages the [Mustache templating
language](https://mustache.github.io).

## epoxy in R Markdown and Quarto documents

In [R Markdown](https://rmarkdown.rstudio.com) and
[Quarto](https://quarto.org) documents, **epoxy** gives you an `epoxy`
chunk where you can write in markdown, blending prose and data using
[glue](https://glue.tidyverse.org)’s template syntax.

Here’s an example using a small list containing data about a `movie`
(expand the section below to see the full code for `movie`). We can use
the inline transformer to format the replacement text as we build up a
description from this data.

<details>
<summary>
Movie data
</summary>

``` r
movie <- list(
  year = 1989,
  title = "Back to the Future Part II",
  budget = 4e+07,
  domgross = 118450002,
  imdb_rating = 7.8,
  actors = c(
    "Michael J. Fox",
    "Christopher Lloyd",
    "Lea Thompson",
    "Thomas F. Wilson"
  ),
  runtime = 108L
)
```

</details>

```` default
```{epoxy}
The movie {.emph {.titlecase movie$title}}
was released in {.strong movie$year}.
It earned {.dollar movie$domgross}
with a budget of {.dollar movie$budget},
and it features movie stars
{.and movie$actors}.
```
````

<blockquote>
The movie *Back to the Future Part II* was released in **1989**. It
earned \$118,450,002 with a budget of \$40,000,000, and it features
movie stars Michael J. Fox, Christopher Lloyd, Lea Thompson and Thomas
F. Wilson.
</blockquote>

Learn more about `epoxy` chunks – and its siblings `epoxy_html` and
`epoxy_latex` – in [Getting
Started](https://pkg.garrickadenbuie.com/epoxy//articles/epoxy.html). Or
read more about epoxy’s inline formatting in `?epoxy_transform_inline`.

## Installation

You can install the latest version of epoxy with
[remotes](https://remotes.r-lib.org)

``` r
# install.packages("remotes")
remotes::install_github("gadenbuie/epoxy")
```

or from [gadenbuie.r-universe.dev](https://gadenbuie.r-universe.dev).

``` r
options(repos = c(
  gadenbuie = "https://gadenbuie.r-universe.dev/",
  getOption("repos")
))

install.packages("epoxy")
```

## Setup

``` r
library(epoxy)
```

Loading epoxy adds four new [knitr
engines](https://bookdown.org/yihui/rmarkdown/language-engines.html), or
chunk types. Each type lets you intermix text with R code or data
(`expr` in the table below), and each is geared toward a different
output context.

| Engine        | Output Context       |                         Delimiter                         |
|:--------------|:---------------------|:---------------------------------------------------------:|
| `epoxy`       | all-purpose markdown |                         `{expr}`                          |
| `epoxy_html`  | HTML                 |                        `{{expr}}`                         |
| `epoxy_latex` | LaTeX                |                         `<expr>`                          |
| `whisker`     | all-purpose          | [mustache template language](https://mustache.github.io/) |

⚠️ **Caution:** Previously, epoxy provided a `glue` engine, but this
conflicts with a similar chunk engine by the
[glue](https://glue.tidyverse.org) package. You can update existing
documents to use the `epoxy` engine, or you can explicitly use epoxy’s
`glue` chunk by including the following in your setup chunk.

``` r
use_epoxy_glue_engine()
```

## Use epoxy

To use epoxy in your R Markdown document, create a new chunk using the
engine of your choice. In that chunk, write in markdown, HTML, or LaTeX
as needed, wrapping R expressions inside the delimiters for the epoxy
chunk.

```` default
```{epoxy}
The average speed of the cars was **{mean(cars$speed)} mph.**
But on average the distance traveled was only _{mean(cars$dist)}_.
```
````

The average speed of the cars was **15.4 mph**. But on average the
distance traveled was only *42.98 ft*.

`epoxy` is built around `glue::glue()`, which evaluates the R
expressions in the `{ }` and inserts the results into the string. The
chunk above is equivalent to this call to `glue::glue()`:

``` r
glue::glue("The average speed of the cars was **{mean(cars$speed)} mph**.
But on average the distance traveled was only _{mean(cars$dist)} ft_.")
#> The average speed of the cars was **15.4 mph**.
#> But on average the distance traveled was only _42.98 ft_.
```

One immediate advantage of using `epoxy` instead of `glue::glue()` is
that RStudio’s autocompletion feature works inside `epoxy` chunks!
Typing `cars$` in the chunk will suggest the columns of `cars`.

## Learn more

There’s a whole lot more that epoxy can do! Learn more:

- [epoxy Package Documentation](https://pkg.garrickadenbuie.com/epoxy/)

- [Getting
  Started](https://pkg.garrickadenbuie.com/epoxy//articles/epoxy.html)

- [Inline Reporting with
  epoxy](https://pkg.garrickadenbuie.com/epoxy//articles/inline-reporting.html)

## Code of Conduct

Please note that the epoxy project is released with a [Contributor Code
of Conduct](http://pkg.garrickadenbuie.com/epoxy/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
