
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epoxy <a href='https://pkg.garrickadenbuie.com/epoxy/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![epoxy r-universe
badge](https://gadenbuie.r-universe.dev/badges/epoxy)](https://gadenbuie.r-universe.dev)
[![R-CMD-check](https://github.com/gadenbuie/epoxy/workflows/R-CMD-check/badge.svg)](https://github.com/gadenbuie/epoxy/actions)
<!-- badges: end -->

epoxy makes templating with [glue](https://glue.tidyverse.org) easy in R
Markdown documents and Shiny apps.

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

<summary>Movie data</summary>

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
earned $118,450,002 with a budget of $40,000,000, and it features movie
stars Michael J. Fox, Christopher Lloyd, Lea Thompson and Thomas F.
Wilson.

</blockquote>

Learn more about `epoxy` chunks – and its siblings `epoxy_html` and
`epoxy_latex` – in [Getting
Started](https://pkg.garrickadenbuie.com/epoxy/articles/epoxy.html). Or
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

## Learn more

There’s a whole lot more that epoxy can do\!

<div class="container">
<div class="row d-grid" style="grid-template-columns: repeat(auto-fill, minmax(325px, 1fr)); gap: 1em;">
<!-- card -->
<div class="col p-0">
<div class="card card-as-link">
<div class="card-body">
<h3 class="card-title mt-0 mb-3">Get Started</h3>
<p class="card-text">Get up and running with epoxy in reports or Shiny apps.</p>
<a href="articles/epoxy.html" class="btn btn-link text-decoration-none float-end">Get started</a>
</div>
</div>
</div>
<!-- card -->
<div class="col p-0">
<div class="card card-as-link">
<div class="card-body">
<h3 class="card-title mt-0 mb-3">Reference</h3>
<p class="card-text">Function reference with usage and examples.</p>
<a href="reference/index.html" class="btn btn-link text-decoration-none float-end">Reference</a>
</div>
</div>
</div>
<!-- card -->
<div class="col p-0">
<div class="card card-as-link">
<div class="card-body">
<h3 class="card-title mt-0 mb-4">Articles</h3>
<p class="card-text">Longer posts and tutorials about using epoxy in your reports and apps.</p>
<a href="articles/" class="btn btn-link text-decoration-none float-end">Articles</a>
</div>
</div>
</div>
</div>
</div>
