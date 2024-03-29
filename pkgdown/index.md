
<!-- pkgdown/index.md is generated from pkgdown/index.Rmd. Please edit that file -->

# epoxy <a href='https://pkg.garrickadenbuie.com/epoxy/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

<a href="https://CRAN.R-project.org/package=epoxy"><img src="https://www.r-pkg.org/badges/version/epoxy" alt="CRAN status" /></a>
<a href="https://gadenbuie.r-universe.dev"><img src="https://gadenbuie.r-universe.dev/badges/epoxy" alt="epoxy r-universe badge" /></a>
<a href="https://github.com/gadenbuie/epoxy/actions"><img src="https://github.com/gadenbuie/epoxy/workflows/R-CMD-check/badge.svg" alt="R-CMD-check" /></a>
<a href="https://app.codecov.io/gh/gadenbuie/epoxy?branch=main"><img src="https://codecov.io/gh/gadenbuie/epoxy/branch/main/graph/badge.svg" alt="Codecov test coverage" /></a>
<a href="https://github.com/gadenbuie/epoxy/blob/main/LICENSE.md" alt="MIT Licensed."><img src="https://img.shields.io/badge/License-MIT-blue.svg" /></a>
<!-- badges: end -->

<div class="lead">

Extra-strength <a href="https://glue.tidyverse.org">glue</a></b> for
scripts, reports, and apps

</div>

## epoxy is super glue

### [In R Markdown and Quarto reports](articles/epoxy-report.html)

Use `epoxy` chunks for extra-strength inline syntax. Just
`library(epoxy)` in your [R Markdown](https://rmarkdown.rstudio.com) or
[Quarto](https://quarto.org) document to get started. All epoxy chunks
make it easy to transform values in place with a `{cli}`-inspired inline
syntax described in `?epoxy_transform_inline`.

### [In R scripts](articles/epoxy-script.html)

The same functions that power epoxy chunks are availble in three
flavors:

- `epoxy()` for markdown and general purpose outputs

- `epoxy_html()` for HTML outputs, with added support for HTML
  templating (see `?epoxy_transform_html`)

- `epoxy_latex()` for LaTeX reports

These functions are accompanied by a robust system for chained
glue-transformers powered by `epoxy_transform()`.

### [In Shiny apps](articles/epoxy-shiny.html)

`ui_epoxy_html()` makes it easy to update text or HTML dynamically,
anywhere in your [Shiny](https://shiny.posit.co/) app’s UI. For more
complicated situations, `ui_epoxy_mustache()` lets you turn any Shiny UI
into a template that leverages the [Mustache templating
language](https://mustache.github.io).

## Learn more

There’s a whole lot more that epoxy can do!

<div class="container">
<div class="row d-grid" style="grid-template-columns: repeat(auto-fill, minmax(325px, 1fr)); gap: 1em;">
<!-- card -->
<div class="col p-0">
<div class="card card-as-link">
<div class="card-body">
<h3 class="card-title mt-0 mb-3"><a href="articles/epoxy.html" class="card-primary-link card-header-link text-decoration-none">Get started</a></h3>
<p class="card-text">Get up and running with epoxy in reports or Shiny apps.</p>
</div>
</div>
</div>
<!-- card -->
<div class="col p-0">
<div class="card card-as-link">
<div class="card-body">
<h3 class="card-title mt-0 mb-3"><a href="reference/index.html" class="card-primary-link card-header-link text-decoration-none">Reference</a></h3>
<p class="card-text">Function reference with usage and examples.</p>
</div>
</div>
</div>
<!-- card -->
<div class="col p-0">
<div class="card card-as-link">
<div class="card-body">
<h3 class="card-title mt-0 mb-4"><a href="articles/" class="card-primary-link card-header-link text-decoration-none">Articles</a></h3>
<p class="card-text">Longer posts and tutorials about using epoxy in your reports and apps.</p>
</div>
</div>
</div>
</div>
</div>

## Installation

You can install epoxy from CRAN:

``` r
install.packages("epoxy")
```

You can install the latest development version of epoxy with
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
