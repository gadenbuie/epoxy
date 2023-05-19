
<!-- pkgdown/index.md is generated from pkgdown/index.Rmd. Please edit that file -->

# epoxy <a href='https://pkg.garrickadenbuie.com/epoxy/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![epoxy r-universe
badge](https://gadenbuie.r-universe.dev/badges/epoxy)](https://gadenbuie.r-universe.dev)
[![R-CMD-check](https://github.com/gadenbuie/epoxy/workflows/R-CMD-check/badge.svg)](https://github.com/gadenbuie/epoxy/actions)
<!-- badges: end -->

<div class="lead">

epoxy makes it easy to use [glue](https://glue.tidyverse.org) in
scripts, reports, and apps.

</div>

## epoxy is super-strength glue

[In R Markdown and Quarto reports](articles/epoxy-report.html)  
Use `epoxy` chunks for super-strength inline syntax. Just
`library(epoxy)` in your [R Markdown](https://rmarkdown.rstudio.com) or
[Quarto](https://quarto.org) document to get started. All epoxy chunks
make it easy to tranform values in place with a `{cli}`-inspired inline
syntax described in `?epoxy_transform_inline`.

[In R scripts](articles/epoxy-script.html)  
The same functions that power epoxy chunks are availble in three
flavors:

  - `epoxy()` for markdown and general purpose outputs

  - `epoxy_html()` for HTML outputs, with added support for HTML
    templating (see `?epoxy_transform_html`)

  - `epoxy_latex()` for LaTeX reports

These functions are accompanied by a robust system for chained
glue-transformers powered by `epoxy_transform()`.

**[In Shiny apps](articles/epoxy-shiny.html)**  
`ui_epoxy_html()` makes it easy to update text or HTML dynamically,
anywhere in your [Shiny](https://shiny.posit.co/) app’s UI. For more
complicated situations, `ui_epoxy_mustache()` lets you turn any Shiny UI
into a template that leverages the [Mustache templating
language](https://mustache.github.io).

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
<a href="articles/epoxy.html" class="card-primary-link btn btn-link text-decoration-none float-end">Get started</a>
</div>
</div>
</div>
<!-- card -->
<div class="col p-0">
<div class="card card-as-link">
<div class="card-body">
<h3 class="card-title mt-0 mb-3">Reference</h3>
<p class="card-text">Function reference with usage and examples.</p>
<a href="reference/index.html" class="card-primary-link btn btn-link text-decoration-none float-end">Reference</a>
</div>
</div>
</div>
<!-- card -->
<div class="col p-0">
<div class="card card-as-link">
<div class="card-body">
<h3 class="card-title mt-0 mb-4">Articles</h3>
<p class="card-text">Longer posts and tutorials about using epoxy in your reports and apps.</p>
<a href="articles/" class="card-primary-link btn btn-link text-decoration-none float-end">Articles</a>
</div>
</div>
</div>
</div>
</div>

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
