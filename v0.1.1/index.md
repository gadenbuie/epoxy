# epoxy

Extra-strength [glue](https://glue.tidyverse.org) for scripts, reports,
and apps

## epoxy is super glue

### [In R Markdown and Quarto reports](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/articles/epoxy-report.md)

Use `epoxy` chunks for extra-strength inline syntax. Just
[`library(epoxy)`](https://pkg.garrickadenbuie.com/epoxy/) in your [R
Markdown](https://rmarkdown.rstudio.com) or [Quarto](https://quarto.org)
document to get started. All epoxy chunks make it easy to transform
values in place with a [cli](https://cli.r-lib.org)-inspired inline
syntax described in
[`?epoxy_transform_inline`](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/reference/epoxy_transform_inline.md).

### [In R scripts](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/articles/epoxy-script.md)

The same functions that power epoxy chunks are availble in three
flavors:

- [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/reference/epoxy.md)
  for markdown and general purpose outputs

- [`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/reference/epoxy.md)
  for HTML outputs, with added support for HTML templating (see
  [`?epoxy_transform_html`](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/reference/epoxy_transform_html.md))

- [`epoxy_latex()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/reference/epoxy.md)
  for LaTeX reports

These functions are accompanied by a robust system for chained
glue-transformers powered by
[`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/reference/epoxy_transform.md).

### [In Shiny apps](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/articles/epoxy-shiny.md)

[`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/reference/ui_epoxy_html.md)
makes it easy to update text or HTML dynamically, anywhere in your
[Shiny](https://shiny.posit.co/) app’s UI. For more complicated
situations,
[`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/reference/ui_epoxy_mustache.md)
lets you turn any Shiny UI into a template that leverages the [Mustache
templating language](https://mustache.github.io).

## Learn more

There’s a whole lot more that epoxy can do!

### [Get started](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/articles/epoxy.md)

Get up and running with epoxy in reports or Shiny apps.

### [Reference](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/reference/index.md)

Function reference with usage and examples.

### [Articles](http://pkg.garrickadenbuie.com/epoxy/v0.1.1/articles/)

Longer posts and tutorials about using epoxy in your reports and apps.

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
