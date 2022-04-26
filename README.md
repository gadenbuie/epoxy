
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epoxy <a href='http://pkg.garrickadenbuie.com/epoxy'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![epoxy r-universe
badge](https://gadenbuie.r-universe.dev/badges/epoxy)](https://gadenbuie.r-universe.dev)
[![R-CMD-check](https://github.com/gadenbuie/epoxy/workflows/R-CMD-check/badge.svg)](https://github.com/gadenbuie/epoxy/actions)
<!-- badges: end -->

epoxy makes templating with [glue](https://glue.tidyverse.org) easy in R
Markdown documents and Shiny apps.

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
  getOptions("repos")
))

install.packages("epoxy")
```

## Setup

``` r
library(epoxy)
```

Loading epoxy adds four new knitr engines, or chunk types. Each type
lets you intermix text with R code or data, and each is geared toward a
different output context:

  - `epoxy` is an all-purpose glue chunk and uses `{ }` for delimiters
  - `epoxy_html` is a glue chunk for HTML and uses `{{ }}` for
    delimiters
  - `epoxy_latex` is a glue chunk for LaTeX and uses `< >` for
    delimiters
  - `whisker` is an all-purpose chunk for the [mustache template
    language](https://mustache.github.io/)

⚠️ **Caution:** Previously, epoxy provided a `glue` chunk, but this
clashes with a chunk engine provided by the
[glue](https://glue.tidyverse.org) package. If you wish to restore use
epoxy’s `glue` chunk, you can include the following in your setup chunk.

``` r
use_epoxy_glue_engine()
```

## Use epoxy

```` default
```{r}
# Subset `airquality` dataset to May, 1973
airquality_may <- airquality[airquality$Month == 5, ]
```

```{epoxy}
The average temperature was {signif(mean(airquality_may$Temp))}
at LaGuardia Airport in New York in May of 1973.
```
````

When rendered, epoxy fills in the `{ }` for you:

    #> > The average temperature was 65.5484
    #> at LaGuardia Airport in New York in May of 1973.

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

`epoxy` is built around `glue::glue()`, so the chunk above is equivalent
to the following.

``` r
glue::glue("The average speed of the cars was **{mean(cars$speed)} mph**.
But on average the distance traveled was only _{mean(cars$dist)} ft_.")
#> The average speed of the cars was **15.4 mph**.
#> But on average the distance traveled was only _42.98 ft_.
```

One immediate advantage of using `epoxy` instead of `glue::glue()` is
that RStudio’s autocompletion feature works inside `epoxy` chunks\!
Typing `cars$` in the chunk will suggest the columns of `cars`.

## Learn more

There’s a whole lot more that epoxy can do\! Learn more:

  - [epoxy Package Documentation](https://pkg.garrickadenbuie.com/epoxy)

  - [Getting
    Started](https://pkg.garrickadenbuie.com/epoxy/articles/epoxy.html)

  - [Inline Reporting with
    epoxy](https://pkg.garrickadenbuie.com/epoxy/articles/inline-reporting.html)
