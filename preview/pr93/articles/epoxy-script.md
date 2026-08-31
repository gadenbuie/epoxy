# epoxy in R scripts

epoxy isn’t just for reports and Shiny apps! You can use the
[`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy.md)
function just like an `epoxy` knitr chunk.

``` r

movie <- list(
    year = 1989,
    title = "Back to the Future Part II",
    budget = 4e+07
)

epoxy(
    "The movie {.titlecase movie$title}",
    "was released in {movie$year}",
    "and was filmed with a budget of",
    "{.dollar movie$budget}.",
    .sep = "\n"
)
#> The movie Back to the Future Part II
#> was released in 1989
#> and was filmed with a budget of
#> $40,000,000.
```

For HTML and LaTeX contexts, check out
[`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy.md)
and
[`epoxy_latex()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy.md).
These work just like
[`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy.md),
but use convenient defaults for HTML and LaTeX settings.
