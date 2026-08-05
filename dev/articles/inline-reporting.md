# Inline Reporting

This vignette is heavily inspired by [Tristan
Mahr](https://www.tjmahr.com/)’s post [*Lists are my secret weapon for
reporting stats with
knitr*](https://www.tjmahr.com/lists-knitr-secret-weapon/). Please read
his original for an excellent introduction on how to better organize
your data for inline reporting scenarios with lists. I’m going to borrow
several examples directly from that post.

## Plug-in reporting

Both Tristan and Yihui Xie call *inline reporting* the act of
interleaving R expressions in the prose of markdown text. When you click
the **Knit** button or call
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
to build your report, `knitr` evaluates these R expressions, turns them
into text and plugs them into your output.

The most common use case is for reporting descriptive statistics. To
illustrate, I’ll use the [`Orange`
dataset](https://rdrr.io/r/datasets/Orange.html) which contains
circumference measurements of 5 orange trees at 7 points in time.

Here is some R code we might use to summarize the `Orange` data:

``` r

n_trees <- length(levels(Orange$Tree))
n_timepoints <- length(unique(Orange$age))
```

And here are some lines we might include in a report about the growth of
these trees:

```` default
```{r setup, include = FALSE}
library(epoxy)
```

```{epoxy}
The dataset contains {nrow(Orange)} tree size measurements
from {n_trees} trees at {n_timepoints} time points in the study.
```
````

The dataset contains 35 tree size measurements from 5 trees at 7
timepoints in the study.

With normal R Markdown [inline
reporting](https://bookdown.org/yihui/rmarkdown-cookbook/r-code.html) we
would have written this in our `.Rmd` file instead:

``` default
The dataset contains `r nrow(Orange)` tree size measurements
from `r n_trees` trees at `r n_timepoints` time points in the study.
```

The two forms are very similar, but the `epoxy` chunk approach provides
a few advantages, as we’ll discover in this vignette.

## Collect your variables in lists

In the above example, we used normal variables that were available in
the global environment of our document. But a small structural change
can bring great benefits. It’s worth reading [Tristan’s blog
post](https://www.tjmahr.com/lists-knitr-secret-weapon/), but to steal
his thunder: store your data in lists.

We could, on the one hand, create variables named `knitted_when`,
`knitted_where` and `knitted_with` that all store facts about the
knitting process. The `knitted_` prefix is helpful as an aid to remember
that these variables are related.

But you could store those three variables in a single object instead.
Bundling everything into a [`list()`](https://rdrr.io/r/base/list.html)
allows you to report the results by accessing the list elements by name
with `$`.

``` r

knitted <- list(
    when = format(Sys.Date()),
    where = knitr::current_input(),
    with = format(utils::packageVersion("knitr")),
    doc_url = "https://rdrr.io/pkg/knitr/man/knit.html"
)
```

```` default
```{epoxy}
Report prepared on {knitted$when} from `{knitted$where}`
with knitr version {knitted$with} {emo_ji('happy')}.
Read more about [`knitr::knit()`]({knitted$doc_url}).
```
````

Report prepared on 2026-08-05 from `inline-reporting.Rmd` with knitr
version 1.51 😆. Read more about
[`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html).

This is still essentially equivalent to R Markdown’s inline R chunks.
But `epoxy` chunks include a `.data` chunk argument, which allows us to
reference items in the `knitted` list directly without having to use
`$`.

```` default
```{epoxy knitted-2, .data = knitted}
Report prepared on {when} from `{where}`
with knitr version {with} {emo_ji('happy')}.
Read more about [`knitr::knit()`]({doc_url}).
```
````

Report prepared on 2026-08-05 from `inline-reporting.Rmd` with knitr
version 1.51 😆. Read more about
[`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html).

Note that we can still have arbitrary R code in epoxy inline
expressions: the `emo_ji()` function — a vignette-safe version of
`emo::ji()` — exists in my global environment.

## Reporting Model Results

Suppose we have some model results that we’ve prepared into a table (for
details, see [Tristan’s blog
post](https://www.tjmahr.com/lists-knitr-secret-weapon/)). These results
summarize a linear mixed model estimating population averages for trees
grown in several ozone conditions. I’ve copied the resulting data frame
into this vignette to avoid taking extra dependencies for this vignette.

``` r

text_ready <-
    data.frame(
        term = c("intercept", "hund_days", "ozone", "hund_days_ozone"),
        estimate = c("4.25", "0.34", "&minus;0.14", "&minus;0.04"),
        se = c(0.131, 0.013, 0.158, 0.015),
        ci = c("[4.00, 4.51]", "[0.31, 0.36]", "[&minus;0.45, 0.17]","[&minus;0.07, &minus;0.01]"),
        stringsAsFactors = FALSE
    )
```

We can use [`split()`](https://rdrr.io/r/base/split.html) to make a list
of data frames that we can index by the values in the `term` column.

``` r

stats <- split(text_ready, text_ready$term)
```

We now have a list of one-row dataframes:

``` r

str(stats)
#> List of 4
#>  $ hund_days      :'data.frame': 1 obs. of  4 variables:
#>   ..$ term    : chr "hund_days"
#>   ..$ estimate: chr "0.34"
#>   ..$ se      : num 0.013
#>   ..$ ci      : chr "[0.31, 0.36]"
#>  $ hund_days_ozone:'data.frame': 1 obs. of  4 variables:
#>   ..$ term    : chr "hund_days_ozone"
#>   ..$ estimate: chr "&minus;0.04"
#>   ..$ se      : num 0.015
#>   ..$ ci      : chr "[&minus;0.07, &minus;0.01]"
#>  $ intercept      :'data.frame': 1 obs. of  4 variables:
#>   ..$ term    : chr "intercept"
#>   ..$ estimate: chr "4.25"
#>   ..$ se      : num 0.131
#>   ..$ ci      : chr "[4.00, 4.51]"
#>  $ ozone          :'data.frame': 1 obs. of  4 variables:
#>   ..$ term    : chr "ozone"
#>   ..$ estimate: chr "&minus;0.14"
#>   ..$ se      : num 0.158
#>   ..$ ci      : chr "[&minus;0.45, 0.17]"
```

Now we can write up our results with inline reporting:

    ```{epoxy}
    The average log-size in the control condition was
    {stats$intercept$estimate} units,
    95% Wald CI {stats$intercept$ci}.
    There was not a statistically clear difference between the
    ozone conditions for their intercepts (day-0 values),
    *B* = {stats$ozone$estimate}, {stats$ozone$ci}.
    For the control group, the average growth rate was
    {stats$hund_days$estimate} log-size units per 100 days,
    {stats$hund_days$ci}. The growth rate for
    the ozone treatment group was significantly slower,
    *diff* = {stats$hund_days_ozone$estimate},
    {stats$hund_days_ozone$ci}.
    ```

The average log-size in the control condition was 4.25 units, 95% Wald
CI \[4.00, 4.51\]. There was not a statistically clear difference
between the ozone conditions for their intercepts (day-0 values), *B* =
−0.14, \[−0.45, 0.17\]. For the control group, the average growth rate
was 0.34 log-size units per 100 days, \[0.31, 0.36\]. The growth rate
for the ozone treatment group was significantly slower, *diff* = −0.04,
\[−0.07, −0.01\].

### Inline reporting with autocomplete

What’s extra neat about epoxy — and not readily apparent if you’re
reading this vignette — is that RStudio’s autocomplete feature kicks in
when you type `stats$` inside a braced expression
[`{ }`](https://rdrr.io/r/base/Paren.html).

Actually, because the IDE doesn’t know about the `epoxy` knitr engine,
the autocomplete tries to help out on every word. It’s typically easy to
ignore the suggestions for words that are part of the prose, and it’s
usually outweighed by the usefulness of being able to autocomplete the
names in your data structures.

### Intermittent inline-reporting

Note that you don’t need to write your entire document or even paragraph
inside an `epoxy` chunk; you can wrap only the data-heavy parts as
needed.

```` default
There was not a statistically clear difference between the
ozone conditions for their intercepts (day-0 values),
```{epoxy}
*B* = {stats$ozone$estimate}, {stats$ozone$ci}.
```
The growth rate for the ozone treatment group was significantly slower,
```{epoxy}
*diff* = {stats$hund_days_ozone$estimate}, {stats$hund_days_ozone$ci}.
```
````

There was not a statistically clear difference between the ozone
conditions for their intercepts (day-0 values), *B* = −0.14, \[−0.45,
0.17\]. The growth rate for the ozone treatment group was significantly
slower, *diff* = −0.04, \[−0.07, −0.01\].

## Repeated inline reporting

Occasionally you may need to re-use the same phrase or document
structure but for different slices of your data.

### Vectorized inline reporting chunks

Suppose we summarize the orange tree growth (normally I would use a
combination of
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
and
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
here.)

``` r

summarize_tree_growth <- function(tree) {
    tree <- Orange[Orange$Tree == tree, ]
    tree <- data.frame(
        tree = tree$Tree[1],
        age_range = diff(range(tree$age)),
        circumference_first = tree$circumference[1],
        circumference_last = tree$circumference[nrow(tree)]
    )
    tree$growth_rate <- with(tree, (circumference_last - circumference_first) / age_range)
    tree
}

orange_summary <- lapply(1:5, summarize_tree_growth)
orange_summary <- do.call(rbind, orange_summary)
orange_summary
#>   tree age_range circumference_first circumference_last growth_rate
#> 1    1      1464                  30                145  0.07855191
#> 2    2      1464                  33                203  0.11612022
#> 3    3      1464                  30                140  0.07513661
#> 4    4      1464                  32                214  0.12431694
#> 5    5      1464                  30                177  0.10040984
```

`epoxy` chunks, like
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html), are
vectorized, so if we find ourselves needing to repeat the same thing
over and over again, we can use this feature to our advantage.

```` default
A quick recap of the growth observed in the orange trees:

```{epoxy .data = orange_summary}
- Tree number {tree} started out at {circumference_first}mm and,
  over {age_range} days, grew to be {circumference_last}mm.
```
````

A quick recap of the growth observed in the orange trees:

- Tree number 1 started out at 30mm and, over 1464 days, grew to be
  145mm.
- Tree number 2 started out at 33mm and, over 1464 days, grew to be
  203mm.
- Tree number 3 started out at 30mm and, over 1464 days, grew to be
  140mm.
- Tree number 4 started out at 32mm and, over 1464 days, grew to be
  214mm.
- Tree number 5 started out at 30mm and, over 1464 days, grew to be
  177mm.

### Template inline reporting chunks

By using [knitr’s reference labels
feature](https://bookdown.org/yihui/rmarkdown-cookbook/reuse-chunks.html#ref-label),
and the `epoxy` `.data` chunk option we saw above, you can create an
epoxy template that you can re-use like a parameterized chunk.

You start by creating a labelled `epoxy` chunk with `eval = FALSE`

```` default
```{epoxy average-growth, eval=FALSE}
an average of {signif(growth_rate * 7, 2)}mm per week.
```
````

that you can later use in your prose by referencing the chunk with
`ref.label` and providing a different slice of data via the `.data`
chunk option.

```` default
The fourth tree was the largest tree at the end of the study, growing
```{epoxy ref.label="average-growth", .data = summarize_tree_growth(4)}
```
Meanwhile, the smallest tree was the third, which grew at
```{epoxy ref.label="average-growth", .data = summarize_tree_growth(3)}
```
````

The fourth tree was the largest tree at the end of the study, growing an
average of 0.87mm per week. Meanwhile, the smallest tree was the third,
which grew at an average of 0.53mm per week.
