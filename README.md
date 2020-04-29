
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epoxy

<!-- badges: start -->

<!-- badges: end -->

epoxy makes [glue](https://glue.tidyverse.org) easy to use in R Markdown
documents.

## Installation

You can install the proof-of-concept version of epoxy with
[remotes](https://remotes.r-lib.org).

``` r
# install.packages("remotes")
remotes::install_github("gadenbuie/epoxy")
```

## Example

``` r
library(epoxy)
```

Loading epoxy adds a new chunk type called `glue`.

### glue chunks

You can write regular markdown in the `glue` chunk, wrapping any R
expressions in `{...}`, just like in `glue::glue()`.

    ```{glue}
    The average speed of the cars was **{mean(cars$speed)} mph.**
    But on average the distance travelled was only _{mean(cars$dist)}_.
    ```

The average speed of the cars was **15.4 mph.** But on average the
distance travelled was only *42.98*.

All of the arguments of `glue::glue()` are available as chunk options,
so you can configure the `.open` and `.close` characters, e.g. `{{`
instead of `{`, among other options.

With an amazing stroke of luck, RStudio autocompletion works inside
`glue` chunks\!

### glue chunks are vectorized

Unlike inline R code, the `glue` chunks are vectorized. This can be
something to watch out for or it can be an advantage:

    ```{glue}
    {1:4}. "{letters[1:4]}" is for {c("apple", "banana", "coconut", "donut")}
    ```

1.  “a” is for apple
2.  “b” is for banana
3.  “c” is for coconut
4.  “d” is for donut

### Style replaced values

You can use the `expoxy_style_wrap()` with the `.transformer` chunk
option to wrap the evaluated R expression in formating or templating
text. Or you can use the pre-set `epoxy_style_bold()`,
`epoxy_style_italic()`, or `epoxy_style_code()` style transformers.

    ```{glue, .transformer = epoxy_style_bold()}
    All cars stopped between {min(cars$dist)} and {max(cars$dist)} feet
    from a starting speed of {min(cars$speed)}---{max(cars$speed)}
    ```

All cars stopped between **2** and **120** feet from a starting speed of
**4**—**120** mph.

### Templating with glue chunks

It’s also possible to create a reusable template. Use the `ref.label`
chunk option to reuse a template using the values in the `glue_data`
chunk option, which can be a list or data frame.

``` r
mpg <- data.frame(
  manufacturer = c("Chevrolet", "Dodge", "Ford"),
  model = c("Malibu", "Caravan", "Expedition"),
  cty = c(19, 7, 11),
  hwy = c(27, 24, 17)
)
```

    ```{glue car-name, eval=FALSE}
    - A {manufacturer} {model} gets {cty} city and {hwy} highway miles per gallon.
    ```
    
    ```{glue ref.label="car-name", glue_data = mpg}
    ```

  - A Chevrolet Malibu gets 19 city and 27 highway miles per gallon.
  - A Dodge Caravan gets 7 city and 24 highway miles per gallon.
  - A Ford Expedition gets 11 city and 17 highway miles per gallon.
