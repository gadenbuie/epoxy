
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epoxy

<!-- badges: start -->

<!-- badges: end -->

epoxy makes [glue](https://glue.tidyverse.org) easy to use in R Markdown
documents.

*epoxy is in a proof-of-concept stage right now.* *I’m still
experimenting, so the API ~~may~~ will change.* *I’d love to hear your
thoughts, feel free to [open
issues](https://github.com/gadenbuie/epoxy/issues)*.

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

The average speed of the cars was **15.4 mph**. But on average the
distance travelled was only *42.98 ft*.

All of the arguments of `glue::glue()` are available as chunk options,
so you can configure the `.open` and `.close` characters, e.g. `{{`
instead of `{`, among other options.

With an amazing stroke of luck, RStudio autocompletion works inside
`glue` chunks\!

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

You can collapse fields automatically using the `epoxy_style_collapse()`
transformer. You can then choose how vectors are collapsed by adding
`*`, `&` or `|` to the end of the expression.

  - `*` collapses with commas, e.g. `{letters[1:3]*}`.
  - `&` collapses with commas and adds `" and "` between the last two
    items
  - `|` collapses with commas and adds `" or "` between the last two
    items.

<!-- end list -->

    ```{glue, .transformer = epoxy_style_collapse()}
    - The first three letters are {letters[1:3]*}.
    - When capitalized, they are {LETTERS[1:3]&}.
    - They're indexed by {1:3|}.
    ```

  - The first three letters are a, b, c.
  - When capitalized, they are A, B and C.
  - They’re indexed by 1, 2 or 3.

You can change the separator between entries and between the last entry
using the `sep`, `last` and the `_and` and `_or` specific arguments of
the `epoxy_style_collapse()` function.

### Templating with glue chunks

It’s also possible to create a reusable template. Use the `ref.label`
chunk option to reuse a template using the values in the `data` chunk
option, which can be a list or data frame.

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
    
    ```{glue ref.label="car-name", data = mpg}
    ```

  - A Chevrolet Malibu gets 19 city and 27 highway miles per gallon.
  - A Dodge Caravan gets 7 city and 24 highway miles per gallon.
  - A Ford Expedition gets 11 city and 17 highway miles per gallon.

## Whisker Engine

Sometimes the `glue` engine doesn’t quite deliver the template power you
need. In these cases, you can use the `whisker` engine instead.

    ```{r}
    contestant <- list(name = "R User", value = 1000, taxed = 600, in_ca = TRUE)
    ```
    
    ```{whisker data = contestant, echo=FALSE}
    Hello {{name}}:
    You have just won ${{value}}!
    {{#in_ca}}
    Well, ${{taxed}}, after taxes.
    {{/in_ca}}
    ```

``` r
contestant <- list(name = "R User", value = 1000, taxed = 600, in_ca = TRUE)
```

Hello R User: You have just won $1000\! Well, $600, after taxes.

## Raw Blocks

### HTML Blocks

Use the `glue_html` block to glue R and HTML together. The output is
[raw HTML](https://pandoc.org/MANUAL.html#raw-htmltex). By default,
expressions in these types of blocks are wrapped in `{{` and `}}`, like
whisker templates above.

    <ul>
    ```{glue_html, data = mpg}
      <li><strong>{{manufacturer}}</strong> <em>{{model}}</em></li>
    ```
    </ul>

<ul>

  <li><strong>Chevrolet</strong> <em>Malibu</em></li>
  <li><strong>Dodge</strong> <em>Caravan</em></li>
  <li><strong>Ford</strong> <em>Expedition</em></li>

</ul>

### LaTeX Blocks

Similarly, you can also use `glue_latex` blocks to glue R and LaTeX
together. By default, expressions in these types of blocks are wrapped
in `<` and `>`.

    \begin{itemize}
    ```{glue_latex, data = mpg}
    \item \textbf{<manufacturer>} \textit{<model>} gets <cty> city and <hwy> highway miles per gallon.
    ```
    \end{itemize}

In R Markdown knitting into a LaTeX output, this renders as:

    \begin{itemize}
    \item \textbf{Chevrolet} \textit{Malibu} gets 19 city and 27 highway miles per gallon.
    \item \textbf{Dodge} \textit{Caravan} gets 7 city and 24 highway miles per gallon.
    \item \textbf{Ford} \textit{Expedition} gets 11 city and 17 highway miles per gallon.
    \end{itemize}
