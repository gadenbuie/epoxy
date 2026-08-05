# epoxy in Reports

## Setup

``` r

library(epoxy)
```

Loading epoxy adds four new [knitr
engines](https://pkg.yihui.org/rmarkdown-book/language-engines.html), or
chunk types. Each type lets you intermix text with R code or data
(`expr` in the table below), and each is geared toward a different
output context.

| Engine | Output Context | Delimiter |
|:---|:---|:--:|
| `epoxy` | all-purpose markdown | `{expr}` |
| `epoxy_html` | HTML | `{{expr}}` |
| `epoxy_latex` | LaTeX | `<<expr>>` |
| `whisker` | all-purpose | [mustache template language](https://mustache.github.io/) |

⚠️ **Caution:** Previously, epoxy provided a `glue` engine, but this
conflicts with a similar chunk engine by the
[glue](https://glue.tidyverse.org) package. You can update existing
documents to use the `epoxy` engine, or you can explicitly use epoxy’s
`glue` chunk by including the following in your setup chunk.

``` r

use_epoxy_glue_engine()
```

## Using epoxy chunks

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

`epoxy` is built around
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html), which
evaluates the R expressions in the
[`{ }`](https://rdrr.io/r/base/Paren.html) and inserts the results into
the string. The chunk above is equivalent to this call to
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html):

``` r

glue::glue("The average speed of the cars was **{mean(cars$speed)} mph**.
But on average the distance traveled was only _{mean(cars$dist)} ft_.")
#> The average speed of the cars was **15.4 mph**.
#> But on average the distance traveled was only _42.98 ft_.
```

One immediate advantage of using `epoxy` instead of
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) is that
RStudio’s autocompletion feature works inside `epoxy` chunks! Typing
`cars$` in the chunk will suggest the columns of `cars`.

## Inline transformers

**epoxy** provides inline transformations inspired by [cli’s inline
markup](https://cli.r-lib.org/reference/inline-markup.html). This
transformer is enabled by default in
[`epoxy()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy.md),
[`epoxy_html()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy.md)
and
[`epoxy_latex()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy.md)
and their respective knitr chunk engines.

Here’s an example using a small list containing data about a `movie`
(expand the section below to see the full code for `movie`). We can use
the inline transformer to format the replacement text as we build up a
description from this data.

Movie data

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

> The movie *Back to the Future Part II* was released in **1989**. It
> earned \$118,450,002 with a budget of \$40,000,000, and it features
> movie stars Michael J. Fox, Christopher Lloyd, Lea Thompson, and
> Thomas F. Wilson.

Read more about inline transformations in
[`?epoxy_transform_inline`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_inline.md).

### Transform replaced values

You can use the
[`epoxy_transform_wrap()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_one_shot.md)
with the `epoxy_transform` chunk option to wrap the evaluated R
expression in formatting or templating text. Or you can use the pre-set
[`epoxy_transform_bold()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_one_shot.md),
[`epoxy_transform_italic()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_one_shot.md),
or
[`epoxy_transform_code()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_one_shot.md)
transformers or with
[`epoxy_transform()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform.md).

    ```{epoxy, .transformer = epoxy_transform("bold")}
    All cars stopped between {min(cars$dist)} and {max(cars$dist)} feet
    from a starting speed of {min(cars$speed)}---{max(cars$speed)}
    ```

All cars stopped between **2** and **120** feet from a starting speed of
**4**—**120** mph.

### epoxy chunks are vectorized

Unlike inline R code, the `epoxy` chunks are vectorized. This can be
something to watch out for or it can be an advantage:

    ```{epoxy}
    {1:4}. "{letters[1:4]}" is for {c("apple", "banana", "coconut", "donut")}
    ```

1.  “a” is for apple
2.  “b” is for banana
3.  “c” is for coconut
4.  “d” is for donut

You can collapse fields automatically using the
[`epoxy_transform_collapse()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_one_shot.md)
transformer. You can then choose how vectors are collapsed by adding
`*`, `&` or `|` to the end of the expression.

- `*` collapses with commas, e.g. `{letters[1:3]*}`.
- `&` collapses with commas and adds `" and "` between the last two
  items
- `|` collapses with commas and adds `" or "` between the last two
  items.

&nbsp;

    ```{epoxy, .transformer = epoxy_transform("collapse")}
    - The first three letters are {letters[1:3]*}.
    - When capitalized, they are {LETTERS[1:3]&}.
    - They're indexed by {1:3|}.
    ```

- The first three letters are a, b, c.
- When capitalized, they are A, B, and C.
- They’re indexed by 1, 2, or 3.

You can change the separator between entries and between the last entry
using the `sep`, `last` and the `_and` and `_or` specific arguments of
the
[`epoxy_transform_collapse()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_one_shot.md)
function.

### Templating with epoxy chunks

It’s also possible to create a reusable template. Use the `ref.label`
chunk option to reuse a template using the values in the `.data` chunk
option, which can be a list or data frame.

``` r

mpg <- data.frame(
    manufacturer = c("Chevrolet", "Dodge", "Ford"),
    model = c("Malibu", "Caravan", "Expedition"),
    cty = c(19, 7, 11),
    hwy = c(27, 24, 17)
)
```

    ```{epoxy car-name, eval=FALSE}
    - A {manufacturer} {model} gets {cty} city and {hwy} highway miles per gallon.
    ```

    ```{epoxy ref.label="car-name", .data = mpg}
    ```

- A Chevrolet Malibu gets 19 city and 27 highway miles per gallon.
- A Dodge Caravan gets 7 city and 24 highway miles per gallon.
- A Ford Expedition gets 11 city and 17 highway miles per gallon.

## Whisker engine

Sometimes the `epoxy` engine doesn’t quite deliver the template power
you need. In these cases, you can use the `whisker` engine instead.

    ```{r}
    contestant <- list(name = "R User", value = 1000, taxed = 600, in_ca = TRUE)
    ```

    ```{whisker .data = contestant, echo=FALSE}
    Hello {{name}}:
    You have just won ${{value}}!
    {{#in_ca}}
    Well, ${{taxed}}, after taxes.
    {{/in_ca}}
    ```

``` r

contestant <- list(name = "R User", value = 1000, taxed = 600, in_ca = TRUE)
```

Hello R User: You have just won \$1000! Well, \$600, after taxes.

## HTML and LaTeX chunks

### Markdown chunks

The `epoxy` chunk engine can be used in any output format. In practice,
it works best in markdown (i.e. generally in R Markdown or Quarto)

```` default
```{epoxy, .data = mpg}
- **{manufacturer}** _{model}_
```
````

where it renders as:

    - **Chevrolet** _Malibu_
    - **Dodge** _Caravan_
    - **Ford** _Expedition_

If you’re writing for an HTML or LaTeX output, however, you may need to
write literal HTML or LaTeX in your document. With the `epoxy` chun,
you’d need to escape any `{` or `}` in your text by doubling them,
otherwise the content within will be treated as a template expression.
To avoid this friction, epoxy provides two additional chunk engines,
`epoxy_html` for writing raw HTML and `epoxy_latex` for writing raw
LaTeX.

### Raw HTML chunks

Use the `epoxy_html` block to epoxy (glue) R and HTML together. The
output is [raw HTML](https://pandoc.org/MANUAL.html#raw-htmltex). By
default, expressions in these types of blocks are wrapped in `{{` and
`}}`, like whisker templates above.

```` default
<ul>
```{epoxy_html, .data = mpg}
  <li><strong>{{manufacturer}}</strong> <em>{{model}}</em></li>
```
</ul>
````

    <ul>
    ```{=html}
      <li><strong>Chevrolet</strong> <em>Malibu</em></li>
      <li><strong>Dodge</strong> <em>Caravan</em></li>
      <li><strong>Ford</strong> <em>Expedition</em></li>
    ```
    </ul>

Notice that the output is HTML but wrapped in a [pandoc raw html
block](https://pandoc.org/MANUAL.html#extension-raw_attribute), which
tells pandoc that the content is HTML that shouldn’t be modified[^1]. It
also means that the output of the chunk will [only be included in HTML
documents](https://pkg.yihui.org/rmarkdown-cookbook/raw-content.html).

If your `epoxy_html` block is contained within another a raw html block,
or if you want to force the output to appear, you can set the chunk
option `html_raw = FALSE`.

````` default
````{=html}
<ul>
```{epoxy_html, .data = mpg, html_raw = FALSE}
  <li><strong>{{manufacturer}}</strong> <em>{{model}}</em></li>
```
</ul>
````
`````

`epoxy_html` uses two custom transformers,
[`epoxy_transform_inline()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_inline.md)
and
[`epoxy_transform_html()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_html.md),
applying the html transformer before the inline transformer. With
[`epoxy_transform_html()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_html.md)
you can use `element.class#id` syntax to wrap expressions in HTML
elements (all parts are optional). Let’s use this syntax to place
manufacturer and model in `<strong>` and `<em>` elements, each with a
custom class.

```` default
<ul>
```{epoxy_html, .data = mpg}
  <li>
    {{strong.car-make manufacturer}}
    {{em.car-model model}}
  </li>
```
</ul>
````

    <ul>
    ```{=html}
      <li>
        <strong class="car-make">Chevrolet</strong><strong class="car-make">Dodge</strong><strong class="car-make">Ford</strong>
        <em class="car-model">Malibu</em><em class="car-model">Caravan</em><em class="car-model">Expedition</em>
      </li>
    ```
    </ul>

Because the
[`epoxy_transform_html()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_transform_html.md)
transformer uses `.<class>` to create `<span class="class">` elements,
[`epoxy_html()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy.md)
also recognizes `@<inline>` to access the inline transformers. So
`{{.uppercase manufacturer}}` is assumed to be a CSS class and not an
inline transformer class.

```` default
```{epoxy_html .data = mpg[1,]}
{{.uppercase manufacturer}}

{{@uppercase manufacturer}}
```
````

    ```{=html}
    <span class="uppercase">Chevrolet</span>

    CHEVROLET
    ```

### Raw LaTeX chunks

Similarly, you can also use `epoxy_latex` chunks to epoxy R and LaTeX
together. Wrap expressions in these types of chunks with `<<` and `>>`.

```` default
\begin{itemize}
```{epoxy_latex, .data = mpg}
\item <<.strong manufacturer>> <<.emph model>> gets <<cty>> city and <<hwy>> highway miles per gallon.
```
\end{itemize}
````

In R Markdown knitting into a LaTeX output, this renders as:

    \begin{itemize}
    ```{=latex}
    \item \textbf{Chevrolet} \emph{Malibu} gets 19 city and 27 highway miles per gallon.
    \item \textbf{Dodge} \emph{Caravan} gets 7 city and 24 highway miles per gallon.
    \item \textbf{Ford} \emph{Expedition} gets 11 city and 17 highway miles per gallon.
    ```
    \end{itemize}

Note that, like `epoxy_html` chunks, `epoxy_latex` places the output in
[raw latex
blocks](https://pkg.yihui.org/rmarkdown-cookbook/raw-latex.html). This
behavior can be disabled by setting the chunk option
`latex_raw = FALSE`.

⚠️ **Note:** Prior to v1.0.0, epoxy used single `<` and `>` characters
for expression delimiters in
[`epoxy_latex()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy.md)
chunks. This can lead to subtle but inescapable problems if you need to
use these characters *inside* your expression. As a result,
[`epoxy_latex()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy.md)
now uses `<<` and `>>` to delimit inline expressions.

[^1]: Without the raw html block, pandoc can do unexpected things to
    HTML, even if your output is HTML-friendly. If you are explicitly
    writing HTML markup in your R Markdown or Quarto document, you
    should probably wrap that markup in an `{=html}` block.
