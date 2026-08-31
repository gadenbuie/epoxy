# Reuse a Template Chunk

Reuse a template from another chunk or file. By calling
`epoxy_use_chunk()` in an R chunk or inline R expression, you can reuse
a template defined in another chunk in your document.

Alternatively, you can store the template in a separate file and use
`epoxy_use_file()` to reuse it. When stored in a file, the template file
can contain YAML front matter (following the [same rules as pandoc
documents](https://pandoc.org/MANUAL.html#extension-yaml_metadata_block))
with options that should be applied when calling an epoxy function. The
specific function called by `epoxy_use_file()` can be set via the
`engine` option in the YAML front matter; the default is
[`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/epoxy.md).

## Usage

``` r
epoxy_use_chunk(.data = NULL, label, ...)

epoxy_use_file(.data = NULL, file, ...)
```

## Arguments

- .data:

  A data set

- label:

  The chunk label, i.e. the human-readable name, of the chunk containing
  the template string. This chunk should be an `epoxy`, `epoxy_html` or
  other epoxy-provided chunk type and it must have a label.
  `epoxy_use_chunk()` will apply the options from this chunk to the
  template, giving preference to arguments in `epoxy_use_chunk()` or the
  chunk options where it is called. See the "Template Options" section
  for more details.

- ...:

  Arguments passed on to
  [`epoxy`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/epoxy.md)

  `.transformer`

  :   A transformer function or transformer chain created with
      [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/epoxy_transform.md).
      Alternatively, a character vector of epoxy transformer names, e.g.
      `c("bold", "collapse")` or a list of epoxy transformers, e.g.
      `list(epoxy_transform_bold(), epoxy_transform_collapse())`.

      In epoxy, you'll most likely want to use the defaults or consult
      [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/epoxy_transform.md)
      for more information. See also
      [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
      for more information on transformers.

  `.style`

  :   **\[deprecated\]** Please use `.transformer` instead.

  `.open`

  :   \[`character(1)`: ‘\\’\]  
      The opening delimiter around the template variable or expression.
      Doubling the full delimiter escapes it.

  `.close`

  :   \[`character(1)`: ‘\\’\]  
      The closing delimiter around the template variable or expression.
      Doubling the full delimiter escapes it.

  `.collapse`

  :   A character string used to collapse a vector result into a single
      value. If `NULL` (the default), the result is not collapsed.

  `.sep`

  :   \[`character(1)`: ‘""’\]  
      Separator used to separate elements.

  `.envir`

  :   \[`environment`:
      [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)\]  
      Environment to evaluate each expression in. Expressions are
      evaluated from left to right. If `.x` is an environment, the
      expressions are evaluated in that environment and `.envir` is
      ignored. If `NULL` is passed, it is equivalent to
      [`emptyenv()`](https://rdrr.io/r/base/environment.html).

  `.na`

  :   \[`character(1)`: ‘NA’\]  
      Value to replace `NA` values with. If `NULL` missing values are
      propagated, that is an `NA` result will cause `NA` output.
      Otherwise the value is replaced by the value of `.na`.

  `.null`

  :   \[`character(1)`: ‘character()’\]  
      Value to replace NULL values with. If
      [`character()`](https://rdrr.io/r/base/character.html) whole
      output is [`character()`](https://rdrr.io/r/base/character.html).
      If `NULL` all NULL values are dropped (as in
      [`paste0()`](https://rdrr.io/r/base/paste.html)). Otherwise the
      value is replaced by the value of `.null`.

  `.comment`

  :   \[`character(1)`: ‘#’\]  
      Value to use as the comment character.

  `.literal`

  :   \[`boolean(1)`: ‘FALSE’\]  
      Whether to treat single or double quotes, backticks, and comments
      as regular characters (vs. as syntactic elements), when parsing
      the expression string. Setting `.literal = TRUE` probably only
      makes sense in combination with a custom `.transformer`, as is the
      case with `glue_col()`. Regard this argument (especially, its
      name) as experimental.

  `.trim`

  :   \[`logical(1)`: ‘TRUE’\]  
      Whether to trim the input template with
      [`trim()`](https://glue.tidyverse.org/reference/trim.html) or not.

- file:

  The template file, i.e. a plain text file, containing the template. An
  `.md` or `.txt` file extension is recommended. In addition to the
  template, the file may also contain YAML front matter containing
  options that are used when rendering the template via
  [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/epoxy.md).

## Value

A character string of the rendered template based on the `label`

chunk. The results are marked as `"asis"` output so that they are
treated as regular text rather than being displayed as code results.

## Use in R Markdown or Quarto

    ```{epoxy movie-release}
    {.emph title} was released in {year}.
    ```

    ```{r}
    # Re-using the template we defined above
    epoxy_use_chunk(bechdel[1, ], "movie-release")
    ```

    ```{r}
    # Using in a dplyr pipeline
    bechdel |>
      dplyr::filter(year == 1989) |>
      epoxy_use_chunk("movie-release")
    ```

Or you can even use it inline:

    It's hard to believe that
    `r epoxy_use_chunk(bechdel[2, ], "movie-release")`.

It's hard to believe that *Back to the Future Part II* was released in
1989.

The same template could also be stored in a file, e.g.
`movie-release.md`:

    ---
    engine: epoxy
    ---

    {.emph title} was released in {year}.

The YAML front matter is used in template files to set options for the
template. You can use the `engine` option to choose the epoxy function
to be applied to the template, e.g. `engine: epoxy_html` or
`engine: epoxy_latex`. By default, `engine: epoxy` is assumed unless
otherwise specified.

## Template Options

When rendering a template, `epoxy_use_chunk()` and `epoxy_use_file()`
will inherit the options set in a number of different ways. The final
template options are determined in the following order, ranked by
importance. Options set in a higher-ranked location will override
options set in a lower-ranked location.

1.  The arguments passed to `epoxy_use_chunk()`, such as `.data` or any
    arguments passed in the `...`. These options always have preference
    over options set anywhere else.

2.  The chunk options from the chunk where `epoxy_use_chunk()` or
    `epoxy_use_file()` is called.

3.  The chunk options from the template chunk or file. These options
    typically are relevant to the template itself, such as the engine
    used or the opening and closing delimiters.

4.  Global knitr chunk options for the document. You can set these with
    `knitr::opts_chunk$set()`, see
    [`?knitr::opts_chunk`](https://rdrr.io/pkg/knitr/man/opts_chunk.html)
    for more information.
