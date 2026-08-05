# Use the epoxy knitr engines

Sets epoxy's knitr engines for use by knitr in R Markdown and other
document formats powered by knitr. These engines are also set up when
loading epoxy with [`library()`](https://rdrr.io/r/base/library.html),
so in general you will not need to call this function explicitly.

epoxy provides four knitr engines:

- `epoxy` uses default glue syntax, e.g. `{var}` for markdown outputs

- `epoxy_html` uses double brace syntax, e.g. `{{var}}` for HTML outputs

- `epoxy_latex` uses double angle brackets syntax, e.g. `<<var>>` for
  LaTeX outputs

- `whisker` uses the whisker package which provides an R-based
  implementation of the [mustache](https://mustache.github.io/)
  templating language.

For historical reasons, aliases for the HTML and LaTeX engines are also
created: `glue_html` and `glue_latex`. You may opt into a third alias —
`glue` for the `epoxy` engine — by calling `use_epoxy_glue_engine()`,
but note that this will most likely overwrite the `glue` engine provided
by the glue package.

## Usage

``` r
use_epoxy_knitr_engines(
  use_glue_engine = "glue" %in% include,
  include = c("md", "html", "latex", "mustache")
)

use_epoxy_glue_engine()
```

## Arguments

- use_glue_engine:

  If `TRUE` (default `FALSE`), uses epoxy's `glue` engine, most likely
  overwriting the `glue` engine provided by glue.

- include:

  The epoxy knitr engines to include. Defaults to all engines except for
  the `glue` engine (which is just an alias for the `epoxy` engine).

## Value

Silently sets epoxy's knitr engines and invisible returns
[knitr::knit_engines](https://rdrr.io/pkg/knitr/man/knit_engines.html)
as they were prior to the function call.

## Functions

- `use_epoxy_glue_engine()`: Use epoxy's `epoxy` engine as the `glue`
  engine.

## See also

[`epoxy()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy.md),
[`epoxy_html()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy.md),
[`epoxy_latex()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy.md),
and
[`epoxy_mustache()`](http:/pkg.garrickadenbuie.com/epoxy/preview/pr132/reference/epoxy_mustache.md)
for the functions that power these knitr engines.

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
use_epoxy_knitr_engines()
}
```
