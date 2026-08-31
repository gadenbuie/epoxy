# Pick an engine-specific value

Set different values that will be used based on the current epoxy or
knitr engine (one of `md`, `html`, or `latex`). The engine-specific
value will be used inside epoxy knitr chunks or epoxy functions matching
the source syntax:
[`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy.md)
(`md`),
[`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy.md)
(`html`), or
[`epoxy_latex()`](http://pkg.garrickadenbuie.com/epoxy/preview/pr93/reference/epoxy.md)
(`latex`).

## Usage

``` r
engine_pick(md, html = md, latex = md)
```

## Arguments

- md, html, latex:

  The value to use in a markdown, HTML, or LaTeX context.

## Value

The value of `md`, `html` or `latex` depending on the epoxy or knitr
currently being evaluated.

## Examples

``` r
# Markdown and HTML are okay with bare `$` character,
# but we need to escape it in LaTeX.
engine_pick(md = "$", latex = "\\$")
#> [1] "$"
```
