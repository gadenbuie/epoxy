# Package index

## epoxy, super-glue wrappers

- [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy.md)
  [`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy.md)
  [`epoxy_latex()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy.md)
  : Epoxy string interpolation
- [`use_epoxy_knitr_engines()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/use_epoxy_knitr_engines.md)
  [`use_epoxy_glue_engine()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/use_epoxy_knitr_engines.md)
  : Use the epoxy knitr engines

## epoxy Transformers

- [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform.md)
  [`epoxy_transform_get()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform.md)
  [`epoxy_transform_set()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform.md)
  : epoxy Transformers
- [`epoxy_transform_inline()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform_inline.md)
  : Epoxy Inline Transformer
- [`epoxy_transform_html()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform_html.md)
  : Concise syntax for expressions inside HTML elements
- [`epoxy_transform_wrap()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform_one_shot.md)
  [`epoxy_transform_bold()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform_one_shot.md)
  [`epoxy_transform_italic()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform_one_shot.md)
  [`epoxy_transform_apply()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform_one_shot.md)
  [`epoxy_transform_code()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform_one_shot.md)
  [`epoxy_transform_collapse()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_transform_one_shot.md)
  : One-shot epoxy transformers
- [`engine_pick()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/engine_pick.md)
  : Pick an engine-specific value

## Reuse epoxy Templates

- [`epoxy_use_chunk()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_use.md)
  [`epoxy_use_file()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_use.md)
  : Reuse a Template Chunk

## Templating for Shiny

- [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/ui_epoxy_html.md)
  [`epoxyHTML()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/ui_epoxy_html.md)
  : Epoxy HTML Output for Shiny
- [`ui_epoxy_markdown()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/ui_epoxy_markdown.md)
  : Epoxy Markdown Template for Shiny
- [`render_epoxy()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/render_epoxy.md)
  [`renderEpoxyHTML()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/render_epoxy.md)
  : Render Epoxy Output
- [`run_epoxy_example_app()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/run_epoxy_example_app.md)
  : Example epoxy Shiny apps

## Mustache-style Templating

Sometimes you need just a little bit more templating power than
[`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy.md)
or [glue](https://glue.tidyverse.org/) can provide. The [mustache
templating language](https://mustache.github.io/) is a simple, popular,
logic-less templating language. Consider using mustache when your
template uses nested data structures or conditionally included content,
but doesn’t require any inline formatting.

- [`epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/epoxy_mustache.md)
  **\[experimental\]** : Mustache-style string interpolation
- [`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/ui_epoxy_mustache.md)
  [`ui_epoxy_whisker()`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/ui_epoxy_mustache.md)
  : Epoxy HTML Mustache Template

## Example Datasets

Interesting datasets that can help you explore, learn, and practice
using epoxy.

- [`bechdel`](http://pkg.garrickadenbuie.com/epoxy/dev/reference/bechdel.md)
  : Top 10 Highest-Rated, Bechdel-Passing Movies
