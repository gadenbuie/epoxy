# Changelog

## epoxy 1.0.1

- Fixed an issue with a test fixture caused by an upstream breaking
  change to get epoxy back on CRAN.

## epoxy 1.0.0

CRAN release: 2023-09-19

### Breaking Changes

- **Breaking change:**
  [`epoxy_latex()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  and the `epoxy_latex` chunk engine it powers now use `<<` and `>>` to
  delimit inline expressions. Where you previously may have used
  `<expr>`, please now use `<<expr>>`. This breaking change was
  necessary to allow the `expr` to include common R operators like `<`
  and `>`. ([\#107](https://github.com/gadenbuie/epoxy/issues/107))

- **Breaking change:** the `whisker` engine is now powered by
  [`epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_mustache.md),
  resulting in a few small changes. In particular, if you previously
  used a list for the `.data` chunk option in a `whisker` chunk, and
  relied on the `whisker` engine’s special treatment of lists to iterate
  over their items, you’ll need to specifically opt into this behavior
  by adding a `.vectorized = TRUE` chunk option.

  This chunk engine still vectorizes over rows in a data frame by
  default, where it’s much more likely to be the behavior you want, but
  bare lists require specifically opting in.
  ([\#103](https://github.com/gadenbuie/epoxy/issues/103))

- `.data` is now the preferred chunk option for passing data frames or
  lists of data to epoxy chunks, where previously `data` was used in
  some places. It works in `whisker` and `epoxy` chunks, and is more
  consistent with the `.data` argument of `glue()` and
  [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md).
  ([\#102](https://github.com/gadenbuie/epoxy/issues/102))

### New Features

- New
  [`epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_mustache.md)
  provides an epoxy-style interface to the
  [mustache](https://mustache.github.io/) templating language, using the
  [whisker](https://cran.r-project.org/package=whisker) package. This
  function also now powers the `whisker` or `mustache` knitr engines.
  `epoxy` also now provides aliases for whisker and mustache in all
  places, so you can use whichever name resonates with you.
  ([\#103](https://github.com/gadenbuie/epoxy/issues/103))

- New
  [`epoxy_use_chunk()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_use.md)
  and
  [`epoxy_use_file()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_use.md)
  allow you to re-use an
  [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md),
  [`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  or other epoxy-provided knitr chunk or file as a template.
  [`epoxy_use_chunk()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_use.md)
  lets you re-use a template chunk with new data, and
  [`epoxy_use_file()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_use.md)
  lets you repeatedly use a template stored in a file. Both functions
  also re-use the options from the template chunk or the options set in
  the YAML from matter of the template file. See
  [`?epoxy_use`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_use.md)
  for more details about how options for both functions are determined.
  ([\#106](https://github.com/gadenbuie/epoxy/issues/106),
  [\#116](https://github.com/gadenbuie/epoxy/issues/116))

### Improvements and bug fixes

- [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  now adds a `.data` pronoun that allows you to refer to the list or
  data frame passed into either the `.data` argument of
  [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  or the `data` or `.data` chunk options.
  ([\#100](https://github.com/gadenbuie/epoxy/issues/100))

- [`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  now supports inline transformations prefixed with `@` instead of `.`,
  e.g. `@strong` instead of `.strong`. Previously, you would have to
  place the inline transformer in a nested `{{ }}` block, e.g.
  `{{ {{ .strong expr }} }}`, but now you only need `{{@strong expr}}`.
  To combine the HTML transformer
  ([`epoxy_transform_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_html.md))
  with the inline transformer, you still need to nest:
  `{{.text-muted {{@strong expr}}}}` creates
  `<span class="text-muted"><strong>{expr}</strong></span>`.
  ([\#120](https://github.com/gadenbuie/epoxy/issues/120))

- [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md),
  and by extension the LaTex and HTML counterparts, and all `epoxy_*`
  knitr engines gain a `.collapse` argument to determine how a vector of
  epoxy-transformed templates should be collapsed. The default is
  `NULL`, which means that the output is returned as a vector. This
  argument is also useful in
  [`epoxy_use_chunk()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_use.md)
  and for knitr chunks being used as a vectorized template.
  ([\#115](https://github.com/gadenbuie/epoxy/issues/115))

- Aded `.sentence` (alias `.sc`) to the list of inline transformers
  provided by
  [`epoxy_transform_inline()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_inline.md).
  This transformer will capitalize the first letter of the first word in
  the expression. This is useful when you want to need to start a
  sentence with a variable that may contain more than one word.
  ([\#112](https://github.com/gadenbuie/epoxy/issues/112))

- The `.titlecase` inline transformer now coerces inputs to character
  with [`as.character()`](https://rdrr.io/r/base/character.html) before
  applying
  [`tools::toTitleCase()`](https://rdrr.io/r/tools/toTitleCase.html),
  since `toTitleCase()` will throw an error for non-character inputs.
  ([\#112](https://github.com/gadenbuie/epoxy/issues/112))

- [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md),
  [`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md),
  [`epoxy_latex()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  and
  [`epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_mustache.md)
  (and their related knitr engines) will all collect remote `tbl_sql`
  tables before evaluation. This makes it much easier to pass data from
  a remote database using [dplyr](https://dplyr.tidyverse.org) and
  [dbplyr](https://dbplyr.tidyverse.org/).
  ([\#117](https://github.com/gadenbuie/epoxy/issues/117))

- Fixed an issue with `epoxy_inline_transform()` when used with custom
  delimiters ([\#116](https://github.com/gadenbuie/epoxy/issues/116)).

## epoxy 0.1.1

CRAN release: 2023-06-11

- [`epoxy_transform_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_html.md)
  now (again) returns a collapsed character string for inline HTML
  transformations. This makes it easier to wrap a vector in individual
  HTML tags and then slot it into a parent template. You can still get
  length-consistent output by setting `collapse = FALSE`, which is most
  useful when you want the template around the expression to repeat.
  ([\#96](https://github.com/gadenbuie/epoxy/issues/96))

- Fixed an issue with
  [`epoxy_transform_set()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md)
  that prevented it from working when
  `knitr::opts_current$get("engine")` returned an unexpected value.

## epoxy 0.1.0

CRAN release: 2023-05-30

This is epoxy’s first release on CRAN! This NEWS file collects changes
from versions that were available on GitHub prior to the CRAN release.

### Breaking Changes

- epoxy’s knitr engine is now `epoxy` and not `glue`. This avoids a name
  clash with the [glue](https://glue.tidyverse.org) package, which
  provides a `glue` knitr engine with similar functionality. epoxy also
  provides `epoxy_html` and `epoxy_latex` knitr engines, although they
  can still be used via their aliases `glue_html` and `glue_latex`.
  ([\#21](https://github.com/gadenbuie/epoxy/issues/21))

- The development version of epoxy included, for some time, two features
  that have changed significantly in this release:

  - The
    [`epoxy_style()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_style.md)
    functions were renamed
    [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md)
    and the `.style` argument was deprecated in favor of the
    `.transformer` argument. This change was made to avoid confusion
    with other meanings of “style”, and for consistency with
    [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).
    The `epoxy_style_` prefix is now `epoxy_transform_`,
    e.g. [`epoxy_transform_bold()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md)
    or
    [`epoxy_transform_collapse()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md).
    ([\#87](https://github.com/gadenbuie/epoxy/issues/87))

  - The previous form of inline formatting – `epoxy_style_format()` –
    has been removed in favor of
    [`epoxy_transform_inline()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_inline.md).
    This new transformer uses cli-style inline formatting and is enabled
    by default in
    [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md),
    [`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
    and
    [`epoxy_latex()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
    ([\#68](https://github.com/gadenbuie/epoxy/issues/68)).

- [`epoxy_transform_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_html.md)
  now returns a character vector rather than a collapsed character
  string when a template expression is a vector.

### New Features

- Added a new internal dataset, `bechdel`, containing details for a
  random sample of ten movies that received a score of **3** on the
  [Bechdel Test](https://bechdeltest.com)
  ([\#24](https://github.com/gadenbuie/epoxy/issues/24)).

- epoxy’s transformers can now be chained via
  [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md).
  For example to use both
  [`epoxy_transform_bold()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md)
  and
  [`epoxy_transform_collapse()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md)
  on all replacement strings, you can call
  `epoxy_transform("bold", "collapse")`.
  [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md)
  accepts a transform function name, e.g. `"collapse"`, the function
  object directly, e.g. `epoxy_transform_collapse`, or a call to a
  transform function,
  e.g. [`epoxy_transform_collapse()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md)
  ([\#26](https://github.com/gadenbuie/epoxy/issues/26)).

- Values passed to the `.transformer` chunk option are now passed first
  to
  [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md),
  allowing you to provide a vector of transformer names or a list of
  functions, e.g. `.transformer = c("bold", "collapse")`. This was
  originally introduced using a separate chunk option `epoxy_style`,
  which is now deprecated
  ([\#31](https://github.com/gadenbuie/epoxy/issues/31),
  [\#87](https://github.com/gadenbuie/epoxy/issues/87)).

- `epoxy`,
  [`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  and
  [`epoxy_latex()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  are now exported functions that power the knitr engines and use the
  same defaults ([\#46](https://github.com/gadenbuie/epoxy/issues/46)).

- The HTML element syntax used in
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  is now available in
  [`epoxy_transform_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_html.md)
  and is used by default in
  [`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  ([\#46](https://github.com/gadenbuie/epoxy/issues/46)).

- The HTML syntax used by
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  and
  [`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  now provides a mechanism for differentiating between HTML-safe and
  -unsafe content. To mark an expression as HTML-safe, use `!!` before
  the variable or expression: e.g. `{{ button !!expr }}`
  ([\#88](https://github.com/gadenbuie/epoxy/issues/88)).

- A new
  [`ui_epoxy_markdown()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_markdown.md)
  function provides a way to create dynamic Shiny UI elements written in
  Markdown. Rendering from markdown to HTML is handled using
  [`pandoc::pandoc_convert()`](https://cderv.github.io/pandoc/reference/pandoc_convert.html)
  if the [pandoc](https://github.com/cderv/pandoc) package is installed,
  or otherwise
  [`commonmark::markdown_html()`](https://docs.ropensci.org/commonmark/reference/commonmark.html)
  is used.

- New
  [`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_mustache.md)
  provides a dynamically rendered Shiny UI element that uses the
  [mustache templating syntax](https://mustache.github.io/). The
  advantage of mustache templating over
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  is that you have dynamic variables in the template can appear anywhere
  in the HTML, not just in the text portion
  ([\#51](https://github.com/gadenbuie/epoxy/issues/51)).

  - [`ui_epoxy_whisker()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_mustache.md)
    is also provided as an alias for discoverability/user comfort
    ([\#60](https://github.com/gadenbuie/epoxy/issues/60)).

- [`epoxyHTML()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  and
  [`renderEpoxyHTML()`](http://pkg.garrickadenbuie.com/epoxy/reference/render_epoxy.md)
  were renamed
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  and
  [`render_epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/render_epoxy.md)
  respectively. This better fits newer Shiny naming conventions and
  reflects that
  [`render_epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/render_epoxy.md)
  serves both
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  and
  [`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_mustache.md)
  ([\#56](https://github.com/gadenbuie/epoxy/issues/56)).

- Added
  [`epoxy_transform_set()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md)
  to enable setting the default `.transformer` option for all chunks or
  epoxy functions. You can use this function to set the epoxy
  transformer for all chunk engines or a subset of chunk engines. Use
  [`epoxy_transform_get()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md)
  to retrieve the epoxy transformer for a particular engine.

  You can also use
  [`epoxy_transform_set()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md)
  to set the inline transformation functions by matching the syntax used
  by
  [`epoxy_transform_inline()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_inline.md).
  See
  [`?epoxy_transform_set`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md)
  for an example.

- [`epoxy_transform_inline()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_inline.md)
  is the default `.transformer` for
  [`epoxy()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  and
  [`epoxy_latex()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  and their related knitr engines.
  [`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)
  now uses `.transformer = epoxy_transform(c("inline", "html"))`.

### Improvements and Bug Fixes

- Added a new
  [`vignette("inline-reporting")`](http://pkg.garrickadenbuie.com/epoxy/articles/inline-reporting.md)
  with thanks to [@tjmahr](https://github.com/tjmahr) for the
  [inspiration](https://www.tjmahr.com/lists-knitr-secret-weapon/)
  ([\#25](https://github.com/gadenbuie/epoxy/issues/25)).

- epoxy requires glue \>= 1.5.0.

- [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  will now render elements with IDs using the `#` syntax, e.g.
  `{{h3#name.author full_name}}` will create an element that is
  (essentially) `<h3 id="name" class="author">{{ full_name }}</h3>`
  ([\#22](https://github.com/gadenbuie/epoxy/issues/22)).

- [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md),
  instead of updating the entire
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  region, now only updates the part of the UI that has actually changed.
  If the template variables are only included in the text portion of the
  template, you should prefer
  [`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_html.md)
  over
  [`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_mustache.md)
  for this reason –
  [`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/reference/ui_epoxy_mustache.md)
  re-renders the entire template with every update.

- The epoxy transformers for bold, italic and code transformations now
  choose the correct syntax for the `epoxy` (markdown), `epoxy_html` and
  `epoxy_latex` engines. Alternatively, you can force the desired syntax
  by setting the `syntax` option
  ([\#28](https://github.com/gadenbuie/epoxy/issues/28)).

- epoxy’s knitr engines can now be set manually via the newly exported
  (and renamed)
  [`use_epoxy_knitr_engines()`](http://pkg.garrickadenbuie.com/epoxy/reference/use_epoxy_knitr_engines.md).
  This function is called when epoxy is loaded, so you most likely do
  not need to call it directly. In general, you can instead simply
  [`library(epoxy)`](https://pkg.garrickadenbuie.com/epoxy/). epoxy
  previously provided a `glue` chunk rather than an `epoxy` chunk and
  you can restore this behavior by calling
  [`use_epoxy_glue_engine()`](http://pkg.garrickadenbuie.com/epoxy/reference/use_epoxy_knitr_engines.md)
  ([\#30](https://github.com/gadenbuie/epoxy/issues/30)).

- A new transformer,
  [`epoxy_transform_apply()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md),
  can be used to globally apply a function to glue expressions.
  [`epoxy_transform_apply()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md)
  uses the same syntax as
  [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) for
  defining the function, i.e. `tolower` or `~ tolower(.x)`
  ([\#37](https://github.com/gadenbuie/epoxy/issues/37)).

- [`epoxy_transform_collapse()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md)
  now uses the [and package](https://github.com/rossellhayes/and/),
  which provides language-aware conjoining of strings. As a result, the
  `sep_and` and `sep_or` arguments of
  [`epoxy_transform_collapse()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_one_shot.md)
  are deprecated and are silently ignored if provided
  ([\#45](https://github.com/gadenbuie/epoxy/issues/45)).

- Added
  [`engine_pick()`](http://pkg.garrickadenbuie.com/epoxy/reference/engine_pick.md)
  for providing a set of options where the correct option will be chosen
  based on the current chunk or epoxy engine.

## epoxy 0.0.2

- Added a `whisker` engine that uses the
  [whisker](https://github.com/edwindj/whisker) package instead of
  `glue`.

- The chunk option `glue_data` was changed to `data`.

- The `glue_html` engine now uses `{{` and `}}` for open/close
  delimiters.

- glue/whisker chunks now accept `.envir` chunk option for the
  containing environment, otherwise falling back to the
  [`knitr::knit_global()`](https://rdrr.io/pkg/knitr/man/knit_global.html)
  env.
