# epoxy 0.1.0

This is epoxy's first release on CRAN! This NEWS file collects changes from
versions that were available on GitHub prior to the CRAN release.

## Breaking Changes

* epoxy's knitr engine is now `epoxy` and not `glue`. This avoids a name clash
  with the [glue](https://glue.tidyverse.org) package, which provides a `glue`
  knitr engine with similar functionality. epoxy also provides `epoxy_html` and
  `epoxy_latex` knitr engines, although they can still be used via their aliases
  `glue_html` and `glue_latex`. (#21)

* The development version of epoxy included, for some time, two features that
  have changed significantly in this release:

  * The `epoxy_style()` functions were renamed `epoxy_transform()` and the
    `.style` argument was deprecated in favor of the `.transformer` argument.
    This change was made to avoid confusion with other meanings of "style", and
    for consistency with `glue::glue()`. The `epoxy_style_` prefix is now
    `epoxy_transform_`, e.g. `epoxy_transform_bold()` or
    `epoxy_transform_collapse()`. (#87)

  * The previous form of inline formatting -- `epoxy_style_format()` --
    has been removed in favor of `epoxy_transform_inline()`. This new
    transformer uses cli-style inline formatting and is enabled by default in
    `epoxy()`, `epoxy_html()` and `epoxy_latex()` (#68).

* `epoxy_transform_html()` now returns a character vector rather than a
  collapsed character string when a template expression is a vector.

## New Features

* Added a new internal dataset, `bechdel`, containing details for a random
  sample of ten movies that received a score of **3** on the
  [Bechdel Test](https://bechdeltest.com) (#24).

* epoxy's transformers can now be chained via `epoxy_transform()`. For
  example to use both `epoxy_transform_bold()` and `epoxy_transform_collapse()`
  on all replacement strings, you can call `epoxy_transform("bold",
  "collapse")`. `epoxy_transform()` accepts a transform function name, e.g.
  `"collapse"`, the function object directly, e.g. `epoxy_transform_collapse`,
  or a call to a transform function, e.g. `epoxy_transform_collapse()` (#26).

* Values passed to the `.transformer` chunk option are now passed first to
  `epoxy_transform()`, allowing you to provide a vector of transformer names or
  a list of functions, e.g. `.transformer = c("bold", "collapse")`. This was
  originally introduced using a separate chunk option `epoxy_style`, which is
  now deprecated (#31, #87).

* `epoxy`, `epoxy_html()` and `epoxy_latex()` are now exported functions that
  power the knitr engines and use the same defaults (#46).

* The HTML element syntax used in `ui_epoxy_html()` is now available in
  `epoxy_transform_html()` and is used by default in `epoxy_html()` (#46).

* The HTML syntax used by `ui_epoxy_html()` and `epoxy_html()` now provides a
  mechanism for differentiating between HTML-safe and -unsafe content. To mark
  an expression as HTML-safe, use `!!` before the variable or expression:
  e.g. `{{ button !!expr }}` (#88).

* New `ui_epoxy_mustache()` provides a dynamically rendered Shiny UI element
  that uses the [mustache templating syntax](https://mustache.github.io/). The
  advantage of mustache templating over `ui_epoxy_html()` is that you have dynamic
  variables in the template can appear anywhere in the HTML, not just in the
  text portion (#51).

  * `ui_epoxy_whisker()` is also provided as an alias for discoverability/user
    comfort (#60).

* `epoxyHTML()` and `renderEpoxyHTML()` were renamed `ui_epoxy_html()` and
  `render_epoxy()` respectively. This better fits newer Shiny naming conventions
  and reflects that `render_epoxy()` serves both `ui_epoxy_html()` and
  `ui_epoxy_mustache()` (#56).

* Added `epoxy_transform_set()` to enable setting the default `.transformer`
  option for all chunks or epoxy functions. You can use this function to set the
  epoxy transformer for all chunk engines or a subset of chunk engines. Use
  `epoxy_transform_get()` to retrieve the epoxy transformer for a particular
  engine.

* `epoxy_transform_inline()` is the default `.transformer` for `epoxy()` and
  `epoxy_latex()` and their related knitr engines. `epoxy_html()` now uses
  `.transformer = epoxy_transform(c("inline", "html"))`.

## Improvements and Bug Fixes

* Added a new `vignette("inline-reporting")` with thanks to @tjmahr for the
  [inspiration](https://www.tjmahr.com/lists-knitr-secret-weapon/) (#25).

* epoxy requires glue >= 1.5.0.

* `ui_epoxy_html()` will now render elements with IDs using the `#` syntax, e.g.
  `{{h3#name.author full_name}}` will create an element that is (essentially)
  `<h3 id="name" class="author">{{ full_name }}</h3>` (#22).

* `ui_epoxy_html()`, instead of updating the entire `ui_epoxy_html()` region,
  now only updates the part of the UI that has actually changed. If the template
  variables are only included in the text portion of the template, you should
  prefer `ui_epoxy_html()` over `ui_epoxy_mustache()` for this reason --
  `ui_epoxy_mustache()` re-renders the entire template with every update.

* The epoxy transformers for bold, italic and code transformations now choose
  the correct syntax for the `epoxy` (markdown), `epoxy_html` and `epoxy_latex`
  engines. Alternatively, you can force the desired syntax by setting the
  `syntax` option (#28).

* epoxy's knitr engines can now be set manually via the newly exported (and
  renamed) `use_epoxy_knitr_engines()`. This function is called when epoxy is
  loaded, so you most likely do not need to call it directly. In general, you
  can instead simply `library(epoxy)`. epoxy previously provided a `glue` chunk
  rather than an `epoxy` chunk and you can restore this behavior by calling
  `use_epoxy_glue_engine()` (#30).

* A new transformer, `epoxy_transform_apply()`, can be used to globally apply a
  function to glue expressions. `epoxy_transform_apply()` uses the same syntax
  as `purrr::map()` for defining the function, i.e. `tolower` or `~ tolower(.x)`
  (#37).

* `epoxy_transform_collapse()` now uses the
  [and package](https://and.rossellhayes.com/), which provides language-aware
  conjoining of strings. As a result, the `sep_and` and `sep_or` arguments of
  `epoxy_transform_collapse()` are deprecated and are silently ignored if
  provided (#45).

* Added `engine_pick()` for providing a set of options where the correct option
  will be chosen based on the current chunk or epoxy engine.


# epoxy 0.0.2

* Added a `whisker` engine that uses the [whisker](https://github.com/edwindj/whisker)
  package instead of `glue`.

* The chunk option `glue_data` was changed to `data`.

* The `glue_html` engine now uses `{{` and `}}` for open/close delimiters.

* glue/whisker chunks now accept `.envir` chunk option for the containing
  environemnt, otherwise falling back to the `knitr::knit_global()` env.
