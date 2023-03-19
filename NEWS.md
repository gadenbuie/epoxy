# epoxy 0.0.2.9000

* epoxy's knitr engine is now `epoxy` and not `glue`. This avoids a name clash
  with the [glue](https://glue.tidyverse.org) package, which provides a `glue`
  knitr engine with similar functionality. epoxy also provides `epoxy_html` and
  `epoxy_latex` knitr engines, although they can still be used via their aliases
  `glue_html` and `glue_latex`. (#21)

* `ui_epoxy_html()` will now render elements with IDs using the `#` syntax, e.g.
  `{{h3#name.author full_name}}` will create an element that is (essentially)
  `<h3 id="name" class="author">{{ full_name }}</h3>` (#22).

* epoxy requires glue >= 1.5.0.

* Added a new internal dataset, `bechdel`, containing details for a random
  sample of ten movies that received a score of **3** on the
  [Bechdel Test](https://bechdeltest.com) (#24).

* epoxy's style transformers can now be chained via `epoxy_style()`. For example
  to use both `epoxy_style_bold()` and `epoxy_style_collapse()` on all
  replacement strings, you can call `epoxy_style("bold", "collapse")`.
  `epoxy_style()` accepts a style function name, e.g. `"collapse"`, the function
  object directly, e.g. `epoxy_style_collapse`, or a call to a style function,
  e.g. `epoxy_style_collapse()` (#26).

* Added a new `vignette("inline-reporting")` with thanks to @tjmahr for the
  [inspiration](https://www.tjmahr.com/lists-knitr-secret-weapon/) (#25).

* The epoxy style transformers for bold, italic and code styles now choose the
  correct syntax for the `epoxy` (markdown), `epoxy_html` and `epoxy_latex`
  engines. Alternatively, you can force the desired syntax by setting the
  `syntax` option (#28).

* epoxy's knitr engines can now be set manually via the newly exported (and
  renamed) `use_epoxy_knitr_engines()`. This function is called when epoxy is
  loaded, so you most likely do not need to call it directly. In general, you
  can instead simply `library(epoxy)`. epoxy previously provided a `glue` chunk
  rather than an `epoxy` chunk and you can restore this behavior by calling
  `use_epoxy_glue_engine()` (#30).

* Added a new chunk option, `epoxy_style`, that takes precedence over the
  `.transformer` chunk option. The new chunk option is best paired with
  `epoxy_style()`, and for convenience you can prove a vector of style names or
  a list of functions, e.g. `epoxy_style = c("bold", "collapse")` (#31).

* A new styler, `epoxy_style_apply()`, can be used to globally apply a
  function to glue expressions. `epoxy_style_apply()` uses the same syntax as 
  `purrr::map()` for defining the function, i.e. `tolower` or `~ tolower(.x)`
  (#37).
  
* `epoxy_style_format()` provides a small inline function, `fmt()` that can be
  used to apply specific formatting to an expression. It wraps all of the
  label functions from the scales package and provides shortcuts for many
  labellers. For example, `{fmt(x, "%")}` will format `x` as a percentage using
  `scales::label_percent()` and `{fmt(x, "$")}` will format `x` as a dollar
  figure. You can also provide your own functions (#39).
  
* By default, `epoxy_style_format()` (#44) and `epoxy_style_collapse()` (#45) 
  are now available in all epoxy chunks. This means you won't need to specify
  the `epoxy_style` chunk option to use the inline `fmt()` function or the
  `*`, `&`, or `|` collapse syntax.
  
* `epoxy_style_collapse()` now uses the [and ackage](https://and.rossellhayes.com/),
  which provides language-aware conjoining of strings. As a result, the
  `sep_and` and `sep_or` arguments of `epoxy_style_collapse()` are deprecated
  and are silently ignored if provided (#45).
  
* `epoxy`, `epoxy_html()` and `epoxy_latex()` are now exported functions that
  power the knitr engines and use the same defaults (#46).
  
* The HTML element syntax used in `ui_epoxy_html()` is now available in
  `epoxy_style_html()` and is used by default in `epoxy_html()` (#46).

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

# epoxy 0.0.2

* Added a `whisker` engine that uses the [whisker](https://github.com/edwindj/whisker)
  package instead of `glue`.

* The chunk option `glue_data` was changed to `data`.

* The `glue_html` engine now uses `{{` and `}}` for open/close delimiters.

* glue/whisker chunks now accept `.envir` chunk option for the containing
  environemnt, otherwise falling back to the `knitr::knit_global()` env.
