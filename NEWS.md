# epoxy 0.0.2.9000

* epoxy's knitr engine is now `epoxy` and not `glue`. This avoids a name clash
  with the [glue](https://glue.tidyverse.org) package, which provides a `glue`
  knitr engine with similar functionality. epoxy also provides `epoxy_html` and
  `epoxy_latex` knitr engines, although they can still be used via their aliases
  `glue_html` and `glue_latex`. (#21)
  
* `epoxyHTML()` will now render elements with IDs using the `#` syntax, e.g.
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
  objet directly, e.g. `epoxy_style_collapse`, or a call to a style function,
  e.g. `epoxy_style_collapse()` (#26).
  
* Added a new `vignette("inline-reporting")` with thanks to @tjmahr for the
  [inspiration](https://www.tjmahr.com/lists-knitr-secret-weapon/) (#25).
  
* The epoxy style transformers for bold, italic and code styles now choose the
  correct syntax for the `epoxy` (markdown), `epoxy_html` and `epoxy_latex`
  engines. Alternatively, you can force the desired syntax by setting the
  `syntax` option (#28).

# epoxy 0.0.2

* Added a `whisker` engine that uses the [whisker](https://github.com/edwindj/whisker)
  package instead of `glue`.

* The chunk option `glue_data` was changed to `data`.

* The `glue_html` engine now uses `{{` and `}}` for open/close delimiters.

* glue/whisker chunks now accept `.envir` chunk option for the containing
  environemnt, otherwise falling back to the `knitr::knit_global()` env.
