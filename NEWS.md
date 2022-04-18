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

# epoxy 0.0.2

* Added a `whisker` engine that uses the [whisker](https://github.com/edwindj/whisker)
  package instead of `glue`.

* The chunk option `glue_data` was changed to `data`.

* The `glue_html` engine now uses `{{` and `}}` for open/close delimiters.

* glue/whisker chunks now accept `.envir` chunk option for the containing
  environemnt, otherwise falling back to the `knitr::knit_global()` env.