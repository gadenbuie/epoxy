.onLoad <- function(libname, pkgname, ...) {

  if ("knitr" %in% loadedNamespaces()) {
    knitr::knit_engines$set(glue = knitr_engine_glue)
  }

  setHook(
    packageEvent("knitr", "onLoad"),
    function(...) {
      knitr::knit_engines$set(glue = knitr_engine_glue)
    }
  )

  invisible()
}
