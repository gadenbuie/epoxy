.onLoad <- function(libname, pkgname, ...) {

  if ("knitr" %in% loadedNamespaces()) {
    epoxy_set_knitr_engines()
  }

  setHook(
    packageEvent("knitr", "onLoad"),
    function(...) epoxy_set_knitr_engines()
  )

  invisible()
}
