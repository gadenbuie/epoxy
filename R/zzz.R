.onLoad <- function(libname, pkgname, ...) {

  if (isNamespaceLoaded("knitr") && "knit_engines" %in% getNamespaceExports("knitr")) {
    epoxy_set_knitr_engines()
  } else {
    setHook(packageEvent("knitr", "onLoad"), function(...) {
      epoxy_set_knitr_engines()
    })
  }

  invisible()
}
