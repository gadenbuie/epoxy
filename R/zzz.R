.onLoad <- function(libname, pkgname, ...) {
	if (isNamespaceLoaded("knitr") && "knit_engines" %in% getNamespaceExports("knitr")) {
		use_epoxy_knitr_engines()
	} else {
		setHook(packageEvent("knitr", "onLoad"), function(...) {
			use_epoxy_knitr_engines()
		})
	}

	if (requireNamespace("debugme", quietly = TRUE)) {
		debugme::debugme()
	}

	invisible()
}
