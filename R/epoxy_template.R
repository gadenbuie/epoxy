epoxy_template_chunk <- function(.data, label, ...) {
	if (!rlang::is_string(label)) {
		rlang::abort("`label` must be a string")
	}

	if (!label %in% knitr::all_labels()) {
		rlang::abort(paste0('Unknown chunk label "', label, '"'))
	}

	template <- knitr_chunk_get(label)

	# For options, we want to apply options in this order:
	# 0. `.data` from this fn and `eval` from this chunk
	# 1. Options from this function call in the ...
	# 2. Options from the chunk in the template
	# 3. Global knitr options in the current environment
	opts_fn <- rlang::list2(...)
	opts_global <- knitr::opts_current$get()

	opts <- opts_global
	opts <- purrr::list_assign(opts, !!!template$opts)
	opts <- purrr::list_assign(opts, !!!opts_fn)

	opts$eval <- knitr::opts_current$eval
	opts$.data <- .data %||% opts$.data

	fn <- switch(
		template$opts$engine,
		epoxy = epoxy,
		epoxy_html = epoxy_html,
		epoxy_latex = epoxy_latex,
		mustache = epoxy_mustache,
		whisker = epoxy_whisker,
		glue = epoxy_glue,
		glue_html = epoxy_html,
		glue_latex = epoxy_latex,
		epoxy
	)

	call <- rlang::call2(
		"eval_epoxy_engine",
		fn = fn,
		code = template$code,
		options = opts
	)

	eval(call)
}

knitr_chunk_get <- function(label) {
	chunk <- knitr::knit_code$get(label)
	list(
		code = paste(c(chunk), collapse = "\n"),
		opts = knitr_chunk_specific_options(label)
	)
}

knitr_chunk_specific_options <- function(label) {
	chunk <- knitr::knit_code$get(label)
	opts <- attr(chunk, "chunk_opts")

	lapply(opts, function(opt) {
		if (!(rlang::is_symbol(opt) || rlang::is_call(opt))) {
			return(opt)
		}
		rlang::eval_bare(opt, env = knitr::knit_global())
	})
}
