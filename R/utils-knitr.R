knitr_current_label <- function() {
	if (isTRUE(knitr::opts_current$get("...inline_chunk"))) {
		return("___inline_chunk___")
	}

	knitr::opts_current$get("label")
}

knitr_chunk_get <- function(label = knitr_current_label()) {
	chunk <- knitr::knit_code$get(label)
	list(
		code = paste(c(chunk), collapse = "\n"),
		opts = knitr_chunk_specific_options(label)
	)
}

knitr_chunk_specific_options <- function(label = knitr_current_label()) {
	if (identical(label, "___inline_chunk___")) {
		return(NULL)
	}

	chunk <- knitr::knit_code$get(label)
	if (is.null(chunk)) return(NULL)

	opts <- attr(chunk, "chunk_opts")

	lapply(opts, function(opt) {
		if (!(rlang::is_symbol(opt) || rlang::is_call(opt))) {
			return(opt)
		}
		rlang::eval_bare(opt, env = knitr::knit_global())
	})
}

# knitr doesn't have a way of detecting when code is being evaluated inside an
# inline code chunk. And worse, the inline chunk "inherits" options from the
# previous chunk -- or at least `opts_current` returns the previous chunk's
# options. This inline chunk detector could probably be built into knitr in some
# way: https://github.com/yihui/knitr/issues/1988
# nocov start
knitr_register_detect_inline <- function() {
	if ("...detect_inline_chunk" %in% knitr::opts_chunk$get()) {
		return()
	}

	# We key off this chunk to always set inline chunk status
	knitr::opts_chunk$set(...detect_inline_chunks = TRUE)

	# Set `...inline_chunk` chunk option to FALSE when entering any
	# regular code chunk, or TRUE when exiting the chunk
	knitr::knit_hooks$set(
		...detect_inline_chunks = knitr_hook_detect_inline_chunk
	)
}
# nocov end

knitr_hook_detect_inline_chunk <- function(before, ...) {
	# Set to FALSE inside a code chunk, reset to TRUE outside
	knitr::opts_current$set(...inline_chunk = !before)
}

knitr_is_inline_chunk <- function() {
	knitr::opts_current$get("...inline_chunk") %||%
		is.null(knitr::opts_current$get("label"))
}
