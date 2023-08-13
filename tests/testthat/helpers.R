render_rmd <- function(rmd_text, ..., output_format = rmarkdown::md_document()) {
	if (length(rmd_text) == 1 && !grepl("\n", rmd_text)) {
		if (file.exists(rmd_text)) {
			rmd_text <- readLines(rmd_text)
		}
	}

	tmpfile <- tempfile(fileext = ".Rmd")
	on.exit(unlink(tmpfile))
	writeLines(rmd_text, tmpfile)
	out <- rmarkdown::render(tmpfile, output_format = output_format, ..., envir = new.env(), quiet = TRUE)
	if (is.character(out) && file.exists(out)) {
		on.exit(unlink(out), add = TRUE)
		readLines(out)
	} else out
}
