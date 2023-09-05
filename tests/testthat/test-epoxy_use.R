# test_that()

describe("epoxy_use_chunk()", {

	it("can be called in an R chunk", {
		expect_equal(
			render_rmd(test_path("rmds", "use-chunk_chunk.Rmd")),
			c(
				"one followed by two",
				"",
				"three followed by four"
			)
		)
	})

	it("uses `.data` in the expected order", {
		res <- render_rmd(test_path("rmds", "use-chunk_chunk-opts.Rmd"))

		# uses the chunk option of the epoxy chunk
		expect_equal(res[1], "three followed by four")

		# uses the `.data` argument of `epoxy_use_chunk()`
		expect_equal(res[3], "five followed by six")

		# uses the `.data` argument even if the chunk has a `.data` option
		expect_equal(res[5], "seven followed by eight")

		# uses the `.data` chunk option if the argument isn't provided
		expect_equal(res[7], "nine followed by ten")

		# inline chunks fall back to the template chunks' .data option
		expect_equal(res[9], "three followed by four")
	})

	it("picks the correct default transformer function", {
		epoxy_transform_set(epoxy_transform_bold, engine = "md")
		epoxy_transform_set(epoxy_transform_italic, engine = "html")
		epoxy_transform_set(epoxy_transform_code, engine = "latex")
		on.exit({ epoxy_transform_set(NULL) })

		picked_md <- NULL
		picked_html <- NULL
		picked_latex <- NULL

		render_basic_rmd(
			"```{epoxy md}",
			"{picked_md <- epoxy_default_transformer(); 'ignore'}",
			"```",
			"```{epoxy_html html}",
			"{{ picked_html <- epoxy_default_transformer(); 'ignore' }}",
			"```",
			"```{epoxy_latex latex}",
			"<< picked_latex <- epoxy_default_transformer(); 'ignore' >>",
			"```",
			"```{r}",
			"epoxy_use_chunk(label = 'md')",
			"epoxy_use_chunk(label = 'html')",
			"epoxy_use_chunk(label = 'latex')",
			"```"
		)

		expect_equal(
			picked_md,
			epoxy_transform_bold()
		)
		expect_equal(
			picked_html,
			with_epoxy_engine("html", epoxy_transform_italic())
		)
		expect_equal(
			picked_latex,
			with_epoxy_engine("latex", epoxy_transform_code())
		)
	})

	it("throws for bad labels", {
		expect_error(epoxy_use_chunk(label = 27))
		expect_error(epoxy_use_chunk(label = c("bad", "label")))
		expect_error(epoxy_use_chunk(label = NULL))
	})

	it("throws for unknown labels", {
		render_basic_rmd(
			"```{r}",
			"expect_error(epoxy_use_chunk(label = 'bad-label'))",
			"```"
		)
	})
})

describe("epoxy_use_file()", {
	template <- test_path("rmds", "use-file_example-1.md")
	template <- normalizePath(template)

	data1 <- list(one = "first", two = "second", three = "third")
	data2 <- list(one = "apple", two = "banana", three = "mango")

	it("reads from a file", {
		expect_equal(
			epoxy_use_file(data1, template),
			knitr::asis_output("first then second then third")
		)
		expect_equal(
			epoxy_use_file(data2, template),
			knitr::asis_output("apple then banana then mango")
		)
	})

	it("works inside an Rmd", {
		rmd_res <- render_basic_rmd(
			"```{r echo=FALSE}",
			"epoxy_use_file(data1, template)",
			"```"
		)

		expect_equal(
			rmd_res,
			"first then second then third"
		)

		rmd_res2 <- render_basic_rmd(
			"```{r echo=FALSE, .data = data2}",
			"epoxy_use_file(file = template)",
			"```"
		)

		expect_equal(
			rmd_res2,
			"apple then banana then mango"
		)
	})

	it("allows .data to be defined in the yaml header", {
		template <- test_path("rmds", "use-file_example-2.md")

		expect_equal(
			epoxy_use_file(file = template),
			knitr::asis_output("one fish, two fish, red fish, blue fish")
		)

		expect_equal(
			epoxy_use_file(
				.data = list(one = "one", two = "two", three = "red", four = "blue"),
				file = template
			),
			knitr::asis_output("one, two, red, blue")
		)
	})

	it("sets html engine via engine yaml option", {
		template_html <- test_path("rmds", "use-file_html.md")

		expect_equal(
			epoxy_use_file(
				.data = list(
					link = "https://example.com",
					text = "example link"
				),
				file = template_html
			),
			knitr::asis_output("<a href=\"https://example.com\">example link</a>")
		)
	})

	it("sets latex engine via engine yaml option", {
		template_latex <- test_path("rmds", "use-file_latex.md")

		expect_equal(
			epoxy_use_file(
				.data = list(
					link = "https://example.com",
					text = "example link"
				),
				file = template_latex
			),
			knitr::asis_output("\\href{https://example.com}{example link}")
		)
	})

	it("errors when the file doesn't exist", {
		expect_error(epoxy_use_file(file = "bad-file.md"))
	})
})
