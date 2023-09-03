test_that("epoxy .data pronoun", {
	expect_equal(
		epoxy("{.data}", .data = list(a = "hi", b = "there")),
		glue('{list(a = "hi", b = "there")}')
	)

	expect_equal(
		epoxy("{.data$a}", .data = list(a = "hi", b = "there")),
		glue("hi")
	)

	expect_equal(
		epoxy(
			"{.comma unlist(.data[c('b', 'a')])}",
			.data = list(a = "hi", b = "there")
		),
		glue("there, hi")
	)
})

test_that("epoxy(.collapse =)", {
	expect_equal(
		epoxy("{letters[1:3]}", .collapse = ", "),
		"a, b, c"
	)

	expect_equal(
		epoxy_latex("<<letters[1:3]>>", .collapse = " \\middot "),
		"a \\middot b \\middot c"
	)

	expect_equal(
		epoxy_html(
			"{{values}}",
			values = letters[1:3],
			.collapse = "<br>"
		),
		"a<br>b<br>c"
	)

	expect_equal(
		epoxy_html(
			"{{li values}}",
			values = letters[1:3],
			# inline html tranformers pre-collapse
			.collapse = "<br>"
		),
		"<li>a</li><li>b</li><li>c</li>"
	)
})

test_that("epoxy_data_subset()", {
	nested <- list(
		outer = list(
			list(inner = "one"),
			list(inner = "two")
		),
		flat = list(inner = "flat"),
		ragged = list(
			list(inner = c("one", "two")),
			list(inner = "three")
		)
	)

	expect_equal(
		epoxy("{.comma outer$inner}", .data = nested),
		glue("one, two")
	)

	expect_equal(
		epoxy("{flat$inner}", .data = nested),
		glue("flat")
	)

	expect_equal(
		epoxy("{ragged$inner[[1]][2]}", .data = nested),
		glue("two")
	)
})

describe("epoxy_html()", {
	it("returns an html glue character", {
		expect_s3_class(
			epoxy_html("word"),
			c("html", "glue", "character")
		)

		expect_s3_class(
			epoxy_html("{{'word'}}"),
			c("html", "glue", "character")
		)
	})

	it("uses html, inline transformers by default", {
		expect_equal(
			epoxy_html("{{ span letters[1:3] }}"),
			html_chr(glue("<span>a</span><span>b</span><span>c</span>"))
		)

		expect_equal(
			epoxy_html("{{ span {{ .and letters[1:3] }} }}"),
			html_chr(glue("<span>{and::and(letters[1:3])}</span>"))
		)

		# need to escape one level to access inline formatter
		expect_equal(
			epoxy_html("{{ {{.and letters[1:3] }} }}"),
			html_chr(glue(and::and(letters[1:3])))
		)

		# otherwise `.and` is interpreted as a class
		expect_equal(
			epoxy_html("{{.and letters[1] }}"),
			html_chr(glue('<span class="and">a</span>'))
		)
	})
})

describe("epoxy_transform_set()", {
	it("sets the default epoxy_transform for all engines", {
		opts <- epoxy_transform_set("bold")
		on.exit(options(opts))

		expect_equal(
			epoxy("{1} and {2} is {3}"),
			glue("**1** and **2** is **3**")
		)

		expect_equal(
			epoxy("{1} and {2} is {3}"),
			epoxy("{1} and {2} is {3}", .transformer = "bold")
		)

		expect_equal(
			epoxy_html("{{1}} and {{2}} is {{3}}"),
			html_chr(glue(
				"<strong>1</strong> and <strong>2</strong> is <strong>3</strong>"
			))
		)

		expect_equal(
			epoxy_latex("<<1>> and <<2>> is <<3>>"),
			glue("\\textbf{{1}} and \\textbf{{2}} is \\textbf{{3}}")
		)
	})

	it("sets inline formatters for all engines", {
		opts <- epoxy_transform_set(.bold = function(x) "PASS")
		on.exit(epoxy_transform_set(.bold = rlang::zap()))

		expect_equal(
			epoxy("{.bold 'hi'}"),
			glue("PASS")
		)

		expect_equal(
			epoxy_html("{{ {{.bold 'hi'}} }}"),
			glue("PASS")
		)

		expect_equal(
			epoxy_latex("<<.bold 'hi'>>"),
			glue("PASS")
		)
	})

	it("set, get, reset with NULL", {
		expect_equal(
			epoxy_transform_get(inline = TRUE),
			list(md = list(), html = list(), latex = list()),
			ignore_attr = TRUE
		)

		epoxy_transform_set(.bold = function(x) "PASS", engine = "md")
		epoxy_transform_set(.mold = function(x) "PASS", engine = "html")
		epoxy_transform_set(.fold = function(x) "PASS", engine = "latex")

		expect_equal(
			epoxy_transform_get(inline = TRUE),
			list(
				md = list(.bold = function(x) "PASS"),
				html = list(.mold = function(x) "PASS"),
				latex = list(.fold = function(x) "PASS")
			)
		)

		epoxy_transform_set(NULL)
		expect_equal(
			epoxy_transform_get(inline = TRUE),
			list(md = list(), html = list(), latex = list())
		)
	})

	it("accepts a spliced list", {
		opts_list <- list(.bold = function(x) "PASS", "inline", "code")
		opts <- epoxy_transform_set(!!!opts_list, engine = "md")
		on.exit({
			# ensure the global settings are reset
			options(epoxy.transformer_default.md = NULL)
			.globals$inline$md <- list(md = list())
		})

		expect_equal(
			epoxy("{.bold 'hi'}"),
			"`PASS`"
		)
	})

	it("sets the default for engine-specific epoxy_transform defaults", {
		opts_md <- epoxy_transform_set("bold", engine = "md")
		on.exit(options(opts_md), add = TRUE)

		opts_html <- epoxy_transform_set("italic", engine = "html")
		on.exit(options(opts_html), add = TRUE)

		opts_latex <- epoxy_transform_set(epoxy_transform_code, engine = "latex")
		on.exit(options(opts_latex), add = TRUE)

		expect_equal(
			epoxy("{1} and {2} is {3}"),
			glue("**1** and **2** is **3**")
		)

		expect_equal(
			epoxy_html("{{1}} and {{2}} is {{3}}"),
			html_chr(glue(
				"<em>1</em> and <em>2</em> is <em>3</em>"
			))
		)

		expect_equal(
			epoxy_latex("<<1>> and <<2>> is <<3>>"),
			glue("\\texttt{{1}} and \\texttt{{2}} is \\texttt{{3}}")
		)
	})
})

test_that("with_epoxy_engine()", {
	old_opts <- options(epoxy.engine = NULL)
	on.exit(options(old_opts))

	expect_equal(with_epoxy_engine("html", engine_current()), "html")
	expect_null(engine_current())

	expect_equal(with_epoxy_engine("markdown", engine_current()), "md")
	expect_null(engine_current())

	expect_equal(with_epoxy_engine("latex", engine_current()), "latex")
	expect_null(engine_current())
})
