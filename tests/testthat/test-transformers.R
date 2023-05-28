test_that("epoxy_transform() works", {
	expect_equal(
		glue(
			"{letters[1:3]}",
			.transformer = epoxy_transform(
				"code",
				epoxy_transform_italic,
				epoxy_transform_bold()
			)
		),
		sprintf("**_`%s`_**", letters[1:3])
	)

	expect_equal(
		glue(
			"{letters[1:3]*}",
			.transformer = epoxy_transform(
				"code",
				epoxy_transform_bold,
				epoxy_transform_collapse(last = ", ... and ... ")
			)
		),
		"**`a`**, **`b`**, ... and ... **`c`**"
	)

	expect_equal(
		glue(
			"{letters[1:3]*}",
			.transformer = epoxy_transform(
				"code",
				epoxy_transform_bold,
				epoxy_transform_collapse(last = ", ... and ... "),
				epoxy_transform_wrap("<< ", " >>")
			)
		),
		"<< **`a`**, **`b`**, ... and ... **`c`** >>"
	)

	expect_equal(
		glue(
			"{letters[1:3]*}",
			.transformer = epoxy_transform(
				"code",
				epoxy_transform_collapse(last = ", ... and ... "),
				epoxy_transform_bold
			)
		),
		"**`a`, `b`, ... and ... `c`**"
	)

	last_and <- ", ... and ... "
	expect_equal(
		glue(
			"{letters[1:3]*}",
			.transformer = epoxy_transform(
				"code",
				epoxy_transform_collapse(last = last_and),
				epoxy_transform_bold
			)
		),
		"**`a`, `b`, ... and ... `c`**"
	)
})

test_that("epoxy_transform() throws an error for unknown transformers", {
	expect_error(
		epoxy_transform("bold", "foo"),
		"doesn't exist"
	)
})

test_that("epoxy_transform_*() chooses engine by epoxy.engine option", {
	with_options(
		list(epoxy.engine = "epoxy"),
		expect_equal(
			glue("{'word'}", .transformer = epoxy_transform("bold")),
			"**word**"
		)
	)

	with_options(
		list(epoxy.engine = "epoxy_html"),
		expect_equal(
			glue("{'word'}", .transformer = epoxy_transform("bold")),
			"<strong>word</strong>"
		)
	)

	with_options(
		list(epoxy.engine = "epoxy_latex"),
		expect_equal(
			glue("{'word'}", .transformer = epoxy_transform("bold")),
			"\\textbf{word}"
		)
	)

	# markdown by default
	with_options(
		list(epoxy.engine = "foo"),
		expect_equal(
			glue("{'word'}", .transformer = epoxy_transform("bold")),
			"**word**"
		)
	)
})

test_that("epoxy_transform_*() functions choose engine by argument", {
	expect_equal(
		glue("{'text'}", .transformer = epoxy_transform_bold(engine = "md")),
		"**text**"
	)

	expect_equal(
		glue("{'text'}", .transformer = epoxy_transform_bold(engine = "markdown")),
		"**text**"
	)

	expect_equal(
		glue("{'text'}", .transformer = epoxy_transform("bold", engine = "markdown")),
		"**text**"
	)

	expect_equal(
		glue("{'text'}", .transformer = epoxy_transform_bold(engine = "html")),
		"<strong>text</strong>"
	)

	expect_equal(
		glue("{'text'}", .transformer = epoxy_transform("bold", engine = "html")),
		"<strong>text</strong>"
	)

	expect_equal(
		glue("{'text'}", .transformer = epoxy_transform_bold(engine = "latex")),
		"\\textbf{text}"
	)

	expect_equal(
		glue("{'text'}", .transformer = epoxy_transform("bold", engine = "latex")),
		"\\textbf{text}"
	)
})

test_that("epoxy_transform_apply()", {
	number <- 1.234234234234

	expect_equal(
		glue(
			"{number} {number}",
			.transformer = epoxy_transform(
				epoxy_transform_apply(~ .x + 10),
				epoxy_transform_apply(round, digits = 2)
			)
		),
		"11.23 11.23"
	)

	expect_equal(
		glue(
			"{word}{missing}",
			word = "here",
			missing = NULL,
			.transformer = epoxy_transform(
				epoxy_transform_apply(~ if (!length(.x)) "" else .x)
			)
		),
		"here"
	)

	scale_pct <- function(x, digits = 2) {
		paste0(round(x * 100, digits), "%")
	}

	expect_equal(
		glue(
			"{x} less than 100% is {y}",
			x = 0.333666,
			y = 0.666666,
			.transformer = epoxy_transform_apply(scale_pct, digits = 0)
		),
		"33% less than 100% is 67%"
	)
})

describe("epoxy_transform_collapse()", {
	it("does nothing by default", {
		expect_equal(
			glue("{1:3}", .transformer = epoxy_transform_collapse()),
			c("1", "2", "3")
		)
	})

	it("collapses with custom separators", {
		expect_equal(
			glue("{1:3*}", .transformer = epoxy_transform_collapse()),
			"1, 2, 3"
		)

		expect_equal(
			glue("{1:3*}", .transformer = epoxy_transform_collapse(" - ")),
			"1 - 2 - 3"
		)

		expect_equal(
			glue("{1:3*}", .transformer = epoxy_transform_collapse(" - ", "... ")),
			"1 - 2... 3"
		)
	})

	it("collapses with and()", {
		expect_equal(
			glue("{1:3&}", .transformer = epoxy_transform_collapse()),
			and::and(1:3)
		)

		expect_equal(
			glue("{1:3&}", .transformer = epoxy_transform_collapse(language = "es")),
			and::and(1:3, language = "es")
		)
	})

	it("collapses with or()", {
		expect_equal(
			glue("{1:3|}", .transformer = epoxy_transform_collapse()),
			and::or(1:3)
		)

		expect_equal(
			glue("{1:3|}", .transformer = epoxy_transform_collapse(language = "es")),
			and::or(1:3, language = "es")
		)
	})

	describe("collapse whitespace edge cases", {
		it("trims whitespace for `and`", {
			expect_equal(
				glue("{1:3& }", .transformer = epoxy_transform_collapse()),
				and::and(1:3)
			)

			expect_equal(
				glue("{1:3 & }", .transformer = epoxy_transform_collapse()),
				and::and(1:3)
			)

			expect_equal(
				glue("{ 1:3 & }", .transformer = epoxy_transform_collapse()),
				and::and(1:3)
			)

			expect_equal(
				glue("{ 1:3& }", .transformer = epoxy_transform_collapse()),
				and::and(1:3)
			)
		})

		it("trims whitespace for `or`", {
			expect_equal(
				glue("{1:3| }", .transformer = epoxy_transform_collapse()),
				and::or(1:3)
			)

			expect_equal(
				glue("{1:3 | }", .transformer = epoxy_transform_collapse()),
				and::or(1:3)
			)

			expect_equal(
				glue("{ 1:3 | }", .transformer = epoxy_transform_collapse()),
				and::or(1:3)
			)

			expect_equal(
				glue("{ 1:3| }", .transformer = epoxy_transform_collapse()),
				and::or(1:3)
			)
		})

		it("trims whitespace for `commas`", {
			expect_equal(
				glue("{1:3* }", .transformer = epoxy_transform_collapse()),
				"1, 2, 3"
			)

			expect_equal(
				glue("{1:3 * }", .transformer = epoxy_transform_collapse()),
				"1, 2, 3"
			)

			expect_equal(
				glue("{ 1:3 * }", .transformer = epoxy_transform_collapse()),
				"1, 2, 3"
			)

			expect_equal(
				glue("{ 1:3* }", .transformer = epoxy_transform_collapse()),
				"1, 2, 3"
			)
		})
	})

	it("chains transformers", {
		expect_equal(
			glue("{1:3&}", .transformer = epoxy_transform_collapse(transformer = epoxy_transform_bold())),
			and::and(glue("**{1:3}**"))
			# "**1**, **2**, and **3**"
		)

		expect_equal(
			glue("{1:3*}", .transformer = epoxy_transform_collapse(transformer = epoxy_transform_bold())),
			"**1**, **2**, **3**"
		)

		expect_equal(
			glue("{1:3&}", .transformer = epoxy_transform_bold(transformer = epoxy_transform_collapse())),
			glue("**{and::and(1:3)}**")
			# "**1, 2, and 3**"
		)

		expect_equal(
			glue("{1:3*}", .transformer = epoxy_transform_bold(transformer = epoxy_transform_collapse())),
			"**1, 2, 3**"
		)
	})
})

describe("epoxy_transform_get()", {
	it("gets the current transform function", {
		opts_md <- epoxy_transform_set("bold", engine = "md")
		on.exit(options(opts_md), add = TRUE)

		opts_html <- epoxy_transform_set("italic", engine = "html")
		on.exit(options(opts_html), add = TRUE)

		opts_latex <- epoxy_transform_set(epoxy_transform_code, engine = "latex")
		on.exit(options(opts_latex), add = TRUE)

		expect_equal(
			epoxy_transform_get("md"),
			epoxy_transform_bold(),
			ignore_function_env = TRUE
		)

		expect_equal(
			epoxy_transform_get("html"),
			epoxy_transform_italic(),
			ignore_function_env = TRUE
		)

		expect_equal(
			epoxy_transform_get("latex"),
			epoxy_transform_code(),
			ignore_function_env = TRUE
		)
	})
})

describe("engine_validate_alias()", {
	it("returns standardized engine name for markdown", {
		expect_equal(engine_validate_alias("markdown"), c(markdown = "md"))
		expect_equal(engine_validate_alias("md"), c(md = "md"))
		expect_equal(engine_validate_alias("glue"), c(glue = "md"))
		expect_equal(engine_validate_alias("epoxy"), c(epoxy = "md"))
	})

	it("returns standardized engine name for html", {
		expect_equal(engine_validate_alias("html"), c(html = "html"))
		expect_equal(engine_validate_alias("glue_html"), c(glue_html = "html"))
		expect_equal(engine_validate_alias("epoxy_html"), c(epoxy_html = "html"))
	})

	it("returns standardized engine name for latex", {
		expect_equal(engine_validate_alias("latex"), c(latex = "latex"))
		expect_equal(engine_validate_alias("glue_latex"), c(glue_latex = "latex"))
		expect_equal(engine_validate_alias("epoxy_latex"), c(epoxy_latex = "latex"))
	})

	it("errors with unknown engine names", {
		expect_snapshot_error(engine_validate_alias("unknown"))
		expect_error(engine_validate_alias("BlubberScript"))
	})
})
