# test_that()

describe("epoxy_style() functions are deprecated", {
	env <- rlang::env(word = "abc")

	it("epoxy_style()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_style("bold")("word", env),
				"**abc**"
			)
		)
	})

	it("epoxy_style_apply()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_style_apply(toupper)("word", env),
				"ABC"
			)
		)
	})

	it("epoxy_style_bold()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_style_bold()("word", env),
				"**abc**"
			)
		)
	})

	it("epoxy_style_code()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_style_code()("word", env),
				"`abc`"
			)
		)
	})

	it("epoxy_style_collapse()", {
		env <- rlang::env(word = c("a", "b", "c"))
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_style_collapse()("word*", env),
				"a, b, c"
			)
		)
	})

	it("epoxy_style_html()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_style_html()("strong word", env),
				html_chr("<strong>abc</strong>")
			)
		)
	})

	it("epoxy_style_inline()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_style_inline()(".code word", env),
				"`abc`"
			)
		)
	})

	it("epoxy_style_italic()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_style_italic()("word", env),
				"_abc_"
			)
		)
	})

	it("epoxy_style_get()", {
		lifecycle::expect_deprecated(epoxy_style_get())
	})

	it("epoxy_style_set()", {
		lifecycle::expect_deprecated(epoxy_style_set())
	})

	it("epoxy_style_wrap()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_style_wrap(before = "-")("word", env),
				"-abc-"
			)
		)
	})
})

describe("deprecated shiny functions", {
	it("epoxyHTML()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxyHTML(.id = "test"),
				ui_epoxy_html(.id = "test")
			)
		)
	})

	it("renderEpoxy()", {
		lifecycle::expect_deprecated(renderEpoxyHTML())
	})
})

describe("engine replaces syntax", {
	it("epoxy_transform()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_transform("bold", syntax = "md"),
				epoxy_transform("bold", engine = "md")
			)
		)
	})

	it("epoxy_transform_set()", {
		lifecycle::expect_deprecated(epoxy_transform_set(syntax = "md"))
	})

	it("epoxy_transform_wrap()", {
		lifecycle::expect_deprecated(
			expect_equal(
				epoxy_transform_wrap(syntax = "md"),
				epoxy_transform_wrap(engine = "md"),
				ignore_function_env = TRUE
			)
		)
	})
})
