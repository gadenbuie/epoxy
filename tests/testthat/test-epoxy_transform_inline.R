# test_that

describe("epoxy_transform_inline()", {
	it("applies transformations in order from outer to inner", {
		expect_equal(
			epoxy("{.strong {.dollar 1234}}"),
			"**$1,234**"
		)

		expect_error(
			epoxy("{.dollar {.strong 1234}}")
		)
	})

	it("applies user-supplied format", {
		expect_equal(
			epoxy(
				"{ .test letters }",
				.transformer = epoxy_transform_inline(
					.test = function(x) "PASS"
				)
			),
			"PASS"
		)
	})

	it("applies user-supplied format over-riding internal alias", {
		expect_equal(
			epoxy(
				"{ .bold letters[1] }",
				.transformer = epoxy_transform_inline(
					.bold = function(x) "PASS"
				)
			),
			"PASS"
		)

		expect_equal(
			epoxy(
				"{ .bold letters[1] }",
				.transformer = epoxy_transform_inline(
					.strong = function(x) "PASS"
				)
			),
			"PASS"
		)

		expect_equal(
			epoxy(
				"{ .strong letters[1] }",
				.transformer = epoxy_transform_inline(
					.bold = function(x) "PASS"
				)
			),
			"**a**"
		)
	})

	it("applies squote and dquote", {
		expect_equal(
			epoxy("{.squote letters[1]}", .transformer = "inline"),
			"'a'"
		)

		expect_equal(
			epoxy("{.dquote letters[1]}", .transformer = "inline"),
			'"a"'
		)

		# from ?sQuote
		opts <- options(epoxy.fancy_quotes = c("\xc2\xab", "\xc2\xbb", "\xc2\xbf", "?"))
		on.exit(options(opts))

		expect_equal(
			epoxy("{.squote letters[1]}", .transformer = "inline"),
			"\xc2\xaba\xc2\xbb"
		)

		expect_equal(
			epoxy("{.dquote letters[1]}", .transformer = "inline"),
			'\xc2\xbfa?'
		)
	})

	it("errors if a non-dotted argument name is provided", {
		expect_snapshot_error(
			epoxy_transform_inline(
				this_thing = function(x) "bad",
				that_thing = function(x) "also bad"
			)
		)
	})

	it("errors if an unnamed argument is provided", {
		expect_snapshot_error(
			epoxy_transform_inline("bad thing")
		)
	})

	it("passes text through if no transformation is found", {
		expect_equal(
			epoxy("{.nope letters[1]}", .transformer = "inline"),
			"a"
		)
	})

	it("returns an error if transformation fails", {
		expect_snapshot_error(
			epoxy(
				"{.blam letters[1]}",
				.transformer = epoxy_transform_inline(
					.blam = function(x) stop("this error is expected in the text")
				)
			)
		)
	})

	it("returns an error if evaluating the text fails", {
		expect_error(
			epoxy(
				"{.blam stop('passed test')}",
				.transformer = epoxy_transform_inline(
					.blam = function(x) stop("this error is not expected in the text")
				)
			),
			"passed test"
		)
	})
})

test_that("epoxy_html() handles internal html at several levels", {
	h <- "<foo>"

	expect_equal(
		format(epoxy_html("{{span {{.strong h }} }}")),
		"<span><strong>&lt;foo&gt;</strong></span>"
	)

	expect_equal(
		format(epoxy_html("{{span h }}")),
		"<span>&lt;foo&gt;</span>"
	)

	expect_equal(
		format(epoxy_html("{{span !!h }}")),
		"<span><foo></span>"
	)
})

describe("epoxy internal inline formatters", {
	it("epoxy_comma() collapses with a comma", {
		expect_equal(
			epoxy("{.comma letters[1:3]}", .transformer = "inline"),
			"a, b, c"
		)
	})

	it("epoxy_bold() emboldens text", {
		expect_equal(
			epoxy("{.strong letters[1]}", .transformer = "inline"),
			"**a**"
		)

		expect_equal(
			epoxy_html("{{.strong letters[1]}}", .transformer = "inline"),
			"<strong>a</strong>"
		)

		expect_equal(
			epoxy_latex("<<.strong letters[1]>>", .transformer = "inline"),
			"\\textbf{a}"
		)
	})

	it("epoxy_italic() italicizes text", {
		expect_equal(
			epoxy("{.italic letters[1]}", .transformer = "inline"),
			"_a_"
		)

		expect_equal(
			epoxy_html("{{.italic letters[1]}}", .transformer = "inline"),
			"<em>a</em>"
		)

		expect_equal(
			epoxy_latex("<<.italic letters[1]>>", .transformer = "inline"),
			"\\emph{a}"
		)
	})

	it("epoxy_code() formats text as code", {
		expect_equal(
			epoxy("{.code letters[1]}", .transformer = "inline"),
			"`a`"
		)

		expect_equal(
			epoxy_html("{{.code letters[1]}}", .transformer = "inline"),
			"<code>a</code>"
		)

		expect_equal(
			epoxy_latex("<<.code letters[1]>>", .transformer = "inline"),
			"\\texttt{a}"
		)
	})
})
