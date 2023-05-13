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
					test = function(x) "PASS"
				)
			),
			"PASS"
		)
	})

	it("applies user-supplied format when it's an internal alias", {
		expect_equal(
			epoxy(
				"{ .bold letters[1] }",
				.transformer = epoxy_transform_inline(
					bold = function(x) "PASS"
				)
			),
			"PASS"
		)

		expect_equal(
			epoxy(
				"{ .strong letters[1] }",
				.transformer = epoxy_transform_inline(
					bold = function(x) "PASS"
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
