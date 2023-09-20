describe("ui_epoxy_html()", {
	example <- "BAD"

	it ("doesn't find things in the global environment", {
		example <- "BADBAD"
		ex <- ui_epoxy_html("test", "{{example}}", example = "GOOD")
		expect_true(grepl("GOOD", format(ex)))
		expect_false(grepl("BAD", format(ex)))

		ex2 <- ui_epoxy_html("test", "{{example}}", example = "GOOD", .placeholder = "NEUTRAL")
		expect_true(grepl("GOOD", format(ex2)))
		expect_false(grepl("NEUTRAL", format(ex2)))
		expect_false(grepl("BAD", format(ex2)))

		ex3 <- ui_epoxy_html("test", "{{example}}", .placeholder = "NEUTRAL")
		expect_true(grepl("NEUTRAL", format(ex3)))
		expect_false(grepl("BAD", format(ex3)))
	})

	it ("works with htmltags, too", {
		ex <- ui_epoxy_html("test", htmltools::tags$p("{{example}}"), example = "GOOD")
		expect_true(grepl("<p>.+GOOD.*</p>", format(ex)))

		ex2 <- ui_epoxy_html("test", "{{p example}}", example = "GOOD")
		expect_true(grepl("<p class=\"epoxy-item__placeholder\".+GOOD.*</p>", format(ex2)))
	})

	it ("works with html in placeholders", {
		ex <- ui_epoxy_html("test", "{{example}}", example="<strong>placeholder</strong>")
		expect_true(grepl("<strong>placeholder</strong>", format(ex), fixed = TRUE))

		ex2 <- ui_epoxy_html("test", "{{example}}", example = htmltools::tags$strong("placeholder"))
		expect_true(grepl("<strong>placeholder</strong>", format(ex2), fixed = TRUE))
	})

	it (".item_container", {
		div_span <- ui_epoxy_html("test", "{{item}}")
		expect_s3_class(div_span, "shiny.tag")
		expect_equal(div_span$name, "epoxy-html")
		expect_true(grepl("^<span", div_span$children[[1]]))

		ul_li <- ui_epoxy_html("test", "{{item}}", .item_tag = "li")
		expect_s3_class(ul_li, "shiny.tag")
		expect_equal(ul_li$name, "epoxy-html")
		expect_true(grepl("^<li", ul_li$children[[1]]))

		ul_li <- ui_epoxy_html("test", "{{li item}}")
		expect_s3_class(ul_li, "shiny.tag")
		expect_equal(ul_li$name, "epoxy-html")
		expect_true(grepl("^<li", ul_li$children[[1]]))
	})

	it("deprecated arguments", {
		lifecycle::expect_deprecated(ui_epoxy_html("test", .container = "foo"))

		lifecycle::expect_deprecated(
			ui_epoxy_html("test", .container_item = "foo")
		)

		lifecycle::expect_deprecated(
			expect_equal(
				ui_epoxy_html("test", "{{item}}", .container_item = "foo"),
				ui_epoxy_html("test", "{{item}}", .item_tag = "foo")
			)
		)

		lifecycle::expect_deprecated(
			ui_epoxy_html("test", .class_item = "foo")
		)
		lifecycle::expect_deprecated(
			expect_equal(
				ui_epoxy_html("test", "{{item}}", .class_item = "foo"),
				ui_epoxy_html("test", "{{item}}", .item_class = "foo")
			)
		)
	})
})
