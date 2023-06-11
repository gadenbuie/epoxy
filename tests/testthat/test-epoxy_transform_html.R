# test_that()

describe("epoxy_transform_html()", {
	env <- rlang::env(x = letters[1:3])

	it("returns a character string when given a length-1 vector", {
		expect_equal(
			epoxy_transform_html()("x[1]", env),
			"a"
		)

		expect_equal(
			epoxy_transform_html()("span x[1]", env),
			html_chr("<span>a</span>")
		)

		expect_equal(
			epoxy_transform_html()("span.test-class x[1]", env),
			html_chr('<span class="test-class">a</span>')
		)

		expect_equal(
			epoxy_transform_html()("span.test-class#test-id x[1]", env),
			html_chr('<span class="test-class" id="test-id">a</span>')
		)
	})

	it("collapse = TRUE returns a character string", {
		expect_equal(
			epoxy_transform_html()("x", env),
			letters[1:3]
		)

		expect_equal(
			epoxy_transform_html()("span x", env),
			html_chr("<span>a</span><span>b</span><span>c</span>")
		)

		expect_equal(
			epoxy_transform_html()("span.test-class x", env),
			html_chr('<span class="test-class">a</span><span class="test-class">b</span><span class="test-class">c</span>')
		)

		expect_equal(
			epoxy_transform_html()("span.test-class#test-id x", env),
			html_chr('<span class="test-class" id="test-id">a</span><span class="test-class" id="test-id">b</span><span class="test-class" id="test-id">c</span>')
		)
	})

	it("collapse = FALSE returns a character vector", {
		expect_equal(
			epoxy_transform_html(collapse = FALSE)("x", env),
			letters[1:3]
		)

		expect_equal(
			epoxy_transform_html(collapse = FALSE)("span x", env),
			html_chr(c(
				"<span>a</span>",
				"<span>b</span>",
				"<span>c</span>"
			))
		)

		expect_equal(
			epoxy_transform_html(collapse = FALSE)("span.test-class x", env),
			html_chr(c(
				'<span class="test-class">a</span>',
				'<span class="test-class">b</span>',
				'<span class="test-class">c</span>'
			))
		)

		expect_equal(
			epoxy_transform_html(collapse = FALSE)("span.test-class#test-id x", env),
			html_chr(c(
				'<span class="test-class" id="test-id">a</span>',
				'<span class="test-class" id="test-id">b</span>',
				'<span class="test-class" id="test-id">c</span>'
			))
		)
	})

	it("uses a configurable default element", {
		expect_equal(
			epoxy_transform_html()(".class x[1]", env),
			html_chr('<span class="class">a</span>')
		)

		expect_equal(
			epoxy_transform_html(element = "div")(".class x[1]", env),
			html_chr('<div class="class">a</div>')
		)
	})

	it("adds a configurable default class", {
		expect_equal(
			epoxy_transform_html(class = "special")("mark.other x[1]", env),
			html_chr('<mark class="special other">a</mark>')
		)
	})

	it("escapes inner HTML or not appropriately", {
		env <- rlang::env(x = "<b>bold</b>", y = HTML("<i>italic</i>"))

		expect_equal(
			epoxy_transform_html()("x", env),
			"&lt;b&gt;bold&lt;/b&gt;"
		)

		expect_equal(
			epoxy_transform_html()("mark x", env),
			html_chr("<mark>&lt;b&gt;bold&lt;/b&gt;</mark>")
		)

		expect_equal(
			epoxy_transform_html()("!!x", env),
			"<b>bold</b>"
		)

		expect_equal(
			epoxy_transform_html()("mark !!x", env),
			html_chr("<mark><b>bold</b></mark>")
		)

		expect_equal(
			epoxy_transform_html()("y", env),
			HTML("<i>italic</i>")
		)

		expect_equal(
			epoxy_transform_html()("mark y", env),
			html_chr("<mark><i>italic</i></mark>")
		)
	})
})

describe("parse_html_markup()", {
	it("returns the input as `item` if no markup detected", {
		expect_equal(
			parse_html_markup("word"),
			list(item = "word", as_html = FALSE)
		)

		expect_equal(
			parse_html_markup("& two"),
			list(item = "& two", as_html = FALSE)
		)
	})

	it("returns a list", {
		expect_true(is.list(parse_html_markup("h1 item")))
	})

	it ("returns the item if only item", {
		expect_equal(parse_html_markup("item")$item, "item")
		expect_equal(parse_html_markup(" item")$item, "item")
		expect_equal(parse_html_markup("      item")$item, "item")
		expect_equal(parse_html_markup("item      ")$item, "item")
		expect_equal(parse_html_markup("... item")$item, "item")
	})

	it ("parses html elements", {
		expect_equal(parse_html_markup("h1 item")$element, "h1")
		expect_equal(parse_html_markup("span item")$element, "span")
	})

	it ("parses class into a single string", {
		m <- parse_html_markup(".a.b.c item")
		expect_equal(length(m$class), 1L)
		expect_equal(m$class, "a b c")

		m <- parse_html_markup("h2.a.b.c item")
		expect_equal(length(m$class), 1L)
		expect_equal(m$class, "a b c")
	})

	it ("parses IDs using # or %",{
		m <- parse_html_markup("#id item")
		expect_equal(length(m$id), 1L)
		expect_equal(m$id, "id")

		m <- parse_html_markup("%id item")
		expect_equal(length(m$id), 1L)
		expect_equal(m$id, "id")

		expect_error(parse_html_markup("%one%two item"))
	})

	it("syntax examples", {
		expect_equal(
			parse_html_markup("li item"),
			list(item = "item", element = "li", as_html = FALSE)
		)

		expect_equal(
			parse_html_markup(".class item"),
			list(item = "item", class = "class", as_html = FALSE)
		)

		expect_equal(
			parse_html_markup("#id item"),
			list(item = "item", id = "id", as_html = FALSE)
		)

		expect_equal(
			parse_html_markup("#id.class item"),
			list(item = "item", class = "class", id = "id", as_html = FALSE)
		)

		expect_equal(
			parse_html_markup(".class#id item"),
			list(item = "item", class = "class", id = "id", as_html = FALSE)
		)

		expect_equal(
			parse_html_markup(".one.two#id item"),
			list(item = "item", class = "one two", id = "id", as_html = FALSE)
		)

		expect_equal(
			parse_html_markup("li.one.two#id item"),
			list(
				item = "item",
				element = "li",
				class = "one two",
				id = "id",
				as_html = FALSE
			)
		)
	})

	it("keeps additional spaces", {
		expect_equal(
			parse_html_markup("div list('a', 'b', 'c')"),
			list(
				item = "list('a', 'b', 'c')",
				element = "div",
				as_html = FALSE
			)
		)

		expect_equal(
			parse_html_markup("list('a', 'b', 'c')"),
			list(
				item = "list('a', 'b', 'c')",
				as_html = FALSE
			)
		)

		expect_equal(
			parse_html_markup("div('a', 'b', 'c')"),
			list(
				item = "div('a', 'b', 'c')",
				as_html = FALSE
			)
		)
	})

	it("retains old alternate 'id' syntax with '%'", {
		expect_equal(
			parse_html_markup("%id item"),
			list(item = "item", id = "id", as_html = FALSE)
		)
	})

	it("errors if more than one 'id'", {
		expect_error(
			parse_html_markup("#one#two item")
		)
	})

	it("allows dashes, underscores and numbers in classes and ids", {
		expect_equal(
			parse_html_markup(".this-class x"),
			list(item = "x", class = "this-class", as_html = FALSE),
			list_as_map = TRUE
		)

		expect_equal(
			parse_html_markup("span.this-class x"),
			list(item = "x", element = "span", class = "this-class", as_html = FALSE),
			list_as_map = TRUE
		)

		expect_equal(
			parse_html_markup("#this-id x"),
			list(item = "x", id = "this-id", as_html = FALSE),
			list_as_map = TRUE
		)

		expect_equal(
			parse_html_markup("span#this-id x"),
			list(item = "x", element = "span", id = "this-id", as_html = FALSE),
			list_as_map = TRUE
		)
	})

	it("identifies allowed html in as_html", {
		expect_equal(
			parse_html_markup("!!x"),
			list(item = "x", as_html = TRUE)
		)

		expect_equal(
			parse_html_markup("   !!x"),
			list(item = "x", as_html = TRUE)
		)

		expect_equal(
			parse_html_markup("li !!x"),
			list(item = "x", element = "li", as_html = TRUE)
		)
	})
})
