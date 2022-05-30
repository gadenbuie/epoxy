# test_that()

describe("epoxy_style_html()", {
  env <- rlang::env(x = letters[1:3])

  it("returns a character string when given a length-1 vector", {
    expect_equal(
      epoxy_style_html()("x[1]", env),
      "a"
    )

    expect_equal(
      epoxy_style_html()("span x[1]", env),
      html_chr("<span>a</span>")
    )

    expect_equal(
      epoxy_style_html()("span.test-class x[1]", env),
      html_chr('<span class="test-class">a</span>')
    )

    expect_equal(
      epoxy_style_html()("span.test-class#test-id x[1]", env),
      html_chr('<span class="test-class" id="test-id">a</span>')
    )
  })

  it("returns a character string when given a length+1 vector", {
    expect_equal(
      epoxy_style_html()("x", env),
      letters[1:3]
    )

    expect_equal(
      epoxy_style_html()("span x", env),
      html_chr("<span>a</span><span>b</span><span>c</span>")
    )

    expect_equal(
      epoxy_style_html()("span.test-class x", env),
      html_chr('<span class="test-class">a</span><span class="test-class">b</span><span class="test-class">c</span>')
    )

    expect_equal(
      epoxy_style_html()("span.test-class#test-id x", env),
      html_chr('<span class="test-class" id="test-id">a</span><span class="test-class" id="test-id">b</span><span class="test-class" id="test-id">c</span>')
    )
  })
})

describe("parse_html_markup()", {
  it("returns the input as `item` if no markup detected", {
    expect_equal(
      parse_html_markup("word"),
      list(item = "word")
    )

    expect_equal(
      parse_html_markup("& two"),
      list(item = "& two")
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
      list(item = "item", element = "li")
    )

    expect_equal(
      parse_html_markup(".class item"),
      list(item = "item", class = "class")
    )

    expect_equal(
      parse_html_markup("#id item"),
      list(item = "item", id = "id")
    )

    expect_equal(
      parse_html_markup("#id.class item"),
      list(item = "item", class = "class", id = "id")
    )

    expect_equal(
      parse_html_markup(".class#id item"),
      list(item = "item", class = "class", id = "id")
    )

    expect_equal(
      parse_html_markup(".one.two#id item"),
      list(item = "item", class = "one two", id = "id")
    )

    expect_equal(
      parse_html_markup("li.one.two#id item"),
      list(item = "item", element = "li", class = "one two", id = "id")
    )
  })

  it("keeps additional spaces", {
    expect_equal(
      parse_html_markup("div list('a', 'b', 'c')"),
      list(
        item = "list('a', 'b', 'c')",
        element = "div"
      )
    )

    expect_equal(
      parse_html_markup("list('a', 'b', 'c')"),
      list(
        item = "list('a', 'b', 'c')"
      )
    )

    expect_equal(
      parse_html_markup("div('a', 'b', 'c')"),
      list(
        item = "div('a', 'b', 'c')"
      )
    )
  })

  it("retains old alternate 'id' syntax with '%'", {
    expect_equal(
      parse_html_markup("%id item"),
      list(item = "item", id = "id")
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
      list(item = "x", class = "this-class")
    )

    expect_equal(
      parse_html_markup("span.this-class x"),
      list(item = "x", element = "span", class = "this-class")
    )

    expect_equal(
      parse_html_markup("#this-id x"),
      list(item = "x", id = "this-id")
    )

    expect_equal(
      parse_html_markup("span#this-id x"),
      list(item = "x", element = "span", id = "this-id")
    )
  })
})
