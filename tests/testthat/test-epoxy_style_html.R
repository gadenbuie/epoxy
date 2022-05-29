# test_that()

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
})
