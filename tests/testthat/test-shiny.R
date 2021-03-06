describe("epoxyHTML()", {
  example <- "BAD"

  it ("doesn't find things in the global environment", {
    example <- "BADBAD"
    ex <- epoxyHTML("test", "{{example}}", example = "GOOD")
    expect_true(grepl("GOOD", format(ex)))
    expect_false(grepl("BAD", format(ex)))

    ex2 <- epoxyHTML("test", "{{example}}", example = "GOOD", .placeholder = "NEUTRAL")
    expect_true(grepl("GOOD", format(ex2)))
    expect_false(grepl("NEUTRAL", format(ex2)))
    expect_false(grepl("BAD", format(ex2)))

    ex3 <- epoxyHTML("test", "{{example}}", .placeholder = "NEUTRAL")
    expect_true(grepl("NEUTRAL", format(ex3)))
    expect_false(grepl("BAD", format(ex3)))
  })

  it ("works with htmltags, too", {
    ex <- epoxyHTML("test", htmltools::tags$p("{{example}}"), example = "GOOD")
    expect_true(grepl("<p>.+GOOD.*</p>", format(ex)))

    ex2 <- epoxyHTML("test", "{{p example}}", example = "GOOD")
    expect_true(grepl("<p class=\"epoxy-item__placeholder\".+GOOD.*</p>", format(ex2)))
  })

  it ("works with html in placeholders", {
    ex <- epoxyHTML("test", "{{example}}", example="<strong>placeholder</strong>")
    expect_true(grepl("<strong>placeholder</strong>", format(ex), fixed = TRUE))

    ex2 <- epoxyHTML("test", "{{example}}", example = htmltools::tags$strong("placeholder"))
    expect_true(grepl("<strong>placeholder</strong>", format(ex2), fixed = TRUE))
  })

  it (".container and .container_item", {
    div_span <- epoxyHTML("test", "{{item}}")
    expect_s3_class(div_span, "shiny.tag")
    expect_equal(div_span$name, "div")
    expect_true(grepl("^<span", div_span$children[[1]]))

    ul_li <- epoxyHTML("test", "{{item}}", .container = "ul", .container_item = "li")
    expect_s3_class(ul_li, "shiny.tag")
    expect_equal(ul_li$name, "ul")
    expect_true(grepl("^<li", ul_li$children[[1]]))

    ul_li <- epoxyHTML("test", "{{li item}}", .container = "ul")
    expect_s3_class(ul_li, "shiny.tag")
    expect_equal(ul_li$name, "ul")
    expect_true(grepl("^<li", ul_li$children[[1]]))
  })
})

test_that("parse_html_markup() returns a list", {
  expect_true(is.list(parse_html_markup("h1 item")))
})

describe("parse_html_markup", {
  it ("returns the item if only item", {
    expect_equal(parse_html_markup("item")$item, "item")
    expect_equal(parse_html_markup(" item")$item, "item")
    expect_equal(parse_html_markup("      item")$item, "item")
    expect_equal(parse_html_markup("item      ")$item, "item")
    expect_equal(parse_html_markup("... item")$item, "item")
  })

  it ("throws error if more than 1 space", {
    expect_error(parse_html_markup("a b c"))
  })

  it ("parses html elements", {
    expect_equal(parse_html_markup("h1 item")$element, "h1")
    expect_equal(parse_html_markup("span item")$element, "span")
  })

  it ("errors if unrecognized html element is used", {
    expect_error(parse_html_markup("foo item")$element, "foo")
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
})
