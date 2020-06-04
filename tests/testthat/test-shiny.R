test_that("epoxyHTML() doesn't find things in the global environment", {
  example <- "BAD"
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

test_that("epoxyHTML() works with htmltags, too", {
  example <- "BAD"
  ex <- epoxyHTML("test", htmltools::tags$p("{{example}}"), example = "GOOD")
  expect_true(grepl("<p>.+GOOD.*</p>", format(ex)))

  ex2 <- epoxyHTML("test", "{{p example}}", example = "GOOD")
  expect_true(grepl("<p class=\"epoxy-item__placeholder\".+GOOD.*</p>", format(ex2)))
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
