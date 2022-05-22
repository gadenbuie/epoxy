test_that("epoxy_style() works", {
  expect_equal(
    glue(
      "{letters[1:3]}",
      .transformer = epoxy_style(
        "code",
        epoxy_style_italic,
        epoxy_style_bold()
      )
    ),
    sprintf("**_`%s`_**", letters[1:3])
  )

  expect_equal(
    glue(
      "{letters[1:3]&}",
      .transformer = epoxy_style(
        "code",
        epoxy_style_bold,
        epoxy_style_collapse(last_and = ", ... and ... ")
      )
    ),
    "**`a`**, **`b`**, ... and ... **`c`**"
  )

  expect_equal(
    glue(
      "{letters[1:3]&}",
      .transformer = epoxy_style(
        "code",
        epoxy_style_bold,
        epoxy_style_collapse(last_and = ", ... and ... "),
        epoxy_style_wrap("<< ", " >>")
      )
    ),
    "<< **`a`**, **`b`**, ... and ... **`c`** >>"
  )

  expect_equal(
    glue(
      "{letters[1:3]&}",
      .transformer = epoxy_style(
        "code",
        epoxy_style_collapse(last_and = ", ... and ... "),
        epoxy_style_bold
      )
    ),
    "**`a`, `b`, ... and ... `c`**"
  )

  last_and <- ", ... and ... "
  expect_equal(
    glue(
      "{letters[1:3]&}",
      .transformer = epoxy_style(
        "code",
        epoxy_style_collapse(last_and = last_and),
        epoxy_style_bold
      )
    ),
    "**`a`, `b`, ... and ... `c`**"
  )
})

test_that("epoxy_style() throws an error for unknown styles", {
  expect_error(
    epoxy_style("bold", "foo"),
    "doesn't exist"
  )
})

test_that("epoxy_style_*() chooses syntax by epoxy.engine option", {
  with_options(
    list(epoxy.engine = "epoxy"),
    expect_equal(
      glue("{'word'}", .transformer = epoxy_style("bold")),
      "**word**"
    )
  )

  with_options(
    list(epoxy.engine = "epoxy_html"),
    expect_equal(
      glue("{'word'}", .transformer = epoxy_style("bold")),
      "<strong>word</strong>"
    )
  )

  with_options(
    list(epoxy.engine = "epoxy_latex"),
    expect_equal(
      glue("{'word'}", .transformer = epoxy_style("bold")),
      "\\textbf{word}"
    )
  )

  # markdown by default
  with_options(
    list(epoxy.engine = "foo"),
    expect_equal(
      glue("{'word'}", .transformer = epoxy_style("bold")),
      "**word**"
    )
  )
})

test_that("epoxy_style_*() functions choose syntax by argument", {
  expect_equal(
    glue("{'text'}", .transformer = epoxy_style_bold(syntax = "md")),
    "**text**"
  )

  expect_equal(
    glue("{'text'}", .transformer = epoxy_style_bold(syntax = "markdown")),
    "**text**"
  )

  expect_equal(
    glue("{'text'}", .transformer = epoxy_style("bold", syntax = "markdown")),
    "**text**"
  )

  expect_equal(
    glue("{'text'}", .transformer = epoxy_style_bold(syntax = "html")),
    "<strong>text</strong>"
  )

  expect_equal(
    glue("{'text'}", .transformer = epoxy_style("bold", syntax = "html")),
    "<strong>text</strong>"
  )

  expect_equal(
    glue("{'text'}", .transformer = epoxy_style_bold(syntax = "latex")),
    "\\textbf{text}"
  )

  expect_equal(
    glue("{'text'}", .transformer = epoxy_style("bold", syntax = "latex")),
    "\\textbf{text}"
  )
})

test_that("epoxy_style_apply()", {
  number <- 1.234234234234

  expect_equal(
    glue(
      "{number} {number}",
      .transformer = epoxy_style(
        epoxy_style_apply(~ .x + 10),
        epoxy_style_apply(round, digits = 2)
      )
    ),
    "11.23 11.23"
  )

  expect_equal(
    glue(
      "{word}{missing}",
      word = "here",
      missing = NULL,
      .transformer = epoxy_style(
        epoxy_style_apply(~ if (!length(.x)) "" else .x)
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
      .transformer = epoxy_style_apply(scale_pct, digits = 0)
    ),
    "33% less than 100% is 67%"
  )
})


describe("epoxy_style_collapse()", {
  it("does nothing by default", {
    expect_equal(
      glue("{1:3}", .transformer = epoxy_style_collapse()),
      c("1", "2", "3")
    )
  })

  it("collapses with custom separators", {
    expect_equal(
      glue("{1:3*}", .transformer = epoxy_style_collapse()),
      "1, 2, 3"
    )

    expect_equal(
      glue("{1:3*}", .transformer = epoxy_style_collapse(" - ")),
      "1 - 2 - 3"
    )

    expect_equal(
      glue("{1:3*}", .transformer = epoxy_style_collapse(" - ", "... ")),
      "1 - 2... 3"
    )
  })

  it("collapses with and()", {
    expect_equal(
      glue("{1:3&}", .transformer = epoxy_style_collapse()),
      and::and(1:3)
    )

    expect_equal(
      glue("{1:3&}", .transformer = epoxy_style_collapse(language = "es")),
      and::and(1:3, language = "es")
    )
  })

  it("collapses with or()", {
    expect_equal(
      glue("{1:3|}", .transformer = epoxy_style_collapse()),
      and::or(1:3)
    )

    expect_equal(
      glue("{1:3|}", .transformer = epoxy_style_collapse(language = "es")),
      and::or(1:3, language = "es")
    )
  })

  it("chains transformers", {
    expect_equal(
      glue("{1:3&}", .transformer = epoxy_style_collapse(transformer = epoxy_style_bold())),
      and::and(glue("**{1:3}**"))
      # "**1**, **2**, and **3**"
    )

    expect_equal(
      glue("{1:3*}", .transformer = epoxy_style_collapse(transformer = epoxy_style_bold())),
      "**1**, **2**, **3**"
    )

    expect_equal(
      glue("{1:3&}", .transformer = epoxy_style_bold(transformer = epoxy_style_collapse())),
      glue("**{and::and(1:3)}**")
      # "**1, 2, and 3**"
    )

    expect_equal(
      glue("{1:3*}", .transformer = epoxy_style_bold(transformer = epoxy_style_collapse())),
      "**1, 2, 3**"
    )
  })

  it("is part of the default transformer", {
    expect_equal(
      glue("{1:3&}", .transformer = epoxy_options_get_transformer(list())),
      and::and(1:3)
    )

    expect_equal(
      glue("{fmt(1:3 + 0.25 * 1:3, '$')&}", .transformer = epoxy_options_get_transformer(list())),
      and::and(c("$1.25", "$2.50", "$3.75"))
      # "$1.25, $2.50, and $3.75"
    )
  })
})
