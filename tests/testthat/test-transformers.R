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

test_that("epoxy_style_*() functions choose default by engine", {
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
