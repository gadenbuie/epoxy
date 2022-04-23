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
