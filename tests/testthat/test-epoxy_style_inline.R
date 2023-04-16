# test_that

describe("epoxy_style_inline()", {
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
        .style = epoxy_style_inline(
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
        .style = epoxy_style_inline(
          bold = function(x) "PASS"
        )
      ),
      "PASS"
    )

    expect_equal(
      epoxy(
        "{ .strong letters[1] }",
        .style = epoxy_style_inline(
          bold = function(x) "PASS"
        )
      ),
      "**a**"
    )
  })
})



