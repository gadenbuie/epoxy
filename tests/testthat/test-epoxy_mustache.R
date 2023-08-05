# test_that()

describe("epoxy_mustache()", {
  it("throws for named inputs in ...", {
    expect_error(epoxy_mustache(a = 1))
  })

  it("concatenates multiple lines", {
    expect_equal(
      epoxy_mustache("one", "two"),
      "one\ntwo"
    )

    expect_equal(
      epoxy_mustache(!!!c("one", "two")),
      "one\ntwo"
    )
  })

  it("renders a template", {
    expect_equal(
      epoxy_mustache(
        "Hello {{name}}!",
        .data = list(name = "Chris")
      ),
      "Hello Chris!"
    )
  })

  it("renders nested templates", {
    expect_equal(
      epoxy_mustache(
        "Hello {{#person}}{{first}} {{last}}{{/person}}!",
        .data = list(
          person = list(
            first = "Nate",
            last = "Nickerson"
          )
        )
      ),
      "Hello Nate Nickerson!"
    )
  })

  it("vectorizes over data frames", {
    expect_equal(
      epoxy_mustache(
        "mpg: {{mpg}}",
        .data = mtcars[1:3, ]
      ),
      paste("mpg:", mtcars$mpg[1:3])
    )
  })

  it("collapses data frame columns if not vectorized", {
    expect_equal(
      epoxy_mustache(
        "mpg: {{mpg}}",
        .data = mtcars[1:3, ],
        .vectorized = FALSE
      ),
      paste("mpg:", paste(mtcars$mpg[1:3], collapse = ","))
    )
  })
})
