test_that("knitr_current_label() returns the label of current chunk", {
  code_chunk_label <- ""
  epoxy_chunk_label <- ""
  inline_chunk_label <- ""

  render_basic_rmd(
    "```{r code-chunk}",
    "code_chunk_label <- knitr_current_label()",
    "```",
    "",
    "```{epoxy, epoxy-chunk}",
    "{epoxy_chunk_label <- knitr_current_label()}",
    "```",
    "",
    "`r inline_chunk_label <- knitr_current_label()`",
    ""
  )

  expect_equal(code_chunk_label, "code-chunk")
  expect_equal(epoxy_chunk_label, "epoxy-chunk")
  expect_equal(inline_chunk_label, "___inline_chunk___")
})

test_that("knitr_chunk_get() gets the chunk and opts from a chunk", {
  the_chunk <- NULL
  foo <- "bar"

  render_basic_rmd(
    "```{r code-chunk, eval = FALSE, foo = foo}",
    "this is the code you're looking for",
    "```",
    "",
    "```{r}",
    "the_chunk <- knitr_chunk_get('code-chunk')",
    "```"
  )

  expect_equal(
    the_chunk,
    list(
      code = "this is the code you're looking for",
      opts = list(
        label = "code-chunk",
        eval = FALSE,
        # expressions in chunk options are evaluated
        foo = "bar"
      )
    )
  )
})

test_that("knitr_chunk_specific_options() returns current chunk options", {
  the_chunk <- NULL
  foo <- "bar"

  render_basic_rmd(
    "```{r code-chunk, echo = FALSE, foo = foo}",
    "the_chunk <- knitr_chunk_specific_options()",
    "```"
  )

  expect_equal(
    the_chunk,
    list(
      label = "code-chunk",
      echo = FALSE,
      foo = "bar"
    )
  )
})

test_that("knitr_chunk_specific_options() gets options for another chunk", {
  the_chunk <- NULL
  foo <- "bar"

  render_basic_rmd(
    "```{r code-chunk, echo = FALSE}",
    "the_chunk <- knitr_chunk_specific_options('other-chunk')",
    "```",
    "",
    "```{r other-chunk, eval = FALSE, bar = foo, .data = NULL}",
    "",
    "```"
  )

  expect_equal(
    the_chunk,
    list(
      label = "other-chunk",
      eval = FALSE,
      bar = "bar",
      .data = NULL
    )
  )
})

test_that("knitr_chunk_specific_options() returns NULL for inline chunks", {
  the_chunk <- NULL
  foo <- "bar"

  render_basic_rmd(
    "```{r code-chunk, echo = FALSE}",
    "the_chunk",
    "```",
    "",
    "`r the_chunk <- knitr_chunk_specific_options()`"
  )

  expect_null(the_chunk)
})

test_that("knitr_is_inline_chunk()", {
  first_chunk <- second_chunk <- third_chunk <- fourth_chunk <- NULL

  render_basic_rmd(
    "```{r first-chunk}",
    "first_chunk <- knitr_is_inline_chunk()",
    "```",
    "",
    "`r second_chunk <- knitr_is_inline_chunk()`",
    "",
    "```{r third-chunk}",
    "third_chunk <- knitr_is_inline_chunk()",
    "```",
    "",
    "`r fourth_chunk <- knitr_is_inline_chunk()`"
  )

  expect_false(first_chunk)
  expect_true(second_chunk)
  expect_false(third_chunk)
  expect_true(fourth_chunk)
})
