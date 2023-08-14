# test_that()

describe("epoxy_use_chunk()", {

	it("can be called in an R chunk", {
		expect_equal(
			render_rmd(test_path("rmds", "use-chunk_chunk.Rmd")),
			c(
				"one followed by two",
				"",
				"three followed by four"
			)
		)
	})

	it("uses `.data` in the expected order", {
		res <- render_rmd(test_path("rmds", "use-chunk_chunk-opts.Rmd"))

		# uses the chunk option of the epoxy chunk
		expect_equal(res[1], "three followed by four")

		# uses the `.data` argument of `epoxy_use_chunk()`
		expect_equal(res[3], "five followed by six")

		# uses the `.data` argument even if the chunk has a `.data` option
		expect_equal(res[5], "seven followed by eight")

		# uses the `.data` chunk option if the argument isn't provided
		expect_equal(res[7], "nine followed by ten")

		# inline chunks fall back to the template chunks' .data option
		expect_equal(res[9], "three followed by four")
	})

  it("throws for bad labels", {
    expect_error(epoxy_use_chunk(label = 27))
    expect_error(epoxy_use_chunk(label = c("bad", "label")))
    expect_error(epoxy_use_chunk(label = NULL))
  })

  it("throws for unknown labels", {
    render_basic_rmd(
      "```{r}",
      "expect_error(epoxy_use_chunk(label = 'bad-label'))",
      "```"
    )
  })
})
