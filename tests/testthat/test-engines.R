describe("knitr engines", {
	opts <- list(
		echo = FALSE,
		eval = TRUE
	)

	it("works with knitr_engine_epoxy()", {
		opts$code <- "{.bold 'hello'}"
		opts$engine <- "epoxy"

		expect_equal(
			knitr_engine_epoxy(opts),
			"**hello**\n"
		)
	})

	it("works with knitr_engine_epoxy_html()", {
		opts$code <- "{{.bold 'hello'}}"
		opts$engine <- "epoxy_html"

		expect_equal(
			knitr_engine_epoxy_html(opts),
			"```{=html}\n<strong>hello</strong>\n```\n"
		)
	})

	it("works with knitr_engine_epoxy_latex()", {
		opts$code <- "<.bold 'hello'>"
		opts$engine <- "epoxy_latex"

		expect_equal(
			knitr_engine_epoxy_latex(opts),
			"```{=latex}\n\\textbf{hello}\n```\n"
		)
	})

	describe("use data chunk options", {
		opts$code <- "{x} and {y}"
		opts$engine <- "epoxy"
		opts$.data <- data.frame(x = 1:2, y = 3:4)

		it("accepts a data frame in the .data chunk option", {
			expect_equal(
				knitr_engine_epoxy(opts),
				"1 and 3\n2 and 4\n"
			)
		})

		it("supports legacy data chunk option", {
			opts$data <- opts$.data
			opts$.data <- NULL

			expect_equal(
				knitr_engine_epoxy(opts),
				"1 and 3\n2 and 4\n"
			)
		})

		it("prefers .data over data", {
			opts$.data <- data.frame(x = 5:6, y = 7:8)
			expect_equal(
				knitr_engine_epoxy(opts),
				"5 and 7\n6 and 8\n"
			)
		})

		it("handles list-column subsetting", {
			opts$.data <- data.frame(x = 1:2)
			opts$.data$a <- list(list(y = 3), list(y = 4))
			opts$code <- "{x} and {a$y}"

			expect_equal(
				knitr_engine_epoxy(opts),
				"1 and 3\n2 and 4\n"
			)
		})
	})
})

test_that("whisker template works", {
	# standard usage ----
	rmd <- '
```{r echo=FALSE}
library(epoxy)
data <- list(name = "Chris", value = 1000, taxed = 600, in_ca = TRUE)
```

```{whisker data = data, data_asis = TRUE, echo=FALSE}
Hello {{name}},
You have just won ${{value}}!
{{#in_ca}}
Well, ${{taxed}}, after taxes.
{{/in_ca}}
```
'

	expect_equal(
		render_rmd(rmd),
		"Hello Chris, You have just won $1000! Well, $600, after taxes."
	)

	# base case is the same as `data_asis = TRUE` ----
	rmd <- '
```{r include=FALSE}
library(epoxy)
knitr::opts_chunk$set(echo = FALSE)
data <- list(name = "Chris", value = 1000, taxed = 600, in_ca = TRUE)
```

```{whisker data = data, echo=FALSE}
Hello {{name}},
You have just won ${{value}}!
{{#in_ca}}
Well, ${{taxed}}, after taxes.
{{/in_ca}}
```
'

	expect_equal(
		render_rmd(rmd),
		"Hello Chris, You have just won $1000! Well, $600, after taxes."
	)

	# with multiple values per list item ----
	rmd <- '
```{r include=FALSE}
library(epoxy)
knitr::opts_chunk$set(echo = FALSE)
data <- list(name = c("Chris", "Jane"), value = c(1000, 2000), taxed = c(600, 600), in_ca = c(TRUE, FALSE))
```

```{whisker data = data, echo=FALSE}
Hello {{name}},
You have just won ${{value}}!
{{#in_ca}}
Well, ${{taxed}}, after taxes.
{{/in_ca}}
```
'

	expect_equal(
		render_rmd(rmd),
		c("Hello Chris, You have just won $1000! Well, $600, after taxes.",
			"",
			"Hello Jane, You have just won $2000!"
		)
	)

	# NULL list items are ignored
	rmd <- '
```{r include=FALSE}
library(epoxy)
knitr::opts_chunk$set(echo = FALSE)
data <- list(name = c("Chris", "Jane"), value = c(1000, 2000), taxed = c(600, 600), in_ca = NULL)
```

```{whisker data = data, echo=FALSE}
Hello {{name}},
You have just won ${{value}}!
{{#in_ca}}
Well, ${{taxed}}, after taxes.
{{/in_ca}}
```
'

	expect_equal(
		render_rmd(rmd),
		c("Hello Chris, You have just won $1000!",
			"",
			"Hello Jane, You have just won $2000!"
		)
	)

	# But mismatched data item lengths throws an error
	rmd <- '
```{r include=FALSE}
library(epoxy)
knitr::opts_chunk$set(echo = FALSE)
data <- list(name = c("Chris", "Jane"), value = 1000, taxed = c(600, 600), in_ca = NULL)
```

```{whisker data = data, echo=FALSE}
Hello {{name}},
You have just won ${{value}}!
{{#in_ca}}
Well, ${{taxed}}, after taxes.
{{/in_ca}}
```
'

	expect_error(render_rmd(rmd))
})


describe("chunk engine deprecations", {
	it ("warns about `epoxy_style` deprecation", {
		lifecycle::expect_deprecated(
			deprecate_epoxy_style_chunk_option(list(epoxy_style = "bold"))
		)
	})

	it ("warns about `glue_data` chunk option deprecation", {
		lifecycle::expect_defunct(
			deprecate_glue_data_chunk_option(list(glue_data = list()))
		)
	})

	it ("warns about `glue` chunk engine usage", {
		lifecycle::expect_deprecated(
			deprecate_glue_engine_prefix(list(engine = "glue"))
		)
	})

	it ("warns about `glue` chunk engine prefix", {
		lifecycle::expect_deprecated(
			deprecate_glue_engine_prefix(list(engine = "glue_html")),
			"epoxy_html"
		)

		lifecycle::expect_deprecated(
			deprecate_glue_engine_prefix(list(engine = "glue_latex")),
			"epoxy_latex"
		)
	})
})
