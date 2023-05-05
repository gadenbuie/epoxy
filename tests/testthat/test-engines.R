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


describe("epoxy_html()", {
	it("returns an html glue character", {
		expect_s3_class(
			epoxy_html("word"),
			c("html", "glue", "character")
		)

		expect_s3_class(
			epoxy_html("{{'word'}}"),
			c("html", "glue", "character")
		)
	})

	it("uses html, inline stylers by default", {
		expect_equal(
			epoxy_html("{{ span letters[1:3] }}"),
			html_chr(glue("<span>{x}</span>", x = letters[1:3]))
		)

		expect_equal(
			epoxy_html("{{ span {{ .and letters[1:3] }} }}"),
			html_chr(glue("<span>{and::and(letters[1:3])}</span>"))
		)

		# need to escape one level to access inline formatter
		expect_equal(
			epoxy_html("{{ {{.and letters[1:3] }} }}"),
			html_chr(glue(and::and(letters[1:3])))
		)

		# otherwise `.and` is interpreted as a class
		expect_equal(
			epoxy_html("{{.and letters[1] }}"),
			html_chr(glue('<span class="and">a</span>'))
		)
	})
})

describe("epoxy_transform_set()", {
	it("sets the default for all styles", {
		opts <- epoxy_transform_set("bold")
		on.exit(options(opts))

		expect_equal(
			epoxy("{1} and {2} is {3}"),
			glue("**1** and **2** is **3**")
		)

		expect_equal(
			epoxy("{1} and {2} is {3}"),
			epoxy("{1} and {2} is {3}", .transformer = "bold")
		)

		expect_equal(
			epoxy_html("{{1}} and {{2}} is {{3}}"),
			html_chr(glue(
				"<strong>1</strong> and <strong>2</strong> is <strong>3</strong>"
			))
		)

		expect_equal(
			epoxy_latex("<1> and <2> is <3>"),
			glue("\\textbf{{1}} and \\textbf{{2}} is \\textbf{{3}}")
		)
	})

	it("sets the default for individual styles", {
		opts_md <- epoxy_transform_set("bold", engine = "md")
		on.exit(options(opts_md), add = TRUE)

		opts_html <- epoxy_transform_set("italic", engine = "html")
		on.exit(options(opts_html), add = TRUE)

		opts_latex <- epoxy_transform_set(epoxy_transform_code, engine = "latex")
		on.exit(options(opts_latex), add = TRUE)

		expect_equal(
			epoxy("{1} and {2} is {3}"),
			glue("**1** and **2** is **3**")
		)

		expect_equal(
			epoxy_html("{{1}} and {{2}} is {{3}}"),
			html_chr(glue(
				"<em>1</em> and <em>2</em> is <em>3</em>"
			))
		)

		expect_equal(
			epoxy_latex("<1> and <2> is <3>"),
			glue("\\texttt{{1}} and \\texttt{{2}} is \\texttt{{3}}")
		)
	})
})

describe("epoxy_transform_get()", {
	it("gets the current style function", {
		opts_md <- epoxy_transform_set("bold", engine = "md")
		on.exit(options(opts_md), add = TRUE)

		opts_html <- epoxy_transform_set("italic", engine = "html")
		on.exit(options(opts_html), add = TRUE)

		opts_latex <- epoxy_transform_set(epoxy_transform_code, engine = "latex")
		on.exit(options(opts_latex), add = TRUE)

		expect_equal(
			epoxy_transform_get("md"),
			epoxy_transform_bold(),
			ignore_function_env = TRUE
		)

		expect_equal(
			epoxy_transform_get("html"),
			epoxy_transform_italic(),
			ignore_function_env = TRUE
		)

		expect_equal(
			epoxy_transform_get("latex"),
			epoxy_transform_code(),
			ignore_function_env = TRUE
		)
	})
})
