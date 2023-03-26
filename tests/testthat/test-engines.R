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
      html_chr(glue("<span>a</span><span>b</span><span>c</span>"))
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
