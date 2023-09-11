skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shinytest2")
library(shinytest2)

test_that("ui_epoxy_html() doesn't break block-level elements", {
	app <- AppDriver$new(
		app_dir = test_path("apps", "epoxy-html-list"),
		name = "epoxy-html-list",
		height = 500,
		width = 700,
		view = interactive(),
		expect_values_screenshot_args = FALSE
	)
	on.exit(app$stop())

	get_test_element_text <- function(id) {
		app$get_js(
			sprintf(
				"document.querySelector('[data-test-id=\"%s\"]').innerText",
				id
			)
		)
	}

	app$run_js("
	document.getElementById('list').addEventListener('epoxy-updated', ev => {
		console.log(ev)
		Shiny.setInputValue('epoxy_updated_list', ev.detail);
	})
  document.getElementById('desc').addEventListener('epoxy-updated', ev => {
		console.log(ev)
		Shiny.setInputValue('epoxy_updated_desc_' + ev.detail.key, ev.detail);
	})
	")

	expect_event <- function(input, ...) {
		data <- list(...)
		expect_equal(
			app$get_values(input = !!input)$input[[!!input]],
			!!data
		)
	}

	app$set_inputs(n = 1)
	expect_equal(
		get_test_element_text("desc"),
		"You've picked 1 letter:"
	)
	expect_equal(
		get_test_element_text("list"),
		"a"
	)
	expect_event(
		"epoxy_updated_list",
		output = "list",
		key = "item",
		data = "a",
		outputType = "html"
	)
	expect_event(
		"epoxy_updated_desc_n",
		output = "desc",
		key = "n",
		data = 1L,
		outputType = "html"
	)
	expect_event(
		"epoxy_updated_desc_thing",
		output = "desc",
		key = "thing",
		data = "letter",
		outputType = "html"
	)

	app$set_inputs(n = 4)
	expect_equal(
		get_test_element_text("desc"),
		"You've picked 4 letters:"
	)
	expect_equal(
		get_test_element_text("list"),
		"a\nb\nc\nd"
	)
	expect_event(
		"epoxy_updated_list",
		output = "list",
		key = "item",
		data = as.list(letters[1:4]),
		outputType = "html",
		# there are three copies created from the original letter template
		# the event has dom elements, shinytest2 only sees an empty named list
		copies = lapply(1:3, function(...) list(a = 1)[0])
	)
	expect_event(
		"epoxy_updated_desc_n",
		output = "desc",
		key = "n",
		data = 4L,
		outputType = "html"
	)
	expect_event(
		"epoxy_updated_desc_thing",
		output = "desc",
		key = "thing",
		data = "letters",
		outputType = "html"
	)
})
