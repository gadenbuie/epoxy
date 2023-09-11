skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shinytest2")
library(shinytest2)

test_that("ui_epoxy_html() can be used without shiny", {
	app <- AppDriver$new(
		app_dir = test_path("apps", "no-shiny"),
		name = "no-shiny",
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

	chrome <- app$get_chromote_session()

	update_input <- function(id, text) {
		inputs <- structure(list(text), names = id)
		app$set_inputs(!!!inputs)
		# In real life, the user's interaction would trigger this event
		app$run_js(sprintf(
			"document.getElementById('%s').dispatchEvent(new Event('input', { bubbles: true }))",
			id
		))
	}

	expect_epoxy_text <- function() {
		inputs <- app$get_values(input = c("first", "last"))$input

		expect_equal(
			get_test_element_text("text"),
			paste0("Hello, ", inputs$first, " ", inputs$last, "!")
		)
	}

  expect_epoxy_text()

	update_input("first", "Jane")
	expect_epoxy_text()

	update_input("last", "Diamonds")
	expect_epoxy_text()

	update_input("first", "Lucy")
	expect_epoxy_text()
})
