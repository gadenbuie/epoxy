skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shinytest2")
library(shinytest2)

test_that("ui_epoxy_html() with an array of values", {

	app <- AppDriver$new(
		app_dir = system.file("examples", "word-list", package = "epoxy"),
		name = "shiny_word-list",
		height = 500,
		width = 700,
		view = interactive(),
		expect_values_screenshot_args = FALSE
	)

	get_output_list <- function() {
		li <- app$get_js(
			"Array.from(document.querySelectorAll('#word_list li')).map(x => x.innerText)"
		)
		unlist(li)
	}

	get_output_list_classes <- function() {
		li <- app$get_js(
			"Array.from(document.querySelectorAll('#word_list li')).map(x => x.className)"
		)
		trimws(gsub("animate|blur|pop", "", unlist(li)))
	}

	expect_equal(app$get_value(input = "number"), "1")
	expect_equal(get_output_list(), "one")
	expect_equal(get_output_list_classes(), "word")

	app$set_inputs(number = "3")
	expect_equal(get_output_list(), c("one", "two", "three"))
	expect_equal(get_output_list_classes(), rep("word", 3))

	app$set_inputs(number = "5")
	expect_equal(get_output_list(), c("one", "two", "three", "four", "five"))
	expect_equal(get_output_list_classes(), rep("word", 5))

	app$set_inputs(number = "1")
	expect_equal(get_output_list(), "one")
	expect_equal(get_output_list_classes(), "word")
})
