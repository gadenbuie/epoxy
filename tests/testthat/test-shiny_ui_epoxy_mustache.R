skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shinytest2")
library(shinytest2)

test_that("ui_epoxy_mustache() with an array of values", {

	app <- AppDriver$new(
		app_dir = system.file("examples", "ui_epoxy_mustache", package = "epoxy"),
		name = "shiny_word-list",
		height = 500,
		width = 700,
		view = interactive(),
		expect_values_screenshot_args = FALSE
	)

	expect_equal(
		app$get_text("#template h2"),
		"Hello, user!"
	)
	expect_equal(
		app$get_text("#template p"),
		"Do you have any favorite fruits?"
	)

	app$set_inputs(name = "Jane")
	expect_equal(
		app$get_text("#template h2"),
		"Hello, Jane!"
	)
	expect_match(
		app$get_js("document.querySelector('#template h2').className"),
		"text-success"
	)

	app$set_inputs(name = "")
	expect_equal(
		app$get_js("document.querySelector('#template h2').className"),
		""
	)

	app$set_inputs(fruits = "apple, banana, mango")
	expect_match(
		app$get_text("#template p"),
		"Your favorite fruits are..."
	)
	expect_setequal(
		app$get_text("#template li"),
		c("apple", "banana", "mango")
	)

	app$set_inputs(fruits = NULL)
	expect_equal(
		app$get_text("#template h2"),
		"Hello, user!"
	)
	expect_equal(
		app$get_text("#template p"),
		"Do you have any favorite fruits?"
	)
})
