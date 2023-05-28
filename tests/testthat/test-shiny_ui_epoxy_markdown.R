skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shinytest2")
library(shinytest2)

test_that("ui_epoxy_markdown()", {

	app <- AppDriver$new(
		app_dir = system.file("examples", "ui_epoxy_markdown", package = "epoxy"),
		name = "ui_epoxy_markdown",
		height = 500,
		width = 900,
		view = interactive(),
		expect_values_screenshot_args = FALSE
	)

	idx <- bechdel$imdb_id == app$get_value(input = "movie")

	expect_equal(
		app$get_text("#about_movie #title"),
		bechdel$title[idx]
	)

	expect_match(
		app$get_text("#about_movie p:nth-of-type(2)"),
		tolower(bechdel$genre[idx])
	)

	expect_snapshot(cat(app$get_text("#about_movie")))

	app$set_inputs(movie = "0462538")
	app$wait_for_idle()

	idx <- bechdel$imdb_id == app$get_value(input = "movie")

	expect_equal(
		app$get_text("#about_movie #title"),
		bechdel$title[idx]
	)

	expect_match(
		app$get_text("#about_movie p:nth-of-type(2)"),
		tolower(bechdel$genre[idx])
	)

	expect_snapshot(cat(app$get_text("#about_movie")))
})
