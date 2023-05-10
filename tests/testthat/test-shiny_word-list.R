skip_on_cran()
skip_if_not_installed("chromote")
skip_if_not_installed("shinytest2")
library(shinytest2)

test_that("ui_epoxy_html() with an array of values", {
	the_app <- shiny::shinyApp(
		ui = shiny::fluidPage(
			shiny::div(
				class = "row",
				style = "margin: 1em;",
				shiny::div(
					class = "col-xs-6",
					shiny::selectInput("number", "Number of words", choices = 1:5)
				),
				shiny::div(
					class = "col-xs-6",
					ui_epoxy_html(
						.id = "word_list",
						shiny::tags$ul("{{li.word the_words}}")
					)
				)
			)
		),
		server = function(input, output, session) {
			words <- shiny::reactive({
				w <- c("one", "two", "three", "four", "five")
				w[seq_len(as.integer(input$number))]
			})

			output$word_list <- render_epoxy(the_words = words())
		}
	)

	app <- AppDriver$new(
		app_dir = the_app,
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
		unlist(li)
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
