library(shiny)
library(epoxy)

ui <- fixedPage(
	sliderInput("n", "Letters", 1, 26, 3),
	p(
		"data-test-id" = "desc",
		ui_epoxy_html("desc", "You've picked {{n}} {{thing}}:")
	),
	tags$ul(
		"data-test-id" = "list",
		ui_epoxy_html("list", "{{item}}", .item_tag = "li")
	)
)

server <- function(input, output, session) {
  output$list <- render_epoxy(
		item = letters[1:input$n]
	)

	output$desc <- render_epoxy(
		n = input$n,
		thing = if (input$n == 1) "letter" else "letters"
	)
}

shinyApp(ui, server)
