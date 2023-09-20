library(shiny)
library(epoxy)

ui <- fixedPage(
	textInput("first", "First Name", "John"),
	textInput("last", "Last Name", "Doe"),
	ui_epoxy_html(
		"hello",
		p("Hello, {{first}} {{last}}!", "data-test-id" = "text")
	),
	includeScript("epoxy-no-shiny.js")
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
