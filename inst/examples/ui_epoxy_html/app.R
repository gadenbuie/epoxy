# Generated from example in ui_epoxy_html(): do not edit by hand
library(shiny)
library(epoxy)

ui <- fluidPage(
	h2("ui_epoxy_html demo"),
	ui_epoxy_html(
		.id = "example",
		.class_item = "inner",
		fluidRow(
			tags$div(
				class = "col-xs-4",
				selectInput(
					inputId = "thing",
					label = "What is this {{color}} thing?",
					choices = c("apple", "banana", "coconut", "dolphin")
				)
			),
			tags$div(
				class = "col-xs-4",
				selectInput(
					inputId = "color",
					label = "What color is the {{thing}}?",
					c("red", "blue", "black", "green", "yellow")
				)
			),
			tags$div(
				class = "col-xs-4",
				sliderInput(
					inputId = "height",
					label = "How tall is the {{color}} {{thing}}?",
					value = 5,
					min = 0,
					max = 10,
					step = 0.1,
					post = "ft"
				)
			)
		),
		tags$p(class = "big", "The {{color}} {{thing}} is {{height}} feet tall."),
		# Default values for placeholders above.
		thing = "THING",
		color = "COLOR",
		height = "HEIGHT"
	),
	tags$style(HTML(
		".big { font-size: 1.5em; }
     .inner { background-color: rgba(254, 233, 105, 0.5);}
     .epoxy-item__placeholder { color: #999999; background-color: unset; }"
	))
)

server <- function(input, output, session) {
	output$example <- render_epoxy(
		thing = input$thing,
		color = input$color,
		height = input$height
	)
}

shinyApp(ui, server)
