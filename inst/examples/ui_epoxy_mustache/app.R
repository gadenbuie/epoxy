library(shiny)

ui <- fluidPage(
  fluidRow(
    style = "max-width: 600px; margin: 0 auto",
    column(
      width = 6,
      ui_epoxy_mustache(
        id = "template",
        h2(class = "{{heading_class}}", "Hello, {{name}}!"),
        "{{#fruits}}",
        p("Your favorite fruits are..."),
        tags$ul(HTML("{{#fruit}}<li>{{.}}</li>{{/fruit}}")),
        "{{/fruits}}",
        "{{^fruits}}<p>Do you have any favorite fruits?</p>{{/fruits}}"
      )
    ),
    column(
      width = 6,
      h2("Inputs"),
      textInput("name", "Your name"),
      textInput("fruits", "Favorite fruits", placeholder = "apple, banana"),
      helpText("Enter a comma-separated list of fruits.")
    )
  )
)

server <- function(input, output, session) {
  user_name <- reactive({
    if (!nzchar(input$name)) return("user")
    input$name
  })

  fruits <- reactive({
    if (!nzchar(input$fruits)) return(NULL)
    list(fruit = strsplit(input$fruits, "\\s*,\\s*")[[1]])
  })

  output$template <- render_epoxy(
    name = user_name(),
    heading_class = if (user_name() != "user") "text-success",
    fruits = fruits()
  )
}

if (interactive()) {
  shiny::shinyApp(ui, server)
}
## Don't show: 
}) # examplesIf
## End(Don't show)
## Don't show: 
if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
run_epoxy_example_app("ui_epoxy_mustache")
