# Generated from example in ui_epoxy_mustache(): do not edit by hand
library(shiny)
library(epoxy)

ui <- fluidPage(
  fluidRow(
    style = "max-width: 600px; margin: 0 auto",
    column(
      width = 6,
      ui_epoxy_mustache(
        id = "template",
        h2(class = "{{heading_class}}", "Hello, {{name}}!"),
        "{{#favorites}}",
        p("Your favorite fruits are..."),
        tags$ul(HTML("{{#fruits}}<li>{{.}}</li>{{/fruits}}")),
        "{{/favorites}}",
        "{{^favorites}}<p>Do you have any favorite fruits?</p>{{/favorites}}"
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

  favorites <- reactive({
    if (!nzchar(input$fruits)) return(NULL)
    list(fruits = strsplit(input$fruits, "\\s*,\\s*")[[1]])
  })

  output$template <- render_epoxy(
    name = user_name(),
    heading_class = if (user_name() != "user") "text-success",
    favorites = favorites()
  )
}

shiny::shinyApp(ui, server)
