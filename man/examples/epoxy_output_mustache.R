if (interactive()) {
  library(shiny)
  library(epoxy)

  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        epoxy_output_mustache(
          id = "template",
          h2(class = "{{heading_class}}", "Hello, {{name}}!"),
          "{{#fruits}}",
          p("Your favorite fruits are..."),
          tags$ul(HTML("{{#fruit}}<li>{{.}}</li>{{/fruit}}")),
          "{{/fruits}}",
          "{{^fruits}}<p>Do you have any favorite fruits?</p>{{/fruits}}"
        )
      ),
      shiny::column(
        width = 6,
        h2("Inputs"),
        textInput("name", "Your name", "user"),
        textInput("fruits", "Favorite fruits", placeholder = "apple, banana"),
        helpText("Enter a comma-separated list of fruits.")
      )
    )
  )

  server <- function(input, output, session) {
    output$template <- render_epoxy(
      name = input$name,
      heading_class = if (nzchar(input$name) && input$name != "user") {
        "text-success"
      },
      fruits = if (nzchar(input$fruits)) {
        list(fruit = strsplit(input$fruits, "\\s*,\\s*")[[1]])
      }
    )
  }

  shinyApp(ui, server)
}
