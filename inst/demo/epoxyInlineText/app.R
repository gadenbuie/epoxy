library(shiny)
library(epoxy)

ui <- fluidPage(
  h3("epoxyInlineText()"),
  p(
    "Of all of the places I've visited,",
    epoxy:::epoxyInlineText("place", "Name of City", "Chicago", hover_color = "#568EA3", focus_color = "#FF715B"),
    "was",
    epoxyHTML(.id = "feeling", "{{feeling}}.", feeling = "my favorite", .watch = list(click = "feeling"), .container = "span")
  ),
  verbatimTextOutput("answer"),
  actionButton("go_chicago", "Go to Chicago"),
  actionButton("go_newyork", "Go to New York")
)

server <- function(input, output, session) {
  output$answer <- renderPrint(str(list(place = input$place, feeling = feeling())))

  feeling <- reactive({
    # TODO: I need a better input structure that handles this use case better
    feelings <- c("my favorite", "the most fun", "terrible to visit", "the stinkiest")

    idx <- input$feeling_feeling_clicked
    if (is.null(idx)) idx <- 0
    idx <- (idx + 1) %% length(feelings)
    if (idx == 0) idx <- length(feelings)

    feelings[idx]
  })

  output$feeling <- renderEpoxyHTML(feeling = feeling())

  observeEvent(input$go_chicago, {
    epoxy:::updateEpoxyInlineText("place", "Chicago")
  })

  observeEvent(input$go_newyork, {
    epoxy:::updateEpoxyInlineText("place", "New York, NY")
  })
}

shinyApp(ui, server)
