# Generated from example in ui_epoxy_markdown(): do not edit by hand
library(shiny)
library(epoxy)

movie_ids <- rlang::set_names(
  bechdel$imdb_id,
  bechdel$title
)

ui <- fixedPage(
  fluidRow(
    column(
      width = 3,
      selectInput("movie", "Movie", movie_ids),
      uiOutput("poster")
    ),
    column(
      width = 9,
      ui_epoxy_markdown(
        .id = "about_movie",
        "
## {{title}}

**Released:** {{ year }} \\
**Rated:** {{ rated }} \\
**IMDB Rating:** {{ imdb_rating }}

_{{ title }}_ is {{ genre }} film released in {{ year }}.
It was filmed in {{ country }} with a budget of {{ budget }}
and made {{ domgross }} at the box office.
_{{ title }}_ recieved a Bechdel rating of **{{ bechdel_rating }}**
for the following plot:

> {{ plot }}
"
      )
    )
  )
)

server <- function(input, output, session) {
  movie <- reactive({
    movie <- bechdel[bechdel$imdb_id == input$movie, ]
    movie$budget <- epoxy("{.dollar budget}", .data = movie)
    movie$domgross <- epoxy("{.dollar domgross}", .data = movie)

    vowels <- c("a", "e", "i", "o", "u")
    movie$genre  <- paste(
      ifelse(substr(tolower(movie$genre), 1, 1) %in% vowels, "an", "a"),
      tolower(movie$genre)
    )

    movie
  })

  output$about_movie <- render_epoxy(.list = movie())
  output$poster <- renderUI(
    img(
      src = movie()$poster,
      alt = paste0("Poster for ", movie()$title),
      style = "max-height: 400px; max-width: 100%; margin: 0 auto; display: block;"
    )
  )
}

shinyApp(ui, server)
