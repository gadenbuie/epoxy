# Generated from example in ui_epoxy_markdown(): do not edit by hand
library(shiny)
library(epoxy)

# Shiny epoxy template functions don't support inline transformations,
# so we still have to do some prep work ourselves.
bechdel <- epoxy::bechdel

as_dollars <- scales::label_dollar(
  scale_cut = scales::cut_short_scale()
)
bechdel$budget <- as_dollars(bechdel$budget)
bechdel$domgross <- as_dollars(bechdel$domgross)

vowels <- c("a", "e", "i", "o", "u")
bechdel$genre  <- paste(
  ifelse(substr(tolower(bechdel$genre), 1, 1) %in% vowels, "an", "a"),
  tolower(bechdel$genre)
)

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
    bechdel[bechdel$imdb_id == input$movie, ]
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
