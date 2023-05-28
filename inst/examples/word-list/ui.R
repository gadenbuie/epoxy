library(shiny)
library(epoxy)

page <-
  if (!requireNamespace("bslib", quietly = TRUE)) {
    bslib::page_fluid
  } else {
    fluidPage
  }

page(
  div(
    class = "row",
    style = "margin: 1em auto; max-width: 800px;",
    div(
      class = "col-sm-6",
      selectInput("number", "Number of words", choices = 1:10)
    ),
    div(
      class = "col-sm-6 mt-xs-5",
      ui_epoxy_html(
        .id = "word_list",
        tags$h3("Here {{ verb }} your {{ n_words }}"),
        tags$ul("{{li.word the_words}}")
      )
    ),
    tags$link(rel = "stylesheet", href = "animation.css"),
    tags$script(src = "extra.js")
  )
)
