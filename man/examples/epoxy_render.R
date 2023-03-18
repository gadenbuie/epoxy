# This small app shows the current time using `epoxyHTML()`
# to provide the HTML template and `epoxy_render()` to
# update the current time every second.

ui <- shiny::fluidPage(
  shiny::h2("Current Time"),
  epoxyHTML(
    "time",
    shiny::p("The current time is {{strong time}}.")
  )
)

server <- function(input, output, session) {
  current_time <- shiny::reactive({
    shiny::invalidateLater(1000)
    strftime(Sys.time(), "%F %T")
  })

  output$time <- epoxy_render(time = current_time())
}

if (interactive()) {
  shiny::shinyApp(ui, server)
}
