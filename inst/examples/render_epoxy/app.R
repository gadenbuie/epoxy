# This small app shows the current time using `ui_epoxy_html()`
# to provide the HTML template and `render_epoxy()` to
# update the current time every second.

ui <- shiny::fluidPage(
  shiny::h2("Current Time"),
  ui_epoxy_html(
    "time",
    shiny::p("The current time is {{strong time}}.")
  )
)

server <- function(input, output, session) {
  current_time <- shiny::reactive({
    shiny::invalidateLater(1000)
    strftime(Sys.time(), "%F %T")
  })

  output$time <- render_epoxy(time = current_time())
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
run_epoxy_example_app("render_epoxy")
