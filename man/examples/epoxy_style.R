epoxy("{.strong {.and letters[1:3]}}")
epoxy("{.and {.strong letters[1:3]}}")

# You can also use the static stylers to apply these transformations
# to every replacement. (This is slightly older syntax.)
epoxy("{letters[1:3]&}", .style = epoxy_style("bold", "collapse"))
epoxy("{letters[1:3]&}", .style = epoxy_style("collapse", "bold"))

# In an epoxy_html chunk...
epoxy_html("{{.strong {{.or letters[1:3] }} }}")

# Or in an epoxy_latex chunk...
epoxy_latex("<.and <.strong letters[1:3] >>")

# ---- Other Transfomers ----

# Format numbers with an inline transformation
amount <- 123.4234234
epoxy("{.number amount}")
epoxy(
  "{.number amount}",
  .style = epoxy_style_inline(
    number = scales::label_number(accuracy = 0.01)
  )
)

# Apply _any_ function to all replacements
epoxy(
  "{amount} is the same as {amount}",
  .style = epoxy_style_apply(round, digits = 0)
)

epoxy(
  "{amount} is the same as {amount}",
  .style = epoxy_style(
    epoxy_style_apply(~ .x * 100),
    epoxy_style_apply(round, digits = 2),
    epoxy_style_apply(~ paste0(.x, "%"))
  )
)
