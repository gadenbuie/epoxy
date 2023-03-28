glue::glue("{letters[1:3]&}", .transformer = epoxy_style("bold", "collapse"))
glue::glue("{letters[1:3]&}", .transformer = epoxy_style("collapse", "bold"))

# In an epoxy_html chunk...
# Note that you don't have to set `engine = "html"`, it just knows
glue::glue(
  "{letters[1:3]&}",
  .transformer = epoxy_style("bold", "collapse", engine = "html")
)

# Or in an epoxy_latex chunk...
glue::glue(
  "{letters[1:3]&}",
  .transformer = epoxy_style("bold", "collapse", engine = "latex")
)

# Other Transfomers ----

# Apply `format()` to all replacements
number <- 1.234234234
glue::glue(
  "{fmt(number, 'number')}",
  .transformer = epoxy_style_format(
    number = scales::label_number(accuracy = 0.01)
  )
)

# Apply _any_ function to all replacements
glue::glue(
  "{number}",
  .transformer = epoxy_style_apply(round, digits = 0)
)

glue::glue(
  "{number}",
  .transformer = epoxy_style(
    epoxy_style_apply(~ .x * 100),
    epoxy_style_apply(round, digits = 2),
    epoxy_style_apply(~ paste0(.x, "%"))
  )
)
