epoxy("{.strong {.and letters[1:3]}}")
epoxy("{.and {.strong letters[1:3]}}")

# If you used the development version of epoxy, the above is equivalent to:
epoxy("{letters[1:3]&}", .transformer = epoxy_transform("bold", "collapse"))
epoxy("{letters[1:3]&}", .transformer = epoxy_transform("collapse", "bold"))

# In an epoxy_html chunk...
epoxy_html("{{.strong {{.or letters[1:3] }} }}")

# Or in an epoxy_latex chunk...
epoxy_latex("<.and <.strong letters[1:3] >>")

# ---- Other Transformers ----

# Format numbers with an inline transformation
amount <- 123.4234234
epoxy("{.number amount}")
epoxy(
  "{.number amount}",
  .transformer = epoxy_transform_inline(
    number = scales::label_number(accuracy = 0.01)
  )
)

# Apply _any_ function to all replacements
epoxy(
  "{amount} is the same as {amount}",
  .transformer = epoxy_transform_apply(round, digits = 0)
)

epoxy(
  "{amount} is the same as {amount}",
  .transformer = epoxy_transform(
    epoxy_transform_apply(~ .x * 100),
    epoxy_transform_apply(round, digits = 2),
    epoxy_transform_apply(~ paste0(.x, "%"))
  )
)
