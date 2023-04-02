revenue <- 0.2123
sales <- 42000.134

# ---- Basic Example with Inline Formatting --------------------------------
epoxy(
  '{.pct revenue} of revenue generates {.dollar sales} in profits.'
)

# With standard {glue} (`epoxy_style_inline()` is a glue transformer)
glue::glue(
  '{.pct revenue} of revenue generates {.dollar sales} in profits.',
  .transformer = epoxy_style_inline()
)

# ---- Setting Inline Formatting Options ----------------------------------
# To set inline format options, provide `scales::label_*()` to the supporting
# epoxy_style_inline arguments.
epoxy(
  '{.pct revenue} of revenue generates {.dollar sales} in profits.',
  .style = epoxy_style_inline(
    percent = scales::label_percent(accuracy = 0.1),
    dollar = scales::label_dollar(accuracy = 10)
  )
)

glue::glue(
  '{.pct revenue} of revenue generates {.dollar sales} in profits.',
  .transformer = epoxy_style_inline(
    percent = scales::label_percent(accuracy = 0.1),
    dollar = scales::label_dollar(accuracy = 10)
  )
)

# ---- Custom Inline Formatting ------------------------------------------
# Add your own formatting functions
search <- "why are cats scared of cucumbers"

epoxy_html(
  "https://example.com?q={{ .url_encode search }}>",
  .style = epoxy_style_inline(
    url_encode = utils::URLencode
  )
)
