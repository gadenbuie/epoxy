revenue <- 0.2123
sales <- 42000.134
glue::glue(
  '{fmt(revenue, "%")} of revenue generates {fmt(sales, "$")} in profits.',
  .transformer = epoxy_style_format()
)

# To set labeller options, provide the label calls
glue::glue(
  '{fmt(revenue, "%")} of revenue generates {fmt(sales, "$")} in profits.',
  .transformer = epoxy_style_format(
    percent = scales::label_percent(accuracy = 0.1),
    dollar = scales::label_dollar(accuracy = 10)
  )
)

# Add your own formatting functions
search <- "why are cats scared of cucumbers"
glue::glue(
  '<https://example.com?q={fmt(search, "url")}>',
  .transformer = epoxy_style_format(
    url = utils::URLencode
  )
)
