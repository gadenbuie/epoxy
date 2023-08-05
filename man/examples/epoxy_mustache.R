# The canonical mustache example
epoxy_mustache(
  "Hello {{name}}!",
  "You have just won {{value}} dollars!",
  "{{#in_ca}}",
  "Well, {{taxed_value}} dollars, after taxes.",
  "{{/in_ca}}",
  .data = list(
    name = "Chris",
    value = 10000,
    taxed_value = 10000 - (10000 * 0.4),
    in_ca = TRUE
  )
)

# Vectorized over the rows of .data
epoxy_mustache(
	"mpg: {{ mpg }}",
	"hp: {{ hp }}",
	"wt: {{ wt }}\n",
	.data = mtcars[1:2, ]
)

# Non-vectorized
epoxy_mustache(
	"mpg: {{ mpg }}",
	"hp: {{ hp }}",
	"wt: {{ wt }}",
	.data = mtcars[1:2, ],
  .vectorized = FALSE
)

# With mustache partials
epoxy_mustache(
  "Hello {{name}}!",
	"{{> salutation }}",
  "You have just won {{value}} dollars!",
  "{{#in_ca}}",
  "Well, {{taxed_value}} dollars, after taxes.",
  "{{/in_ca}}",
	.partials = list(
		salutation = c("Hope you are well, {{name}}.")
	),
	.sep = " ",
  .data = list(
    name = "Chris",
    value = 10000,
    taxed_value = 10000 - (10000 * 0.4),
    in_ca = TRUE
  )
)


