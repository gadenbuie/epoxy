movie <- bechdel[1, ]
movies <- bechdel[2:4, ]

# A basic example with a single row of data
epoxy("{.emph movie$title} ({movie$year}) was directed by {movie$director}.")

# Or vectorized over multiple rows of data
epoxy("* {.emph movies$title} ({movies$year}) was directed by {movies$director}.")

# You can provide the data frame to `.data` to avoid repeating `data$`
epoxy("{.emph title} ({year}) was directed by {director}.", .data = movie)
epoxy("* {.emph title} ({year}) was directed by {director}.", .data = movies)

# Inline transformers can be nested
epoxy("I'd be happy to watch {.or {.italic title}}.", .data = movies)
epoxy("They were directed by {.and {.bold director}}.", .data = movies)

# Learn more about inline transformers in ?epoxy_transform_inline
epoxy("The budget for {.emph title} was {.dollar budget}.", .data = movie)

# --------- HTML and LaTeX variants ---------
# There are also HTML and LaTeX variants of epoxy.
# Each uses default options that are most natural for the format.

# epoxy_html() uses `{{ expr }}` for delimiters
epoxy_html("I'd be happy to watch {{ title }}.", .data = movie)
# It also supports an HTML transformer syntax
epoxy_html("I'd be happy to watch {{em.movie-title title}}.", .data = movie)
# Or use the inline transformer syntax, which uses `@` instead of `.` in HTML
epoxy_html("I'd be happy to watch {{@or {{@emph title}} }}.", .data = movies)

# epoxy_latex() uses `<< expr >>` for delimiters
epoxy_latex("I'd be happy to watch <<.or <<.emph title >> >>.", .data = movies)
