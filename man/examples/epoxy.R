movie <- bechdel[1, ]
movies <- bechdel[2:4, ]

epoxy("*{movie$title}* ({movie$year}) was directed by {movie$director}.")
epoxy("- *{movies$title}* ({movies$year}) was directed by {movies$director}.")

epoxy("*{title}* ({year}) was directed by {director}.", .data = movie)
epoxy("- *{title}* ({year}) was directed by {director}.", .data = movies)

epoxy(
  "{title} ({year}) was directed by {director}.",
  .data = movie,
  .style = "bold"
)

epoxy(
  "I'd be happy to watch {title |}.",
  .data = movies,
  .style = c("italic", "collapse")
)

epoxy(
  "They were directed by {director &}.",
  .data = movies,
  .style = c("collapse", "bold")
)

epoxy("The budget for *{title}* was {fmt(budget, '$')}.", .data = movie)

