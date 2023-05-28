function(input, output, session) {
  words <- shiny::reactive({
    w <- c(
      "one", "two", "three", "four", "five",
      "six", "seven", "eight", "nine", "ten"
    )
    w[seq_len(as.integer(input$number))]
  })

  n_words <- shiny::reactive( {
    n_words <- length(words())
    paste(n_words, ngettext(n_words, "word", "words"))
  })

  output$word_list <- render_epoxy(
    verb = if (length(words()) == 1) "is" else "are",
    n_words = n_words(),
    the_words = words()
  )
}
