library(dplyr)

# ## Bechdel Test from tidytuesday
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-09/readme.md
raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

top_movies <-
  raw_bechdel %>%
  left_join(
    movies %>% select(imdb_id, imdb_rating),
    by = "imdb_id"
  ) %>%
  arrange(desc(rating), desc(imdb_rating)) %>%
  slice_head(n = 10) %>%
  select(imdb_id, bechdel_rating = rating)

bechdel <-
  movies %>%
  select(
    imdb_id,
    year,
    title,
    budget:intgross,
    plot,
    rated,
    language,
    country,
    imdb_rating,
    director,
    actors,
    genre,
    awards,
    runtime,
    poster
  ) %>%
  inner_join(top_movies, ., by = "imdb_id") %>%
  mutate(
    across(where(is.character), ~ if_else(.x == "N/A", "", .x)),
    across(budget:intgross, as.numeric),
    runtime = as.integer(sub(" min$", "", runtime))
  )

usethis::use_data(bechdel, overwrite = TRUE)
