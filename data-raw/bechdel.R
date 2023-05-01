library(dplyr)
library(httr2)

# ## Bechdel Test from tidytuesday
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-09/readme.md
raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

set.seed(424042)
top_movies <-
	raw_bechdel %>%
	rename(bechdel_rating = rating) %>%
	left_join(
		movies %>% select(imdb_id, imdb_rating, rated),
		by = "imdb_id"
	) %>%
	filter(rated %in% c("N/A", "G", "PG", "PG-13")) %>%
	arrange(desc(bechdel_rating), desc(imdb_rating)) %>%
	filter(imdb_rating > 6, bechdel_rating == 3) %>%
	group_by(imdb_rating) %>%
	slice_sample(n = 2, replace = TRUE) %>%
	ungroup() %>%
	distinct() %>%
	slice_sample(n = 10) %>%
	arrange(desc(imdb_rating)) %>%
	select(imdb_id, bechdel_rating)


get_tmdb_movie <- function(title, year) {
	res <-
		request("https://api.themoviedb.org/3") %>%
		req_url_path_append("search", "movie") %>%
		req_url_query(api_key = keyring::key_get("tmdb_api_key_v3")) %>%
		req_url_query(query = title, year = year) %>%
		req_perform() %>%
		resp_body_json()

	if (is.null(res$results)) {
		return(NA_character_)
	}

	poster_path <- res$results[[1]]$poster_path
	as.character(glue::glue("https://image.tmdb.org/t/p/w500{poster_path}"))
}

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
		runtime
	) %>%
	inner_join(top_movies, ., by = "imdb_id") %>%
	mutate(
		across(where(is.character), ~ if_else(.x == "N/A", "", .x)),
		across(budget:intgross, as.numeric),
		runtime = as.integer(sub(" min$", "", runtime)),
		poster = purrr::pmap_chr(., function(title, year, ...) get_tmdb_movie(title, year))
	)

# bechdel %>%
#   select(.title = title, .poster = poster) %>%
#   purrr::pmap(function(.title, .poster) htmltools::withTags(
#     div(
#       style="max-width: 20vw; margin: 10px;",
#       h3(.title),
#       p(img(src = .poster, style = "max-width: 100%; max-height: 100%"))
#     )
#   )) %>%
#   htmltools::div(style = "display: flex; flex-direction: row; flex-wrap: wrap") %>%
#   htmltools::browsable()

usethis::use_data(bechdel, overwrite = TRUE)
