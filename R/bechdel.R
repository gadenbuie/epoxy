#' Top 10 Highest-Rated, Bechdel-Passing Movies
#'
#' A small dataset for \pkg{epoxy} demonstrations with the top audience-rated
#' movies that pass the [Bechdel Test](https://bechdeltest.com).
#'
#' @format A data frame with 10 rows and 18 variables:
#' \describe{
#'   \item{imdb_id}{IMDB Movie ID}
#'   \item{bechdel_rating}{Rating (0-3): 0 = unscored; 1 = It has to have at least two (named) women in it; 2 = Who talk to each other; 3 = About something besides a man.}
#'   \item{year}{Year}
#'   \item{title}{Title of movie}
#'   \item{budget}{Budget in $USD as of release year}
#'   \item{domgross}{Domestic gross in $USD in release year}
#'   \item{intgross}{International gross in $USD in release year}
#'   \item{plot}{Plot of the movie}
#'   \item{rated}{Moving rating, e.g. PG, PG-13, R, etc.}
#'   \item{language}{Language of the movie}
#'   \item{country}{Country where the movie was produced}
#'   \item{imdb_rating}{IMDB rating of the movie, 0-10}
#'   \item{director}{Director of the movie}
#'   \item{actors}{Major actors appearing in the movie}
#'   \item{genre}{Genre}
#'   \item{awards}{Awards won by the movie, text description}
#'   \item{runtime}{Movie runtime in minutes}
#'   \item{poster}{URL of movie poster image, sourced from [themoviedb.org](https://www.themoviedb.org). Poster images URLs ar provided from the TMDB API but \pkg{epoxy} is not endorsed or certified by TMDB.}
#' }
#' @source [TidyTuesday (2021-03-09)](https://github.com/rfordatascience/tidytuesday/blob/044e769/data/2021/2021-03-09/readme.md),
#'   [FiveThirtyEight](https://github.com/fivethirtyeight/data/tree/master/bechdel),
#'   [bechdeltest.com](https://bechdeltest.com/),
#'   [themoviedb.org](https://www.themoviedb.org)
"bechdel"
