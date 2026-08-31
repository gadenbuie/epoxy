# Top 10 Highest-Rated, Bechdel-Passing Movies

A small dataset for epoxy demonstrations with the top audience-rated
movies that pass the [Bechdel Test](https://bechdeltest.com).

## Usage

``` r
bechdel
```

## Format

A data frame with 10 rows and 18 variables:

- imdb_id:

  IMDB Movie ID

- bechdel_rating:

  Rating (0-3): 0 = unscored; 1 = It has to have at least two (named)
  women in it; 2 = Who talk to each other; 3 = About something besides a
  man.

- year:

  Year

- title:

  Title of movie

- budget:

  Budget in \$USD as of release year

- domgross:

  Domestic gross in \$USD in release year

- intgross:

  International gross in \$USD in release year

- plot:

  Plot of the movie

- rated:

  Moving rating, e.g. PG, PG-13, R, etc.

- language:

  Language of the movie

- country:

  Country where the movie was produced

- imdb_rating:

  IMDB rating of the movie, 0-10

- director:

  Director of the movie

- actors:

  Major actors appearing in the movie

- genre:

  Genre

- awards:

  Awards won by the movie, text description

- runtime:

  Movie runtime in minutes

- poster:

  URL of movie poster image, sourced from
  [themoviedb.org](https://www.themoviedb.org). Poster images URLs ar
  provided from the TMDB API but epoxy is not endorsed or certified by
  TMDB.

## Source

[TidyTuesday
(2021-03-09)](https://github.com/rfordatascience/tidytuesday/blob/044e769/data/2021/2021-03-09/readme.md),
[FiveThirtyEight](https://github.com/fivethirtyeight/data/tree/master/bechdel),
[bechdeltest.com](https://bechdeltest.com/),
[themoviedb.org](https://www.themoviedb.org)
