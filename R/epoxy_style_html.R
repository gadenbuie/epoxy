
epoxy_style_html <- function(
  class = NULL,
  transformer = glue::identity_transformer
) {
  function(text, envir) {
    markup <- parse_html_markup(text)

    text <- transformer(markup$item, envir)

    if (identical(names(markup), "item")) {
      # regular glue text, no added html markup
      return(text)
    }

    tag_name <- markup$element
    if (is.null(tag_name)) tag_name <- element
    if (!is.null(markup$class)) {
      class <- collapse_space(c(class, markup$class))
    }

    html <- lapply(text, function(x) {
      htmltools::tag(
        tag_name,
        list(class = class, id = markup$id, x),
        .noWS = c("inside", "outside")
      )
    })

    out <-
      if (length(html) == 1) {
        html[[1]]
      } else {
        htmltools::tagList(html)
      }

    html_chr(out)
  }
}

html_chr <- function(x) {
  if (!is.character(x)) {
    x <- format(x)
  }
  class(x) <- c("html", class(x))
  x
}

parse_html_markup <- function(x) {
  x_og <- x
  x <- trimws(x)
  n_spaces <- str_count(x, " ")
  if (n_spaces == 0) {
    return(list(item = x))
  }

  # pug-like syntax starts with # (id), . (class), or element name
  has_el_syntax <-
    substr(x, 1, 1) %in% c("#", "%", ".") ||
    grepl(html_element_rgx(), x)

  if (!has_el_syntax) {
    return(list(item = x))
  }

  x <- strsplit(x, " ")[[1]]
  item_id <- paste(x[-1], collapse = " ")

  rgx_markup <- "(([#%. ]|^)[[:alnum:]_-]+)"
  m <- str_extract_all(x[1], rgx_markup)[[1]]

  if (!length(m)) {
    # should have been caught but just in case
    return(list(item = item_id))
  }

  out <- list(item = item_id)
  for (m_part in m) {
    if (grepl("^[.]", m_part)) {
      out$class <- c(out$class, sub("^[.]", "", m_part))
    } else if (grepl("^[#%]", m_part)) {
      this_id <- sub("^[#%]", "", m_part)
      if (!is.null(out$id)) {
        rlang::abort(c(
          "Multiple IDs were specified, please specify only one ID.",
          i = out$id,
          x = this_id
        ))
      }
      out$id <- this_id
    } else {
      if (!is.null(out$element)) {
        rlang::abort("Multiple elements were specified, please specify only one element.")
      }
      if (!m_part %in% names(htmltools::tags)) {
        rlang::abort(glue::glue("Uknown tag used in markup: `{m_part}`"))
      }
      out$element <- m_part
    }
  }

  if (!is.null(out$class)) {
    out$class <- paste(out$class, collapse = " ")
  }

  out <- out[intersect(c("item", "element", "class", "id"), names(out))]

  out
}

html_element_rgx <- function() {
  rgx <- paste(names(htmltools::tags), collapse = "|")
  sprintf("^(%s)[#%%.[:alnum:]_-]* ", rgx)
}
