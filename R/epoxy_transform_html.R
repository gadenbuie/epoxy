#' Concise syntax for expressions inside HTML elements
#'
#' @description
#' `epoxy_transform_html()` provides a
#' [pug](https://pughtml.com/what-is-pug-html)-like syntax for expressions in
#' HTML that are wrapped in HTML elements.
#'
#' ## Syntax
#'
#' You can specify the HTML element and its `id` and `class` into which the
#' text of the expression will be placed. The template is to specify the element
#' using the syntax below, followed by the R expression, separated by a space:
#'
#' ```
#' {{ [<element>][#<id> | .<class>...] expr }}
#' ```
#'
#' For example, to place the expression in a `<li>` element with `id = "food"`
#' and `class = "fruit"`, you could write
#'
#' ```
#' {{ li#food.fruit fruit_name }}
#' ```
#'
#' Each item in the HTML template is optional:
#'
#' 1. If a specific HTML element is desired, the element name must be first. If
#'    no element is specified, the default as set by the `element` argument of
#'    [epoxy_transform_html()] will be used.
#'
#' 2. IDs are specified using `#<id>` and only one ID may be present
#'
#' 3. Classes are written using `.<class>` and as many classes as desired are
#'    allowed.
#'
#' If the expression is a vector, the same element container will be used for
#' each item in the vector.
#'
#' Finally, if the expression returns HTML, it will be escaped by default. You
#' can either use [htmltools::HTML()] to mark it as safe HTML in R, or you can
#' write `!!expr` in the inline markup: `{{ li#food.fruit !!fruit_name }}`.
#'
#' @examples
#' epoxy_html("<ul>{{ li letters[1:3] }}</ul>")
#' epoxy_html("<ul>{{ li.alpha letters[1:3] }}</ul>")
#' epoxy_html("<ul>{{ li#my-letter letters[7] }}</ul>")
#'
#' # The default element is used if no element is directly requested
#' epoxy_html("My name starts with {{ .name-letter letters[7] }}")
#'
#' epoxy_html(
#' 	"{{ h3#title title }}",
#' 	title = "Epoxy for HTML"
#' )
#'
#' # If your replacement text contains HTML, it's escaped by default.
#' hello <- "<strong>Hi there!</strong>"
#' epoxy_html("{{ hello }}")
#'
#' # You can use !! inline to mark the text as safe HTML...
#' epoxy_html("{{ !!hello }}")
#' epoxy_html("{{ button !!hello }}")
#'
#' # ...or you can use htmltools::HTML() to mark it as safe HTML in R.
#' hello <- htmltools::HTML("<strong>Hi there!</strong>")
#' epoxy_html("{{ hello }}")
#'
#' @param class `[character()]`\cr Additional classes to be added to the inline
#'   HTML element.
#' @param element `[character()`\cr The default HTML element tag name to be used
#'   when an element isn't specified in the expression.
#' @param collapse `[logical(1)]`\cr If `TRUE`, transformed HTML outputs will be
#'   collapsed into a single character string. This is helpful when you're
#'   including the value of a vector within an outer HTML tag. Use `collapse =
#'   FALSE` to return a vector of HTML character strings instead, which follows
#'   what you'd typically expect from `glue::glue()`, i.e. when you want to
#'   repeat the outer wrapping text for each element of the vector.
#'
#' @inheritParams epoxy_transform_inline
#' @inherit epoxy_transform params return
#'
#' @seealso Used by default in [epoxy_html()]
#' @family epoxy's glue transformers
#' @export
epoxy_transform_html <- function(
	class = NULL,
	element = "span",
	collapse = TRUE,
	transformer = glue::identity_transformer
) {
	function(text, envir) {
		'!DEBUG html {text: "`text`"}'
		markup <- parse_html_markup(text)

		text <- transformer(markup$item, envir)
		if (inherits(text, "html")) {
			markup$as_html <- TRUE
		}

		is_bare_item <- identical(names(markup), c("item", "as_html"))
		if (is_bare_item) {
			# regular glue text, no added html markup
			if (!markup$as_html) {
				text <- escape_html(text)
			}
			return(text)
		}

		tag_name <- markup$element
		if (is.null(tag_name)) tag_name <- element

		html <- lapply(text, function(x) {
			if (markup$as_html) x <- htmltools::HTML(x)
			htmltools::tag(
				tag_name,
				list(class = class, class = markup$class, id = markup$id, x),
				.noWS = c("inside", "outside")
			)
		})

    if (!isTRUE(collapse)) {
			# Return a vector of html character strings
			return(html_chr(vapply(html, format, character(1))))
		}

		# otherwise, collapse length-1 html tags into a single character string
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
		return(parse_placeholder(x))
	}

	# pug-like syntax starts with # (id), . (class), or element name
	has_el_syntax <-
		substr(x, 1, 1) %in% c("#", "%", ".") ||
			grepl(html_element_rgx(), x)

	if (!has_el_syntax) {
		return(parse_placeholder(x))
	}

	x <- strsplit(x, " ")[[1]]
	item_id <- paste(x[-1], collapse = " ")

	rgx_markup <- "(([#%. ]|^)[[:alnum:]_-]+)"
	m <- str_extract_all(x[1], rgx_markup)[[1]]

	out <- parse_placeholder(item_id)

	if (!length(m)) {
		# should have been caught but just in case
		return(out)
	}

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
				rlang::abort(glue::glue("Unknown tag used in markup: `{m_part}`"))
			}
			out$element <- m_part
		}
	}

	if (!is.null(out$class)) {
		out$class <- paste(out$class, collapse = " ")
	}

	keep_names <- c("item", "element", "class", "id", "as_html")
	out <- out[intersect(keep_names, names(out))]

	out
}

parse_placeholder <- function(x) {
	as_html <- grepl("^!!", x)
	list(
		item = sub("^!!", "", x),
		as_html = as_html
	)
}

html_element_rgx <- function() {
	rgx <- paste(names(htmltools::tags), collapse = "|")
	sprintf("^(%s)[#%%.[:alnum:]_-]* ", rgx)
}
