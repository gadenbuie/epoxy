#' Epoxy HTML Output for Shiny
#'
#' A glue-like output for Shiny. `ui_epoxy_html()` lets you use placeholders in
#' your HTML such as `"{{first_name}}"`, that are provided values from the
#' server by giving `render_epoxy()` a `first_name` value. Unlike
#' [ui_epoxy_mustache()], updates are highly targeted: only the regions where
#' the server-side data have changed are updated in `ui_epoxy_html()`.
#'
#' @section HTML Markup: By default, placeholders are inserted into a `<span>`
#' element in your UI, with the classes specified in `.class_item`.
#'
#' `ui_epoxy_html()` also supports an HTML markup syntax similar to
#' [pug](https://pughtml.com/what-is-pug-html) (an HTML preprocessor). As an
#' example, the markup syntax
#' ```
#' "{{h3.example.basic#basic-three demo}}"
#' ```
#' creates a `demo` placeholder inside the following tag.
#' ```
#' <h3 id="basic-three" class="example basic"></h3>
#' ```
#'
#' The placeholder template string follows the pattern `{{<markup> <name>}}`.
#' The markup syntax comes first, separated from the placeholder name by a
#' space. The HTML element is first, followed by classes prefixed with `.` or
#' and ID prefixed with `#`. The template markup can contain only one element
#' and one ID, but many classes can be specified.
#'
#' By default, the placeholder is assumed to be text content and any HTML
#' in the sent to the placeholder will be escaped --- in other words if you sent
#' `"<strong>word</strong>"`, you'd see that exact literal text in your app,
#' rather than an emboldened **word**. To mark a placeholder as safe to accept
#' HTML, use `!!` before the placeholder, e.g. `{{<markup> !!<name>}}`. So
#' `{{h3 !!demo}}` will create an `<h3>` tag that accepts HTML within it.
#'
#' @examplesIf rlang::is_installed("shiny")
#' library(shiny)
#'
#' ui <- fluidPage(
#' 	h2("ui_epoxy_html demo"),
#' 	ui_epoxy_html(
#' 		.id = "example",
#' 		.class_item = "inner",
#' 		fluidRow(
#' 			tags$div(
#' 				class = "col-xs-4",
#' 				selectInput(
#' 					inputId = "thing",
#' 					label = "What is this {{color}} thing?",
#' 					choices = c("apple", "banana", "coconut", "dolphin")
#' 				)
#' 			),
#' 			tags$div(
#' 				class = "col-xs-4",
#' 				selectInput(
#' 					inputId = "color",
#' 					label = "What color is the {{thing}}?",
#' 					c("red", "blue", "black", "green", "yellow")
#' 				)
#' 			),
#' 			tags$div(
#' 				class = "col-xs-4",
#' 				sliderInput(
#' 					inputId = "height",
#' 					label = "How tall is the {{color}} {{thing}}?",
#' 					value = 5,
#' 					min = 0,
#' 					max = 10,
#' 					step = 0.1,
#' 					post = "ft"
#' 				)
#' 			)
#' 		),
#' 		tags$p(class = "big", "The {{color}} {{thing}} is {{height}} feet tall."),
#' 		# Default values for placeholders above.
#' 		thing = "THING",
#' 		color = "COLOR",
#' 		height = "HEIGHT"
#' 	),
#' 	tags$style(HTML(
#' 		".big { font-size: 1.5em; }
#'      .inner { background-color: rgba(254, 233, 105, 0.5);}
#'      .epoxy-item__placeholder { color: #999999; background-color: unset; }"
#' 	))
#' )
#'
#' server <- function(input, output, session) {
#' 	output$example <- render_epoxy(
#' 		thing = input$thing,
#' 		color = input$color,
#' 		height = input$height
#' 	)
#' }
#'
#' if (interactive()) {
#' 	shinyApp(ui, server)
#' }
#'
#' @eval write_epoxy_example_app("ui_epoxy_html")
#'
#' @param .id The output id
#' @param ... UI elements or text (that will be treated as HTML), containing
#'   template variables. Use named values to provide initial placeholder values.
#' @param .class Classes added to the output div, in addition to `.epoxy-html`
#' @param .class_item Classes added to the `.container` wrapping each template
#'   variable.
#' @param .container The name of the HTML element to be used for the output
#'   element, by default `"div"`.
#' @param .container_item The name of the HTML element to be used for each
#'   template item, by default `"span"`.
#' @param .placeholder Default placeholder if a template variable placeholder
#'   isn't provided.
#' @param .aria_live,.aria_atomic The
#'   [aria-live](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Attributes/aria-live)
#'   and [aria-atomic](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Attributes/aria-atomic)
#'   attribute values for the entire template region. By default, with
#'   `"polite"`, any updates within the region will be announced via screen
#'   readers.
#'
#'   If your template includes changes in lots of disparate areas, it would be
#'   better to set `"aria-live" = "polite"` and `"aria-atomic" = "true"`` on
#'   specific regions that should be announced together. Otherwise, the default
#'   is to announce the entire region within the `ui_epoxy_html()` whenever any
#'   of the values within change. In other words, set `.aria_live = "off"` and
#'   `.aria_atomic = NULL` on the `ui_epoxy_html()` parent item and then set
#'   `"aria-live" = "polite"` and `"aria-atomic" = "true"` on the parent
#'   containers of each region in the app that receives updates.
#'   `ui_epoxy_html()` does targeted updates, changing only the parts of the
#'   UI that have changed.
#' @inheritParams epoxy
#' @inheritParams glue::glue
#'
#' @seealso [ui_epoxy_mustache()], [render_epoxy()]
#' @return An HTML object.
#' @export
ui_epoxy_html <- function(
	.id,
	...,
	.class = NULL,
	.class_item = NULL,
	.container = "div",
	.container_item = "span",
	.placeholder = "",
	.sep = "",
	.open = "{{",
	.close = "}}",
	.na = "",
	.null = "",
	.literal = FALSE,
	.trim = FALSE,
	.aria_live = c("polite", "off", "assertive"),
	.aria_atomic = TRUE
) {
	.container <- match.arg(.container, names(htmltools::tags))
	.container_item <- match.arg(.container_item, names(htmltools::tags))

	.aria_live <- rlang::arg_match(.aria_live)
	.aria_atomic <- if (!is.null(.aria_atomic)) {
		if (isTRUE(.aria_atomic)) "true" else "false"
	}

	dots <- rlang::list2(...)
	dots$.placeholder <- .placeholder
	dots$.transformer <- epoxyHTML_transformer(.class_item, .container_item)
	dots$.na <- .na
	dots$.sep <- .sep
	dots$.null <- .null
	dots$.trim <- .trim
	dots$.open <- .open %||% "{{"
	dots$.close <- .close %||% "}}"
	# disable # as comment so we can use it for id syntax (requires glue >= 1.5)
	dots$.comment <- character()
	dots$.literal <- .literal
	dots$.envir <- new.env(parent = emptyenv())

	tags <- purrr::keep(dots, is_tag)
	deps <- if (length(tags)) {
		purrr::flatten(purrr::map(tags, htmltools::findDependencies))
	}

	dots <- purrr::map_if(dots, ~ inherits(.x, "shiny.tag"), format)

	res <- rlang::eval_bare(rlang::call2(glue::glue, !!!dots))

	out <- htmltools::tags[[.container]](
		id = .id,
		class = "epoxy-html epoxy-init",
		class = .class,
		"aria-atomic" = .aria_atomic,
		"aria-live" = .aria_live,
		htmltools::HTML(res),
		htmltools::htmlDependency(
			name = "epoxy",
			version = "0.0.1",
			package = "epoxy",
			src = "srcjs",
			script = "output-epoxy.js",
			all_files = FALSE
		)
	)
	if (!is.null(deps) && length(deps)) {
		htmltools::attachDependencies(out, deps)
	} else {
		out
	}
}

#' Epoxy Markdown Template for Shiny
#'
#' Create reactive HTML from a Markdown template. `ui_epoxy_markdown()` uses the
#' same template syntax as [ui_epoxy_html()], but rather than requiring HTML
#' inputs, you can write in markdown. The template is first rendered from
#' markdown to HTML using [pandoc::pandoc_convert()] (if \pkg{pandoc} is
#' available) or [commonmark::markdown_html()] otherwise.
#'
#' @param ... Unnamed arguments are treated as lines of markdown text, and named
#'   arguments are treated as initial values for templated variables.
#' @param .markdown_fn The function used to convert the markdown to HTML. This
#'   function is passed the markdown text as a character vector for the first
#'   argument and any additional arguments from the list `.markdown_args`. By
#'   default, we use [pandoc::pandoc_convert()] if \pkg{pandoc} is available,
#'   otherwise we use [commonmark::markdown_html()].
#' @param .markdown_args A list of arguments to pass to
#'   [commonmark::markdown_html()].
#' @inheritParams ui_epoxy_html
#'
#' @seealso [ui_epoxy_html()], [ui_epoxy_mustache()], [render_epoxy()]
#' @return An HTML object.
#' @export
ui_epoxy_markdown <- function(
	.id,
	...,
	.markdown_fn = NULL,
	.markdown_args = list(),
	.class = NULL,
	.class_item = NULL,
	.container = "div",
	.container_item = "span",
	.placeholder = "",
	.sep = "",
	.open = "{{",
	.close = "}}",
	.na = "",
	.null = "",
	.literal = FALSE,
	.trim = FALSE,
	.aria_live = c("polite", "off", "assertive"),
	.aria_atomic = TRUE
) {

	dots <- list_split_named(rlang::dots_list(...))
	lines <- dots[["unnamed"]]
	dots <- dots[["named"]]

	if (is.null(lines)) {
		rlang::abort(
			"You must provide at least one line of markdown text in `...` as an unnamed character string or vector."
		)
	}

	if (is.null(.markdown_fn)) {
		.markdown_fn <- function(lines, ...) {
			if (rlang::is_installed("pandoc")) {
				x <- pandoc::pandoc_convert(text = lines, to = "html", ...)
				return(paste(x, collapse = "\n"))
			}

			rlang::check_installed("commonmark", "for converting markdown to HTML")
			commonmark::markdown_html(lines, ...)
		}
	}

	html <- rlang::exec(.markdown_fn, lines, !!!.markdown_args)

	ui_epoxy_html(
		.id,
		htmltools::HTML(html),
		!!!dots,
		.class = .class,
		.class_item = .class_item,
		.container = .container,
		.container_item = .container_item,
		.placeholder = .placeholder,
		.sep = .sep,
		.open = .open,
		.close = .close,
		.na = .na,
		.null = .null,
		.literal = .literal,
		.trim = .trim,
		.aria_live = .aria_live,
		.aria_atomic = .aria_atomic
	)
}

#' @describeIn ui_epoxy_html `r lifecycle::badge('deprecated')` Deprecated
#'   alias, please use `ui_epoxy_html()`.
#' @export
epoxyHTML <- function(.id, ...) {
	lifecycle::deprecate_soft(
		"0.1.0",
		"epoxyHTML()",
		"ui_epoxy_html()",
		details = "`epoxyHTML()` was renamed. Please use the new name at your earliest convenience."
	)
	ui_epoxy_html(.id, ...)
}

transformer_js_literal <- function(text, envir) {
	paste0("${", text, "}")
}

epoxyHTML_transformer <- function(
	class = NULL,
	element = "span"
) {
	function(text, envir) {
		'!DEBUG epoxyHTML {text: "`text`"}'
		markup <- parse_html_markup(text)
		placeholder <- rlang::env_get(
			markup$item,
			env = envir,
			inherit = TRUE,
			default = get0(".placeholder", envir, inherits = FALSE)
		)
		if (!is.null(placeholder)) {
			placeholder <- htmltools::HTML(placeholder)
		}
		tag_name <- markup$element
		if (is.null(tag_name)) tag_name <- element
		htmltools::tag(
			tag_name,
			list(
				class = "epoxy-item__placeholder",
				class = class,
				class = markup$class,
				id = markup$id,
				`data-epoxy-item` = markup$item,
				`data-epoxy-as-html` = tolower(markup$as_html %||% FALSE),
				`data-epoxy-placeholder` = placeholder,
				placeholder
			)
		)
	}
}

#' Epoxy HTML Mustache Template
#'
#' A Shiny output that uses [mustache templating](https://mustache.github.io/)
#' to render HTML. Mustache is a powerful template language with minimal
#' internal logic. The advantage of `ui_epoxy_mustache()` is that all parts of
#' the HTML can be templated -- including element attributes -- whereas
#' [ui_epoxy_html()] requires that the dynamic template variables appear in the
#' text portion of the UI. The downside is that the entire template is
#' re-rendered (in the browser), each time that updated data is sent from the
#' server -- unlike [ui_epoxy_html()], whose updates are specific to the parts
#' of the data that have changed.
#'
#' @examplesIf rlang::is_installed("shiny")
#' library(shiny)
#'
#' ui <- fluidPage(
#' 	fluidRow(
#' 		style = "max-width: 600px; margin: 0 auto",
#' 		column(
#' 			width = 6,
#' 			ui_epoxy_mustache(
#' 				id = "template",
#' 				h2(class = "{{heading_class}}", "Hello, {{name}}!"),
#' 				"{{#favorites}}",
#' 				p("Your favorite fruits are..."),
#' 				tags$ul(HTML("{{#fruits}}<li>{{.}}</li>{{/fruits}}")),
#' 				"{{/favorites}}",
#' 				"{{^favorites}}<p>Do you have any favorite fruits?</p>{{/favorites}}"
#' 			)
#' 		),
#' 		column(
#' 			width = 6,
#' 			h2("Inputs"),
#' 			textInput("name", "Your name"),
#' 			textInput("fruits", "Favorite fruits", placeholder = "apple, banana"),
#' 			helpText("Enter a comma-separated list of fruits.")
#' 		)
#' 	)
#' )
#'
#' server <- function(input, output, session) {
#' 	user_name <- reactive({
#' 		if (!nzchar(input$name)) return("user")
#' 		input$name
#' 	})
#'
#' 	favorites <- reactive({
#' 		if (!nzchar(input$fruits)) return(NULL)
#' 		list(fruits = strsplit(input$fruits, "\\s*,\\s*")[[1]])
#' 	})
#'
#' 	output$template <- render_epoxy(
#' 		name = user_name(),
#' 		heading_class = if (user_name() != "user") "text-success",
#' 		favorites = favorites()
#' 	)
#' }
#'
#' if (interactive()) {
#' 	shiny::shinyApp(ui, server)
#' }
#'
#' @eval write_epoxy_example_app("ui_epoxy_mustache")
#'
#' @param id The ID of the output.
#' @param ... Character strings of HTML or [htmltools::tags]. All elements
#'   should be unnamed.
#' @param .file A path to a template file. If provided, no other template lines
#'   should be included in `...`.
#' @param .sep The separator used to concatenate elements in `...`.
#' @param .container A character tag name, e.g. `"div"` or `"span"`, or a
#'   function that returns an [htmltools::tag()].
#'
#' @return Returns a Shiny output UI element.
#'
#' @seealso [ui_epoxy_html()], [render_epoxy()]
#' @export
ui_epoxy_mustache <- function(
	id,
	...,
	.file = NULL,
	.sep = "",
	.container = "div"
) {
	rlang::check_dots_unnamed()

	if (is.character(.container)) {
		tag_name <- .container
		.container <- function(...) {
			htmltools::tags[[tag_name]](..., "aria-live" = "polite")
		}
	}

	dots <- rlang::list2(...)
	if (length(dots) == 0) {
		if (is.null(.file)) return(NULL)
		dots <- as.list(readLines(.file))
	} else {
		if (!is.null(.file)) {
			rlang::abort("Cannot specify both `...` and `.file`.")
		}
	}

	tags <- purrr::keep(dots, is_tag)
	deps <- purrr::flatten(purrr::map(tags, htmltools::findDependencies))

	dots <- purrr::map_if(dots, is_tag, format)
	dots <- purrr::flatten_chr(dots)

	if (!purrr::every(dots, is.character)) {
		rlang::abort("All template elements in `...` must be characters or htmltools tags.")
	}

	out <- .container(
		id = id,
		class = "epoxy-mustache",
		`data-epoxy-template` = paste(dots, collapse = .sep),
		epoxy_mustache_dependencies()
	)

	if (!is.null(deps) && length(deps)) {
		htmltools::attachDependencies(out, deps)
	} else {
		out
	}
}

#' @describeIn ui_epoxy_mustache An alias for `ui_epoxy_mustache()`, provided
#'   because R users are more familiar with this syntax via the \pkg{whisker}
#'   package.
#' @export
ui_epoxy_whisker <- ui_epoxy_mustache

epoxy_mustache_dependencies <- function() {
	htmltools::tagList(
		htmltools::htmlDependency(
			name = "mustache",
			package = "epoxy",
			version = "4.2.0",
			src = "lib/mustache",
			script = "mustache.min.js",
			all_files = FALSE
		),
		htmltools::htmlDependency(
			name = "epoxy-mustache",
			package = "epoxy",
			version = "0.0.1",
			src = "srcjs",
			script = "output-epoxy-mustache.js",
			all_files = FALSE
		)
	)
}


#' Render Epoxy Output
#'
#' Server-side render function used to provide values for template items. Use
#' named values matching the template variable names in the associated
#' [ui_epoxy_html()] or [ui_epoxy_mustache()]. When the values are updated by
#' the app, `render_epoxy()` will update the values shown in the app's UI.
#'
#' @examplesIf rlang::is_installed("shiny")
#' # This small app shows the current time using `ui_epoxy_html()`
#' # to provide the HTML template and `render_epoxy()` to
#' # update the current time every second.
#'
#' ui <- shiny::fluidPage(
#' 	shiny::h2("Current Time"),
#' 	ui_epoxy_html(
#' 		"time",
#' 		shiny::p("The current time is {{strong time}}.")
#' 	)
#' )
#'
#' server <- function(input, output, session) {
#' 	current_time <- shiny::reactive({
#' 		shiny::invalidateLater(1000)
#' 		strftime(Sys.time(), "%F %T")
#' 	})
#'
#' 	output$time <- render_epoxy(time = current_time())
#' }
#'
#' if (rlang::is_interactive()) {
#' 	shiny::shinyApp(ui, server)
#' }
#'
#' @eval write_epoxy_example_app("render_epoxy")
#'
#' @param ... Named values corresponding to the template variables created with
#'   the associated [ui_epoxy_html()] UI element.
#' @param .list A named list or a [shiny::reactiveValues()] list with names
#'   corresponding to the template variables created with the associated
#'   [ui_epoxy_html()] UI element.
#' @param env The environment in which to evaluate the `...`
#' @param outputArgs A list of arguments to be passed through to the implicit
#'   call to [ui_epoxy_html()] when `render_epoxy` is used in an interactive R
#'   Markdown document.
#' @param outputFunc Either [ui_epoxy_html()] or [ui_epoxy_mustache()], i.e. the
#'   UI function to be paired with this output. This is only used when calling
#'   `render_epoxy()` in an Shiny runtime R Markdown document and when you
#'   are only providing the output without an explicit, corresponding UI
#'   element.
#'
#' @return A server-side Shiny render function that should be assigned to
#'   Shiny's `output` object and named to match the `.id` of the corresponding
#'   [ui_epoxy_html()] call.
#'
#' @seealso [ui_epoxy_html()], [ui_epoxy_mustache()]
#' @export
render_epoxy <- function(
	...,
	.list = NULL,
	env = parent.frame(),
	outputFunc = ui_epoxy_html,
	outputArgs = list()
) {
	rlang::check_installed("shiny")

	epoxyPrepare <- function(..., .list = NULL) {
		if (!is.null(.list)) {
			if (inherits(.list, "reactivevalues")) {
				.list <- shiny::reactiveValuesToList(.list)
			}
			if (!is.list(.list)) {
				stop("`.list` must be a list", call. = FALSE)
			}
			if (is.null(names(.list))) {
				stop("`.list` must be a named list", call. = FALSE)
			}
		}
		dots <- rlang::enquos(...)
		dots <- purrr::map(dots, function(x) {
			tryCatch(rlang::eval_tidy(x), error = identity)
	  })
		errored <- c()
		for (i in seq_along(dots)) {
			if (rlang::cnd_inherits(dots[[i]], "error")) {
				errored <- c(errored, names(dots)[i])
				dots[[i]] <- conditionMessage(dots[[i]])
			}
		}

		data <- lapply(c(dots, .list), format_tags)
		if (length(errored)) {
			data[["__errors__"]] <- I(errored)
		}
		data
	}

	shiny::installExprFunction(
		name = "epoxyPrepare",
		quoted = FALSE,
		expr = epoxyPrepare(..., .list = .list)
	)

	shiny::createRenderFunction(
		func = epoxyPrepare,
		transform = function(value, session, name, ...) {
			value <- as.list(value)
			stopifnot(!is.null(names(value)))
			value
		},
		outputFunc = outputFunc,
		outputArgs = outputArgs
	)
}

#' @describeIn render_epoxy `r lifecycle::badge('deprecated')` Deprecated alias,
#'   please use `render_epoxy()`.
#' @export
renderEpoxyHTML <- function(..., env = parent.frame()) {
	lifecycle::deprecate_soft("0.1.0", "renderEpoxyHTML()", "render_epoxy()")
	render_epoxy(..., env = env)
}

format_tags <- function(x) {
	if (!inherits(x, c("shiny.tag", "shiny.tag.list"))) {
		return(x)
	}
	format(x)
}


write_epoxy_example_app <- function(name, fn_name = paste0(name, "()")) {
	rd_path <- paste0(file.path("man", name), ".Rd")
	ex_path <- file.path("inst", "examples", name, "app.R")
	dir.create(dirname(ex_path), showWarnings = FALSE, recursive = TRUE)

	tools::Rd2ex(rd_path, out = ex_path)
	ex <- readLines(ex_path, warn = FALSE)
	idx_start <- min(grep("## End(Don't show)", ex, fixed = TRUE))
	idx_end <- max(grep("shinyApp", ex, fixed = TRUE))
	if (is.infinite(idx_end)) return("")
	if (nzchar(ex[idx_start])) idx_start <- idx_start + 1
	if (ex[idx_start] == "library(shiny)") idx_start <- idx_start + 1
	app_lines <- c(
		glue("# Generated from example in {fn_name}: do not edit by hand"),
		"library(shiny)",
		"library(epoxy)",
		"",
		ex[idx_start:(idx_end - 3) + 1],
		trimws(ex[idx_end])
	)
	writeLines(app_lines, ex_path)
	c(
		"\n",
		"@examplesIf rlang::is_interactive()",
		sprintf("run_epoxy_example_app(\"%s\")", name),
		""
	)
}

#' Example epoxy Shiny apps
#'
#' Run an example epoxy Shiny app showcasing the Shiny UI and server components
#' provided by epoxy.
#'
#' @examples
#' # List examples by passing `name = NULL`
#' run_epoxy_example_app(name = NULL)
#'
#' @param name Name of the example, currently one of `"ui_epoxy_html"`,
#'   `"ui_epoxy_mustache"`, or `"render_epoxy"`.
#' @inheritParams shiny::runApp
#' @inheritDotParams shiny::runApp -display.mode
#'
#' @return Runs the Shiny example app interactively. Nothing is returned.
#'
#' @seealso [ui_epoxy_html()], [ui_epoxy_mustache()], [render_epoxy()]
#' @export
run_epoxy_example_app <- function(
	name = c("ui_epoxy_html", "ui_epoxy_mustache", "render_epoxy"),
	display.mode = "showcase",
	...
) {
	rlang::check_installed("shiny")
	apps <- list.dirs(
		system.file("examples", package = "epoxy"),
		recursive = FALSE
	)
	names(apps) <- basename(apps)
	if (is.null(name)) {
		rlang::inform(c("Example app options:", names(apps)))
		return(invisible(apps))
	}
	name <- rlang::arg_match(name, names(apps))
	if (identical(Sys.getenv("TESTTHAT"), "true")) {
		return(apps[[name]])
	}
	shiny::runApp(apps[[name]], display.mode = display.mode, ...)
}
