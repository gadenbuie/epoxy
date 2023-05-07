#' Deprecated: `epoxy_style()`
#'
#' `epoxy_style()` was renamed [epoxy_transform()] in \pkg{epoxy} version 0.1.0.
#'
#' @param ... Passed to the new `epoxy_transform` function.
#'
#' @keywords internal
#' @export
#' @name epoxy_style
NULL

# TODO(lifecycle): epoxy_style functions were deprecated 2023-05-06.

#' @describeIn epoxy_style Renamed [epoxy_transform()].
#' @export
epoxy_style <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style()",
		with = "epoxy_transform()"
	)
	epoxy_transform(...)
}

#' @describeIn epoxy_style Renamed [epoxy_transform_apply()].
#' @export
epoxy_style_apply <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style_apply()",
		with = "epoxy_transform_apply()"
	)
	epoxy_transform_apply(...)
}

#' @describeIn epoxy_style Renamed [epoxy_transform_bold()].
#' @export
epoxy_style_bold <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style_bold()",
		with = "epoxy_transform_bold()"
	)
	epoxy_transform_bold(...)
}

#' @describeIn epoxy_style Renamed [epoxy_transform_code()].
#' @export
epoxy_style_code <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style_code()",
		with = "epoxy_transform_code()"
	)
	epoxy_transform_code(...)
}

#' @describeIn epoxy_style Renamed [epoxy_transform_collapse()].
#' @export
epoxy_style_collapse <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style_collapse()",
		with = "epoxy_transform_collapse()"
	)
	epoxy_transform_collapse(...)
}

#' @describeIn epoxy_style Renamed [epoxy_transform_get()].
#' @export
epoxy_style_get <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style_get()",
		with = "epoxy_transform_get()"
	)
	epoxy_transform_get(...)
}

#' @describeIn epoxy_style Renamed [epoxy_transform_html()].
#' @export
epoxy_style_html <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style_html()",
		with = "epoxy_transform_html()"
	)
	epoxy_transform_html(...)
}

#' @describeIn epoxy_style Renamed [epoxy_transform_inline()].
#' @export
epoxy_style_inline <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style_inline()",
		with = "epoxy_transform_inline()"
	)
	epoxy_transform_inline(...)
}

#' @describeIn epoxy_style Renamed [epoxy_transform_italic()].
#' @export
epoxy_style_italic <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style_italic()",
		with = "epoxy_transform_italic()"
	)
	epoxy_transform_italic(...)
}

#' @describeIn epoxy_style Renamed [epoxy_transform_set()].
#' @export
epoxy_style_set <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style_set()",
		with = "epoxy_transform_set()"
	)
	epoxy_transform_set(...)
}

#' @describeIn epoxy_style Renamed [epoxy_transform_wrap()].
#' @export
epoxy_style_wrap <- function(...) {
	lifecycle::deprecate_warn(
		when = "0.1.0",
		what = "epoxy_style_wrap()",
		with = "epoxy_transform_wrap()"
	)
	epoxy_transform_wrap(...)
}
