# Concise syntax for expressions inside HTML elements

`epoxy_transform_html()` provides a [pug](https://www.pughtml.com/)-like
syntax for expressions in HTML that are wrapped in HTML elements.

### Syntax

You can specify the HTML element and its `id` and `class` into which the
text of the expression will be placed. The template is to specify the
element using the syntax below, followed by the R expression, separated
by a space:

    {{ [<element>][#<id> | .<class>...] expr }}

For example, to place the expression in a `<li>` element with
`id = "food"` and `class = "fruit"`, you could write

    {{ li#food.fruit fruit_name }}

Each item in the HTML template is optional:

1.  If a specific HTML element is desired, the element name must be
    first. If no element is specified, the default as set by the
    `element` argument of `epoxy_transform_html()` will be used.

2.  IDs are specified using `#<id>` and only one ID may be present

3.  Classes are written using `.<class>` and as many classes as desired
    are allowed.

If the expression is a vector, the same element container will be used
for each item in the vector.

Finally, if the expression returns HTML, it will be escaped by default.
You can either use
[`htmltools::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html)
to mark it as safe HTML in R, or you can write `!!expr` in the inline
markup: `{{ li#food.fruit !!fruit_name }}`.

## Usage

``` r
epoxy_transform_html(
  class = NULL,
  element = "span",
  collapse = TRUE,
  transformer = glue::identity_transformer
)
```

## Arguments

- class:

  `[character()]`  
  Additional classes to be added to the inline HTML element.

- element:

  `[character()`  
  The default HTML element tag name to be used when an element isn't
  specified in the expression.

- collapse:

  `[logical(1)]`  
  If `TRUE`, transformed HTML outputs will be collapsed into a single
  character string. This is helpful when you're including the value of a
  vector within an outer HTML tag. Use `collapse = FALSE` to return a
  vector of HTML character strings instead, which follows what you'd
  typically expect from
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html), i.e.
  when you want to repeat the outer wrapping text for each element of
  the vector.

- transformer:

  The transformer to apply to the replacement string. This argument is
  used for chaining the transformer functions. By providing a function
  to this argument you can apply an additional transformation after the
  current transformation. In nearly all cases, you can let
  [`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md)
  handle this for you. The chain ends when
  [`glue::identity_transformer()`](https://glue.tidyverse.org/reference/identity_transformer.html)
  is used as the `transformer`.

## Value

A function of `text` and `envir` suitable for the `.transformer`
argument of
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

## See also

Used by default in
[`epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy.md)

Other epoxy's glue transformers:
[`epoxy_transform()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform.md),
[`epoxy_transform_inline()`](http://pkg.garrickadenbuie.com/epoxy/reference/epoxy_transform_inline.md)

## Examples

``` r
epoxy_html("<ul>{{ li letters[1:3] }}</ul>")
#> <ul><li>a</li><li>b</li><li>c</li></ul>
epoxy_html("<ul>{{ li.alpha letters[1:3] }}</ul>")
#> <ul><li class="alpha">a</li><li class="alpha">b</li><li class="alpha">c</li></ul>
epoxy_html("<ul>{{ li#my-letter letters[7] }}</ul>")
#> <ul><li id="my-letter">g</li></ul>

# The default element is used if no element is directly requested
epoxy_html("My name starts with {{ .name-letter letters[7] }}")
#> My name starts with <span class="name-letter">g</span>

epoxy_html(
  "{{ h3#title title }}",
  title = "Epoxy for HTML"
)
#> <h3 id="title">Epoxy for HTML</h3>

# If your replacement text contains HTML, it's escaped by default.
hello <- "<strong>Hi there!</strong>"
epoxy_html("{{ hello }}")
#> &lt;strong&gt;Hi there!&lt;/strong&gt;

# You can use !! inline to mark the text as safe HTML...
epoxy_html("{{ !!hello }}")
#> <strong>Hi there!</strong>
epoxy_html("{{ button !!hello }}")
#> <button><strong>Hi there!</strong></button>

# ...or you can use htmltools::HTML() to mark it as safe HTML in R.
hello <- htmltools::HTML("<strong>Hi there!</strong>")
epoxy_html("{{ hello }}")
#> <strong>Hi there!</strong>
```
