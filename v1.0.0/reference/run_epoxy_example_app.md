# Example epoxy Shiny apps

Run an example epoxy Shiny app showcasing the Shiny UI and server
components provided by epoxy.

## Usage

``` r
run_epoxy_example_app(
  name = c("ui_epoxy_html", "ui_epoxy_markdown", "ui_epoxy_mustache", "render_epoxy"),
  display.mode = "showcase",
  ...
)
```

## Arguments

- name:

  Name of the example, currently one of `"ui_epoxy_html"`,
  `"ui_epoxy_markdown"`, `"ui_epoxy_mustache"`, or `"render_epoxy"`.

- display.mode:

  The mode in which to display the application. If set to the value
  `"showcase"`, shows application code and metadata from a `DESCRIPTION`
  file in the application directory alongside the application. If set to
  `"normal"`, displays the application normally. Defaults to `"auto"`,
  which displays the application in the mode given in its `DESCRIPTION`
  file, if any.

- ...:

  Arguments passed on to
  [`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html)

  `appDir`

  :   The application to run. Should be one of the following:

      - A directory containing `server.R`, plus, either `ui.R` or a
        `www` directory that contains the file `index.html`.

      - A directory containing `app.R`.

      - An `.R` file containing a Shiny application, ending with an
        expression that produces a Shiny app object.

      - A list with `ui` and `server` components.

      - A Shiny app object created by
        [`shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

  `port`

  :   The TCP port that the application should listen on. If the `port`
      is not specified, and the `shiny.port` option is set (with
      `options(shiny.port = XX)`), then that port will be used.
      Otherwise, use a random port between 3000:8000, excluding ports
      that are blocked by Google Chrome for being considered unsafe:
      3659, 4045, 5060, 5061, 6000, 6566, 6665:6669 and 6697. Up to
      twenty random ports will be tried.

  `launch.browser`

  :   If true, the system's default web browser will be launched
      automatically after the app is started. Defaults to true in
      interactive sessions only. The value of this parameter can also be
      a function to call with the application's URL.

  `host`

  :   The IPv4 address that the application should listen on. Defaults
      to the `shiny.host` option, if set, or `"127.0.0.1"` if not. See
      Details.

  `workerId`

  :   Can generally be ignored. Exists to help some editions of Shiny
      Server Pro route requests to the correct process.

  `quiet`

  :   Should Shiny status messages be shown? Defaults to FALSE.

  `test.mode`

  :   Should the application be launched in test mode? This is only used
      for recording or running automated tests. Defaults to the
      `shiny.testmode` option, or FALSE if the option is not set.

## Value

Runs the Shiny example app interactively. Nothing is returned.

## See also

[`ui_epoxy_html()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/ui_epoxy_html.md),
[`ui_epoxy_markdown()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/ui_epoxy_markdown.md),
[`ui_epoxy_mustache()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/ui_epoxy_mustache.md),
[`render_epoxy()`](http://pkg.garrickadenbuie.com/epoxy/v1.0.0/reference/render_epoxy.md)

## Examples

``` r
# List examples by passing `name = NULL`
run_epoxy_example_app(name = NULL)
#> Example app options:
#> • render_epoxy
#> • ui_epoxy_html
#> • ui_epoxy_markdown
#> • ui_epoxy_mustache
#> • word-list
```
