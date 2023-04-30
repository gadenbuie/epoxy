abc <- c("a", "b", "c")

epoxy("{abc}", .style = epoxy_style_wrap("'"))

epoxy("{abc}", .style = epoxy_style_bold())

epoxy("{abc}", .style = epoxy_style_italic())

epoxy("{abc}", .style = epoxy_style_code())

epoxy("{abc}", .style = epoxy_style_apply(toupper))
