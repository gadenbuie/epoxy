abc <- c("a", "b", "c")

epoxy("{abc}", .transformer = epoxy_transform_wrap("'"))

epoxy("{abc}", .transformer = epoxy_transform_bold())

epoxy("{abc}", .transformer = epoxy_transform_italic())

epoxy("{abc}", .transformer = epoxy_transform_code())

epoxy("{abc}", .transformer = epoxy_transform_apply(toupper))
