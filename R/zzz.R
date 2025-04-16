
.onLoad <- function(libname, pkgname) {
	httr::timeout(15)
	options(timeout = max(300, getOption("timeout")))
}


