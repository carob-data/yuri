
.removeprotocol <- function(x) gsub("http://|https://|www\\.", "", x)

one_simple_uri <- function(uri, reverse, warn=TRUE) { 
	if (reverse) {
		if (grepl(":", uri)) {
			return(gsub("_", "/", uri))
		} else {
			return(gsub("_", "/", sub("_", ":", uri))	)
		}
	}
	
	ur <- .removeprotocol(uri)
	w <- FALSE
	if (grepl("dx.doi.org/", ur)) {
		u <- gsub("dx.doi.org/", "", ur)
		w <- TRUE
		u <- paste0("doi_", u)
	} else if (grepl("doi.org/", ur)) {
		u <- gsub("doi.org/", "", ur)
		u <- paste0("doi_", u)
		w <- TRUE
	} else if (grepl("persistentId=doi:", ur)) {
		u <- unlist(strsplit(ur, "persistentId=doi:"))[2]
		u <- paste0("doi_", u)
		w <- TRUE
	} else if (grepl("^doi:", ur)) {
		u <- gsub("^doi:", "doi_", ur)		
	} else if (grepl("persistentId=hdl:", ur)) {
		u <- unlist(strsplit(ur, "persistentId=hdl:"))[2]
		u <- paste0("hdl_", u)
		w <- TRUE
	} else if (grepl("^hdl:", ur)) {
		u <- gsub("^hdl:", "hdl_", ur)		
		w <- TRUE
	} else if (grepl("hdl.handle.net/", ur)) {
		u <- gsub("hdl.handle.net/", "", ur)
		u <- paste0("hdl_", u)
		w <- TRUE
	} else {
		return(NULL) #stop(paste0("Not a valid object identifier (DOI or HDL)"))
	}
	if (warn && w) {
		warning(paste(uri, "is not a valid URI. It should be:", gsub("_", ":", u)), call.=FALSE)
	}
	gsub("/", "_", u)
}

simpleURI <- function(uri, reverse=FALSE, warn=TRUE) {
	sapply(uri, \(u) one_simple_uri(u, reverse[1], warn[1]), USE.NAMES=FALSE)
}

