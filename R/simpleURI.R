
.removeprotocol <- function(x) gsub("http://|https://|www\\.", "", x)

simpleURI <- function(uri, reverse=FALSE) {
  
	if (reverse) {
		if (grepl(":", uri)) {
			return(gsub("_", "/", uri))
		} else {
			return(gsub("_", "/", sub("_", ":", uri))	)
		}
	}
	
	ur <- .removeprotocol(uri)
	if (grepl("dx.doi.org/", ur)) {
		u <- gsub("dx.doi.org/", "", ur)
		u <- paste0("doi_", u)
	} else if (grepl("doi.org/", ur)) {
		u <- gsub("doi.org/", "", ur)
		u <- paste0("doi_", u)
	} else if (grepl("persistentId=doi:", ur)) {
		u <- unlist(strsplit(ur, "persistentId=doi:"))[2]
		u <- paste0("doi_", u)
	} else if (grepl("^doi:", ur)) {
		u <- gsub("^doi:", "doi_", ur)		
	} else if (grepl("persistentId=hdl:", ur)) {
		u <- unlist(strsplit(ur, "persistentId=hdl:"))[2]
		u <- paste0("hdl_", u)
	} else if (grepl("^hdl:", ur)) {
		u <- gsub("^hdl:", "hdl_", ur)		
	} else if (grepl("hdl.handle.net/", ur)) {
		u <- gsub("hdl.handle.net/", "", ur)
		u <- paste0("hdl_", u)
	} else {
		return(NULL) #stop(paste0("Not a valid object identifier (DOI or HDL)"))
	}
	gsub("/", "_", u)
}

