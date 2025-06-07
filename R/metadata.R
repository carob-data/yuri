

read_metadata <- function(uri, path) {

	if (is.null(path)) {
		path <- file.path(tempdir(), "carob")
	}
	dataset_id <- yuri::simpleURI(uri)
	jf <- file.path(path, paste0(dataset_id, ".json"))
	if (file.exists(jf)) {
		x <- jsonlite::fromJSON(readLines(jf))
	} else {
		jf <- file.path(path, "datapackage.json")
		x <- jsonlite::fromJSON(readLines(jf, warn=FALSE))
	}
	x
}


setv <- function(x) {
	ifelse(is.null(x), as.character(NA), as.character(x))
}

setp <- function(x) {
	if (is.null(x)) return(as.character(NA))
	paste(x, collapse="; ")
}

meta_dataverse <- function(x, uri) {
	lic <- get_license(x)

	vers <- x$data$latestVersion$versionNumber 
	if (!is.null(vers)) {
		minor <- x$data$latestVersion$versionMinorNumber 
		if (!is.null(minor)) {
			vers <- paste0(vers, ".", minor)
		}
	} 

	i<- which(x$data$latestVersion$metadataBlocks$citation$fields$typeName == "title")
	if (length(i) > 0) {
		titl <- x$data$latestVersion$metadataBlocks$citation$fields$value[[i]]
	} else {
		titl <- as.character(NA)
	}

	i <- which(x$data$latestVersion$metadataBlocks$citation$fields$typeName == "dsDescription")
	if (length(i) > 0) {
		desc <- x$data$latestVersion$metadataBlocks$citation$fields$value[[i]][[1]]$value
	} else {
		desc <- as.character(NA)
	}

	i <- which(x$data$latestVersion$metadataBlocks$citation$fields$typeName == "author")
	if (length(i) > 0) {
		aut <- x$data$latestVersion$metadataBlocks$citation$fields$value[[i]]$authorName$value
		aut <- paste(aut, collapse="; ")
	} else {
		aut <- as.character(NA)
	}

	data.frame(
		uri = uri,
		dataset_id = yuri::simpleURI(uri),
		license = lic,
		title = titl,
		authors = setp(aut),
		data_published = setv(x$data$publicationDate),
		data_organization = as.character(NA),
		data_publisher = setv(x$data$publisher),
		version = setv(vers),
		description = desc,
		design = as.character(NA)
	)

}


meta_CKAN <- function(x, uri) {

	aut <- x$result$creator
	i <- grep("contributor_person$|contributor_person_*.[0-9]$", names(x$result))	
	r <- unlist(x$result[i])
	add <- r[order(names(r))]
	aut <- c(aut, add)

	data.frame(
		uri = uri,
		dataset_id = yuri::simpleURI(uri),
		license = get_license(x),
		title = setv(x$result$title),
		authors = setp(aut),
		data_published = setv(x$result$creation_date),
		data_organization = as.character(NA),
		data_publisher = setv(x$result$publisher),
		version = setv(x$result$version),
		description = setv(x$result$notes),
		design = as.character(NA)
	)
}

cleaner <- function(x) {
	x <- gsub("\u201C|\u201D|\u2018|\u2019", "'", x)
	x <- gsub("<p>|<p class=\"MsoNormal\">|</p>|\r\n$|\n$", "", x)
	gsub('<span lang=\"EN-US\">|</span>', "", x)
}

meta_zenodo <- function(x, uri) {
	data.frame(
		uri = uri,
		dataset_id = yuri::simpleURI(uri),
		license = get_license(x),
		title = setv(x$metadata$title),
		authors = setp(x$metadata$creators$name),
		data_published = setv(x$metadata$publication_date),
		data_organization = setp(unique(x$metadata$creators$affiliation)),
		data_publisher = "zenodo.org",
		version = setv(x$revision),
		description = cleaner(setv(x$metadata$description)),
		design = as.character(NA)
	)
}



meta_dryad <- function(x, uri) {
	aut <- x$authors
	if (!is.null(aut)) {
		aut <- paste0(aut$lastName, ", ", aut$firstName)
	}

	data.frame(
		uri = uri,
		dataset_id = yuri::simpleURI(uri),
		license = get_license(x),
		title = cleaner(setv(x$title)),
		authors = setp(aut),
		data_published = setv(x$publicationDate),
		data_organization = setp(unique(x$authors$affiliation)),
		data_publisher = "dryad.org",
		version = setv(x$versionNumber),
		description = cleaner(setv(x$abstract)),
		design = cleaner(setv(x$methods))
	)
	
}


get_type <- function(x) {
	nms <- names(x)
	if (all(c("status", "data") %in% nms)) return("dataverse")
	if (all(c("help", "success", "result") %in% nms)) return("CKAN")
	if ("_links" %in% nms) return("dryad")
	if (all(c("created", "doi") %in% nms)) return("zenodo")
	"other"
}


get_citation <- function(m, uri) {
	titl <- gsub("\\.\\.$", ".", paste0(m$title, "."))
	year <- substr(m$data_published, 1, 4)
	if (is.na(year)) year <- "????"
	v <- ifelse(is.na(m$version), "Not versioned. ", paste0("Version ", m$version, ". "))
	pb <- ifelse(is.na(m$data_publisher), "", paste(m$data_publisher, ". "))
	cit <- paste0(m$authors, " (", year, "). ", titl, " ", pb, v, uri)
	gsub("\\. \\.", ". ", cit)
}


extract_metadata <- function(uri, path) {
	js <- read_metadata(uri, path)
	type <- get_type(js)
	if (type == "dataverse") {
		m <- meta_dataverse(js, uri)
	} else if (type == "CKAN") {
		m <- meta_CKAN(js, uri)
	} else if (type == "dryad") {
		m <- meta_dryad(js, uri)
	} else if (type == "zenodo") {
		m <- meta_zenodo(js, uri)
	} else {
		m <- meta_mix(js, uri)
	}
	m$data_citation <- get_citation(m, uri)
	m
}

