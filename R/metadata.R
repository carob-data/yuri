

read_metadata <- function(uri, path) {
	if (is.null(path)) {
		path <- file.path(tempdir(), "carob")
	}
	dataset_id <- yuri::simpleURI(uri)
	jf1 <- file.path(path, dataset_id, paste0(dataset_id, ".json"))
	if (file.exists(jf1)) {
		return(jsonlite::fromJSON(readLines(jf1)))
	} 
	jf2 <- file.path(path, "datapackage.json")
	if (file.exists(jf2)) {
		return(jsonlite::fromJSON(readLines(jf2, warn=FALSE)))
	}
	stop(paste("cannot find:\n", jf1, "\nor\n", jf2))
}


setv <- function(x) {
	ifelse(is.null(x), as.character(NA), trimws(as.character(x)))
}

setp <- function(x) {
	if (is.null(x)) return(as.character(NA))
	paste(na.omit(trimws(x)), collapse="; ")
}



cleaner <- function(x) {
	b <- "Ab1Ba2Ab3"
	x <- gsub("\u201C|\u201D|\u2018|\u2019", "'", x)
	x <- gsub("\n\n|<br><br>|</br></br>|\r\n\r\n", b, x)
	x <- gsub("\n|\r", " ", x)
#	x <- gsub("<br>|</br>|<sub>|</sub>|<em>|</em>|<i>|</i>|<b>|</b>|<div>|</div>|</p>|<p>|</h.>|<h.>|</a>", "", x)
#	x <- gsub('<a href=".*?">', "", x)
#	x <- gsub("<p.*?>", "", x)
#	x <- trimws(gsub('<span lan g=\"EN-US\">|</span>', "", x))
	x <- gsub("<.*?>", "", x)
	x <- gsub(paste0("(", b, ")+"), b, x)
	gsub(b, "\n\n", gsub(paste0(b, "$"), "", x))
}

meta_dataverse <- function(x) {
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
		aut <- unique(x$data$latestVersion$metadataBlocks$citation$fields$value[[i]]$authorName$value)
		aff <- na.omit(unique(x$data$latestVersion$metadataBlocks$citation$fields$value[[i]]$authorAffiliation$value))
		# "producer"
	} else {
		aff <- aut <- as.character(NA)
	}
	
	if (is.null(aff) || isTRUE(is.na(aff))) {
		i <- which(x$data$latestVersion$metadataBlocks$citation$fields$typeName == "producer")
		if (length(i) > 0) {
			aff <- x$data$latestVersion$metadataBlocks$citation$fields$value[[i]]$producerName$value
			aff <- unique(aff)
		}
	}

	doi <- as.character(NA)
	i <- which(x$data$latestVersion$metadataBlocks$citation$fields$typeName == "publication")
	if (length(i) > 0) {
		d <- x$data$latestVersion$metadataBlocks$citation$fields$value[[i]]
		i <- which(d$publicationRelationType$value == "Cites")
		d <- d[-i, ]
		d <- grep("doi|hdl", d, value=TRUE)
		if (length(d) > 0) {
			doi <- sapply(d, yuri::simpleURI)	
		}
	}
	
	design <- as.character(NA)
	d <- x$data$latestVersion$metadataBlocks$socialscience$fields
	if (is.data.frame(d)) {
		flds <- c("typeName", "value")
		if (all(flds %in% names(d))) {
			design <- paste0(apply(d[,flds], 1, \(x) paste0(x[1], ": ", x[2])), collapse="; ")
		}
	}

	data.frame(
		license = lic,
		title = titl,
		authors = setp(aut),
		publication = setp(doi),
		date_published = setv(x$data$publicationDate),
		data_organization = setp(aff),
		publisher = setv(x$data$publisher),
		version = setv(vers),
		description = cleaner(desc),
		design = design
	)
}


meta_CKAN <- function(x) {

	aut <- x$result$creator
	i <- grep("contributor_person$|contributor_person_*.[0-9]$", names(x$result))	
	r <- unlist(x$result[i])
	aut <- c(aut, r[order(names(r))])

	i <- grep("contributor_projectlead_institute", names(x$result))	
	aff1 <- unique(unlist(x$result[i]))
	i <- grep("contributor.*affiliation", names(x$result))	
	aff <- unique(c(aff1, unlist(x$result[i])))
	aff <- trimws(aff[!grepl("Not Applicable", aff, ignore.case=TRUE)])
	aff <- aff[aff != ""]
	if (length(aff) == 0) aff <- NULL
	

	v <- x$data$latestVersion$versionNumber
	minor <- x$data$latestVersion$versionMinorNumber
	if (!is.null(minor)) {
		v <- paste0(v, ".", minor)
	}

	data.frame(
		license = get_license(x),
		title = setv(x$result$title),
		authors = setp(aut),
		publication = as.character(NA),
		date_published = setv(x$result$creation_date),
		data_organization = setp(aff),
		publisher = setv(x$result$publisher),
		version = setv(v),
		description = setv(x$result$notes),
		design = as.character(NA)
	)
}



meta_zenodo <- function(x) {
	data.frame(
		license = get_license(x),
		title = setv(x$metadata$title),
		authors = setp(x$metadata$creators$name),
		publication = as.character(NA),
		date_published = setv(x$metadata$publication_date),
		data_organization = setp(unique(x$metadata$creators$affiliation)),
		publisher = "zenodo.org",
		version = setv(x$revision),
		description = cleaner(setv(x$metadata$description)),
		design = as.character(NA)
	)
}



meta_dryad <- function(x) {
	aut <- x$authors
	if (!is.null(aut)) {
		aut <- paste0(aut$lastName, ", ", aut$firstName)
	}
	pub <- x$relatedWorks
	if (NROW(pub) > 0) {
		doipub <- pub[pub$identifierType == "DOI", ]
		if (nrow(doipub) > 0) {
			pub <- sapply(doipub$identifier, \(i) simpleURI(simpleURI(i), reverse=TRUE))
			pub <- paste0(pub, collapse="; ")
		} else {
			pub <- paste0(pub$identifier, collapse="; ")		
		}
	} else {
		pub <- as.character(NA)
	}
	data.frame(
		license = get_license(x),
		title = cleaner(setv(x$title)),
		authors = setp(aut),
		publication = pub,
		date_published = setv(x$publicationDate),
		data_organization = setp(unique(x$authors$affiliation)),
		publisher = "dryad.org",
		version = setv(x$versionNumber),
		description = cleaner(setv(x$abstract)),
		design = cleaner(setv(x$methods))
	)
	
}


meta_figshare <- function(x, path) {

	flics <- file.path(path, "licenses.txt")
	if (file.exists(flics)) {
		licenses <- setp(readLines(flics))
	} else {
		licenses <- as.character(NA)
	}

	data.frame(
		license = licenses,
		title = setv(x$title),
		authors = setp(x$authors$full_name),
		publication = setp(x$references),
		date_published = setv(x$modified_date),  # created_date
		data_organization = setp(unique(x$institution_id)),
		publisher = "figshare.com",
		version = setv(x$version),
		description = cleaner(setv(x$description)),
		design = as.character(NA)
	)
}


get_type <- function(x) {
	nms <- names(x)
	if (all(c("status", "data") %in% nms)) return("dataverse")
	if (all(c("help", "success", "result") %in% nms)) return("CKAN")
	if ("_links" %in% nms) return("dryad")
	if (all(c("created", "doi") %in% nms)) return("zenodo")
	if (grepl("figshare", x$url)) return("figshare") 
	"other"
}


get_citation <- function(m, uri) {
	titl <- gsub("\\.\\.$", ".", paste0(m$title, "."))
	year <- substr(m$date_published, 1, 4)
	if (is.na(year)) year <- "????"
	v <- ifelse(is.na(m$version), "Not versioned. ", paste0("Version ", m$version, ". "))
	pb <- ifelse(is.na(m$publisher), "", paste0(m$publisher, ". "))
	cit <- paste0(m$authors, " (", year, "). ", titl, " ", pb, v, uri)
	gsub("\\. \\.|\\.\\.", ". ", cit)
}


extract_metadata <- function(uri, path) {

	dataset_id <- simpleURI(uri)
	uri <- simpleURI(dataset_id, reverse=TRUE)

	js <- read_metadata(uri, path)
	type <- get_type(js)
	if (type == "dataverse") {
		m <- meta_dataverse(js)
	} else if (type == "CKAN") {
		m <- meta_CKAN(js)
	} else if (type == "dryad") {
		m <- meta_dryad(js)
	} else if (type == "zenodo") {
		m <- meta_zenodo(js)
	} else if (type == "figshare") {
		m <- meta_figshare(js, file.path(path, dataset_id))
	} else {
		m <- meta_mix(js, uri)
	}
	data.frame(
		uri = uri,
		dataset_id = dataset_id,
		data_citation = get_citation(m, uri),
		m
	)
}

