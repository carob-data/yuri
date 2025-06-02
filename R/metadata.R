

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


get_version <- function(x) {
	v <- x$data$latestVersion$versionNumber 
	if (!is.null(v)) {
		minor <- x$data$latestVersion$versionMinorNumber 
		if (!is.null(minor)) {
			v <- paste0(v, ".", minor)
		}
	}
	if (is.null(v)) {
		v <- x$versionNumber 
	}
	if (is.null(v)) {
		v <- x$revision
	}
	if (is.null(v)) {
		v <- x$result$version
	}
	if (is.null(v)) {
		v <- as.character(NA)
	}
	v
}


get_license <- function(x) {

	if (!is.null(x$metadata$license$id)) { # Zenodo
		return(toupper(x$metadata$license$id))
	}

	if (!is.null(x$licenses$name)) { # Rothamsted
		return(x$licenses$name)
	}
  
	if (!is.null(x$license)) { # Dryad
		lic <- x$license
		if (substr(lic, 1, 4) == "http") {
			lic <- gsub(".html$", "", basename(lic))
		}
		return(lic)
	}

	lic <- x$data$latestVersion$license
	trms <- x$data$latestVersion$termsOfUse
	if (is.null(trms)) trms <- x$license
	if (isTRUE(grepl("This dataset is made available without information", trms))) {
		return("not specified")
	}
	
#Creative Commons Attribution 4.0 
	
	if ((is.null(lic) || (lic[1] == "NONE")) && (!is.null(trms))) {
		trm <- strsplit(trms, '\"')[[1]]
		g <- grep("/creativecommons.org/|/licensebuttons.net", tolower(trm), value=TRUE)
		if (length(g) == 0) {
			g <- grep("Creative Commons", trm, value=TRUE, ignore.case=TRUE)
			if (length(g) == 0) {
				g <- grep("by-nc-nd", trm, value=TRUE, ignore.case=TRUE)
				if (length(g) > 0) {
					return("CC-BY-NC-ND")
				}
				g <- grep("by-nc-sa", trm, value=TRUE, ignore.case=TRUE)
				if (length(g) > 0) {
					return("CC-BY-NC-SA")
				}
				if (grepl("CIMMYT|CSISA", trms)) {
					return("CIMMYT license")
				}
			}
			if (grepl("by.4.0|CC-BY-4.0|", trms, ignore.case=TRUE)) {
				return("CC-BY-4.0")
			}
			if (grepl("CC0-1.0", trm, ignore.case=TRUE)) {
				return("CC0-1.0")
			}
  			gg <- unlist(regmatches(g, gregexpr('Creative (.+?) license', g, ignore.case=TRUE)))
			if (any(tolower(gg) == "creative commons attribution 4.0 international license")) {
				gg <- "CC-BY-4.0"
			} 
			return(	gsub("-DEED.AST", "", gg[1]) )
		} else {
			trm <- g[1]
			trm <- gsub("http://", "", trm)
			trm <- gsub("https://", "", trm)
			trm <- gsub("creativecommons.org/licenses", "CC", trm)
			trm <- gsub("licensebuttons.net/l", "CC", trm)
			trm <- gsub("creativecommons.org/publicdomain/zero", "CC0", trm)	
			trm <- gsub("/88x31.png", "", trm)
			trm <- gsub("/", "-", trm)
			trm <- toupper(gsub("-$", "", trm))
			trm <- toupper(trm)
			trm <- gsub(" ", "-", trm)			
		} 
		if (nchar(trm) > 0) {
			if (is.null(lic) || (lic == "NONE")) {
				lic <- trm	
			} else {
				lic <- paste0(lic, "; ", trm)
			}
		}
	} else if (is.null(lic)) { #ckan
		lic <- x$result$license_id 	
		if (is.null(lic)) lic <- "?"
		lic <- toupper(lic)
	}
	
	if (is.list(lic)) {
		lic <- lapply(lic, function(x) gsub(" ", "-", gsub("CC-ZERO", "CC-0", x)))
		if ((length(lic) > 1) && ("name" %in% names(lic))) {
			lic <- lic$name
		}
	} else {
		lic <- gsub(" ", "-", gsub("CC-ZERO", "CC0", lic))
	}
	if (is.null(lic)) lic <- "unknown"
	
	gsub("-DEED.AST", "", lic)
}


get_title <- function(x) {
	i <- which(x$data$latestVersion$metadataBlocks$citation$fields$typeName == "title")
	out <- NULL
	if (length(i) > 0) {
		out <- x$data$latestVersion$metadataBlocks$citation$fields$value[[i]]
	}
	if (is.null(out)) { 
		#ckan
		out <- x$result$title
	}
	if (is.null(out)) {
		#zenodo
		out <- x$metadata$title
	}
	if (is.null(out)) { 
		# dryad; Rothamsted
		out <- x$title
	}
	if (is.null(out)) {
		out <- as.character(NA)
	}
	out
}



get_description <- function(x) {
	i <- which(x$data$latestVersion$metadataBlocks$citation$fields$typeName == "dsDescription")
	out <- NULL
	if (length(i) > 0) {
		out <- x$data$latestVersion$metadataBlocks$citation$fields$value[[i]][[1]]$value
	}
	if (is.null(out)) {
		#ckan
		out <- x$result$notes
	}
	if (is.null(out)) {
		#dryad
		out <- gsub("<p>|</p>", "", x$abstract)
		if (nchar(out) < 2) out <- NULL
	}
	if (is.null(out)) {
		#zenodo
		out <- x$metadata$description
	}
	if (is.null(out)) {
		#rothamsted
		out <- x$description
	}
	if (is.null(out)) {
		out <- as.character(NA)
	}
	
	out <- gsub("\u201C", "'", out)
	out <- gsub("\u201D", "'", out)
	out <- gsub("\u2018", "'", out)
	out <- gsub("\u2019", "'", out)
	gsub("<p>|</p>", "'", out)
}


get_authors <- function(x) {

	i <- which(x$data$latestVersion$metadataBlocks$citation$fields$typeName == "author")
	out <- NULL
	if (length(i) > 0) {
		out <- x$data$latestVersion$metadataBlocks$citation$fields$value[[i]]$authorName$value
	}
	if (is.null(out)) {
		r <- x$result
		if (!is.null(r)) {
			out <- r$creator
			i <- grep("contributor_person$|contributor_person_*.[0-9]$", names(r))	
			r <- unlist(r[i])
			add <- r[order(names(r))]
			out <- c(out, add)
		}
	}
	#zenodo
	if (is.null(out)) {
		out <- x$metadata$creators$name
	}
	#dryad 
	if (is.null(out)) {
		out <- x$authors
		out <- paste0(out$lastName, ", ", out$firstName)
	}
	#Rothamsted
	if (is.null(out)) {
		out <- x$contributors$title
	}
	if (is.null(out)) {
		out <- as.character(NA)
	}
	paste(out, collapse="; ")
}


extract_metadata <- function(uri, path) {

	js <- read_metadata(uri, path)
	
	lic <- get_license(js)
	if (is.null(lic)) {
		warning("no license found")
		lic <- as.character(NA)
	}

	authors <- get_authors(js)
	auth <- paste(authors, collapse="; ")
	titl <- gsub("\\.\\.$", ".", paste0(get_title(js), "."))

	pubdate <- c(js$data$publicationDate, js$result$creation_date, js$publicationDate, js$metadata$publication_date)
	if (is.null(pubdate)) pubdate <- "????-??-??"
	year <- substr(pubdate, 1, 4)

	v <- get_version(js)
	if (!is.null(v)) {
		vv <- paste0("Version ", v, ". ")
	} 
	
	
	pub <- c(js$data$publisher, js$result$publisher) 
	if (is.null(pub)) {
		if (grepl("zenodo", uri)) pub <- "Zenodo"
	}
	cit <- paste0(auth, " (", year, "). ", titl, " ", pub, ". ", vv, uri)

	data.frame(
		uri = uri,
		dataset_id = yuri::simpleURI(uri),
		license = lic,
		title = titl,
		authors = authors,
		data_published = pubdate,
		version = v,
		description = get_description(js),
		data_citation = cit
	)
}






