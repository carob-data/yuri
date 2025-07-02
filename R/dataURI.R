# Author: Robert J. Hijmans
# Sept 2019
# version 1
# license 	


filter_files <- function(x) { 
	x <- grep("\\.json$|\\.pdf$|\\.doc$|\\.docx$|\\.zip$", x, value=TRUE, invert=TRUE)
	# remove opened excel files
	grep("/~$", x, fixed=TRUE, invert=TRUE, value=TRUE)
}


writeOK <- function(path, uu) {
	stamp <- utils::timestamp(quiet=TRUE)
	writeLines(c(stamp, uu), file.path(path, "ok.txt"))
}



.download_dataverse_files <- function(u, baseu, path, uname, domain, protocol, unzip, zipf) {
	pid <- unlist(strsplit(u, "\\?"))[2]
	uu <- paste0(baseu, "/api/datasets/:persistentId?", pid)
	
	# the nice way
	#r <- httr::GET(uu)
	#httr::stop_for_status(r)
	#js <- httr::content(r, as = "text", encoding = "UTF-8")
	# but for cimmyt...
	tmpf <- tempfile()
	
	if (grepl("worldagroforestry", uu) || grepl("cirad.fr", uu) || grepl("cipotato", uu)) {
		# temporary fix because WorldAgroFor https cert has expired
		# not sure why for CIP on Ubuntu (cert expired)
		utils::download.file(uu, tmpf, quiet=TRUE, method="curl", extra="-k", mode="wb")
	} else {
		utils::download.file(uu, tmpf, quiet=TRUE, mode="wb")
	}
	js <- readLines(tmpf, encoding = "UTF-8", warn=FALSE)
	js <- jsonlite::fromJSON(js)
	fjs <- js$data$latestVersion$files
	jsp <- jsonlite::toJSON(js, pretty=TRUE)
	writeLines(jsp, file.path(path, paste0(uname, ".json")))
	f <- if(is.null(fjs$dataFile)) {fjs$datafile} else {fjs$dataFile}
	f$checksum <- NULL
	f$tabularTags <- NULL
	if (!is.null(f$categories)) {
		fc <- unlist(f$categories)
		if (!is.list(fc) && length(fc) == nrow(f)) {
			f$categories <- fc
		} else {
			f$categories <- NULL		
		}
	}
	fn <- file.path(path, paste0(uname, "_files.txt"))
	try(utils::write.csv(f, fn))
	rest <- f$restricted
	if (!is.null(rest)) {
		f <- f[!rest, ]
		if (nrow(f) == 0) {
			stop("access to the files is restricted")
		}
		warning("access to some files is restricted")
	}
	if (nrow(f) == 0) {
		stop("no files!")
	}
	
	if (sum(f$originalFileSize, na.rm=TRUE) < 10000000) {
		files <- paste0(f$id, collapse = ",")
		fu <- paste0(protocol, domain, "/api/access/datafiles/", files, "?format=original")
	## temporary fix because WorldAgroFor https cert has expired
		if (grepl("worldagroforestry", fu) || grepl("cirad.fr", fu) || grepl("cipotato", fu)) {
			utils::download.file(fu, zipf, quiet=TRUE, mode="wb", method="curl", extra="-k")
		} else {
			utils::download.file(fu, zipf, mode="wb", quiet=TRUE)
		}
	} else {
		#for (i in 1:nrow(f)) {
		#	print(paste("part", i)); utils::flush.console()
		#	fu <- paste0(protocol, domain, "/api/access/datafiles/", f$id[i], "?format=original")
		#	zipi <- file.path(path, paste0(uname, "_", i, ".zip"))
		#	utils::download.file(fu, zipi, mode="wb", quiet=TRUE)
## temporary fix because WorldAgroFor https cert has expired
##			utils::download.file(fu, zipi, quiet=TRUE, mode="wb", method="curl", extra="-k")
		#	zipf <- c(zipf, zipi)
		#}
	
		f$originalFileSize[is.na(f$originalFileSize)] <- 10000
		i <- 1
		zipf <- NULL
		while(TRUE) {
#			print(paste("part", i)); utils::flush.console()
			if (!is.null(f$originalFileSize)) {
				cs <- cumsum(f$originalFileSize)
			} else {
				cs <- cumsum(f$filesize)
			}
			k <- which (cs < 9000000)
			if (length(k) == 0) k <- 1
			files <- paste0(f$id[k], collapse = ",")
			fu <- paste0(protocol, domain, "/api/access/datafiles/", files, "?format=original")
			zipi <- file.path(path, paste0(uname, "_", i, ".zip"))
			if (grepl("worldagroforestry", uu)  || grepl("cirad.fr", fu) || grepl("cipotato", fu)) {
## temporary fix for expired https certificates
				utils::download.file(fu, zipi, quiet=TRUE, mode="wb", method="curl", extra="-k")
			} else {
				utils::download.file(fu, zipi, mode="wb", quiet=TRUE)
			}
			f <- f[-k,]
			zipf <- c(zipf, zipi)
			if (nrow(f) == 0) break
			i <- i + 1
		}
	}	
	if (unzip) {
		ff <- .dataverse_unzip(zipf, path)
		f7 <- list.files(path, pattern="\\.7z$", full.names=TRUE)
		ff <- .dataverse_unzip(f7, path)
	}

	writeOK(path, uu)
	list.files(file.path(path), full.names = TRUE, recursive=TRUE)
}


.download_ckan_files <- function(u, baseu, path, uname, unzip, overwrite=TRUE) {
	pid <- unlist(strsplit(u, "dataset/"))[2]
	uu <- paste0(baseu, "/api/3/action/package_show?id=", pid)
	y <- httr::GET(uu)
	if (y$status_code != 200) {
		return(NULL)
	}
	
	ry <- httr::content(y, as="raw")
	meta <- rawToChar(ry)
	writeLines(meta, file.path(path, paste0(uname, ".json")))
	js <- jsonlite::fromJSON(meta)
	d <- js$result$resources
	done <- TRUE
	files <- ""[0]
	i <- duplicated(tolower(d$name))
	d$name[i] <- paste0(d$name[i], "_dup")
	i <- duplicated(tolower(d$name))
	d$name[i] <- paste0(d$name[i], "_2")
	
	for (i in 1:nrow(d)) {
		u <- file.path(baseu, "dataset", d$package_id[i], "resource", d$id[i], "download", d$name[i])
		#if (d$available[i] == "yes") { "active" ?
		outf <- file.path(path, d$name[i])
		if ((!overwrite) & file.exists(outf)) next
		ok <- try(utils::download.file(d$url[i], outf, mode="wb", quiet=TRUE), silent=TRUE )
		if (inherits(ok, "try-error")) {
			print("cannot download")
			done <- FALSE
		} else {
			files <- c(files, outf)
		}
	}

	if (done) {
		if (unzip) {
			i <- grepl("\\zip$", files)
			if (any(i)) {
				ff <- files[i]
				for (f in ff) utils::unzip(f, junkpaths=TRUE, exdir=path)
			}
		}
		writeOK(path, uu)
	}
	files
}

.download_dryad_files <- function(u, baseu, path, uname, unzip){ 
	pid <- gsub(":", "%253A", gsub("/", "%252F", unlist(strsplit(u, "dataset/"))[2]))
	uu <- paste0(baseu, "/api/v2/datasets/", pid)
	y <- httr::GET(uu)
	if (y$status_code != 200) {
		return(NULL)
	}
  
	ry <- httr::content(y, as="raw")
	meta <- rawToChar(ry)
	writeLines(meta, file.path(path, paste0(uname, ".json")))
	js <- jsonlite::fromJSON(meta)
	d <- js$id
	done <- TRUE
	outf <- file.path(path, paste0(uname, ".zip"))
	ok <- try(utils::download.file(file.path(uu,"download"), outf, headers = c("User-Agent" = "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1.0.3705; .NET CLR 1.1.4322)"), mode="wb", quiet=TRUE) )
	if (inherits(ok, "try-error")) {
		print("cannot download ", paste0(uname, ".zip"))
		done <- FALSE
	} else {
		if (unzip) {
			utils::unzip(outf, exdir = file.path(path))	
		}
	}
	if (done) writeOK(path, uu)
	list.files(file.path(path), full.names = TRUE)
}

.download_zenodo_files <- function(u, path, uname, unzip){
  
#	pid <- gsub("https://zenodo.org/records/", "", u)
#	uu <- paste0("zenodo.org/api/deposit/depositions/", pid, "/files")
	pid <- basename(u)
	uu <- paste0("https://zenodo.org/api/records/", pid)
	y <- httr::GET(uu)
	if (y$status_code != 200) {
		return(NULL)
	}
	ry <- httr::content(y, as="raw")
	meta <- rawToChar(ry)
	writeLines(meta, file.path(path, paste0(uname, ".json")))
	js <- jsonlite::fromJSON(meta)
	# d <- js$links$download
	d <- unlist(js$files$links)
	d <- gsub("/draft", "", d)
	done <- TRUE
	files <- ""[0]
	
	outz <- file.path(path, paste0(uname, ".zip"))
	#dir.create(file.path(path, uname))
	#outf <- file.path(path, uname)
	for (link in d) {
		outf <- file.path(path, basename(gsub("/content", "", link)))
		ok <- try(utils::download.file(link, outf, mode="wb", quiet=TRUE))
		if (inherits(ok, "try-error")) {
			message(paste("cannot download", uname))
			done <- FALSE
		} else {
			files <- c(files, outf)
		}
	}
	## utils::zip can fail if Sys.getenv("R_ZIPCMD", "zip") returns an empty string
	#utils::zip(outz, list.files(outf, full.names = TRUE), flags = "-q")
	#utils::unzip(outz, junkpaths = TRUE, exdir=path)
	#unlink(file.path(path, uname), recursive = TRUE, force = TRUE)
	

	if (done) {
		if (unzip) {
			i <- grepl("\\zip$", files)
			if (any(i)) {
				ff <- files[i]
				for (f in ff) utils::unzip(f, junkpaths=TRUE, exdir=path)
			}
		}
		writeOK(path, uu)
	}

	list.files(path, full.names = TRUE)
}


download_size <- function(url) as.numeric(httr::HEAD(url)$headers[["content-length"]])

.download_figshare_files <- function(u, path, uname, unzip){

	pid <- basename(u)
	uu <- paste0("https://api.figshare.com/v2/collections/", pid)
  
	y <- httr::GET(uu)
	if (y$status_code != 200) {
		return(NULL)
	}
	ry <- httr::content(y, as="raw")
	meta <- rawToChar(ry)
	writeLines(meta, file.path(path, paste0(uname, ".json")))

	uu <- paste0(uu, "/articles?page=1&page_size=100")
	y <- httr::GET(uu)
	if (y$status_code != 200) {
		return(NULL)
	}
	ry <- httr::content(y, as="raw")
	m <- rawToChar(ry)
	dir.create(file.path(path, "_more_metadata"), FALSE, FALSE)
	writeLines(m, file.path(path, "_more_metadata", paste0(uname, "_files.json")))

	js <- jsonlite::fromJSON(m)
	urls <- js$url_public_api
	if (length(urls) == 100) {
		message("Houston, we have a problem")
	}

	licenses <- vector("list", length(urls))
	done <- TRUE
	files <- NULL
	for (i in 1:length(urls)) {
		d <- httr::GET(urls[i])
		d <- httr::content(d, as="raw")
		d <- rawToChar(d)
		js <- jsonlite::fromJSON(d)
		this_url <- js$files$download_url
		if (is.null(this_url)) next
		this_file <- file.path(path, js$files$name)
		licenses[i] <- js$license$name
		for (j in 1:length(this_file)) {
			if (!file.exists(this_file[j])) {
				writeLines(d, file.path(path, "_more_metadata", paste0(uname, "_", js$files$name[j], ".json")))
				message(basename(this_file[j])) 
				utils::flush.console()
				ok <- try(utils::download.file(this_url[j], this_file[j], mode="wb", quiet=TRUE))
				if (inherits(ok, "try-error")) {
					message("   download failed")
					done <- FALSE
					if (file.exists(this_file[j])) file.remove(this_file[j])
				} 
				files <- c(files, this_file[j])
			}
		}
	}
	writeLines(unique(unlist(licenses)), file.path(path, "licenses.txt"))
	
	if (done) {
		if (unzip) {
			i <- grepl("\\.zip$", files)
			if (any(i)) {
				message("   unzipping")
				ff <- files[i]
				for (f in ff) utils::unzip(f, junkpaths=FALSE, exdir=path)
			}
		}
		writeOK(path, uu)
	}

	list.files(path, full.names = TRUE)
}


.download_rothamsted_files <- function(u, path, uname, unzip) {

	uu <- gsub("dataset", "metadata", u)
	bn <- basename(u)
	uu <- gsub("01-", "", uu)
	uu <- paste0(uu, "/", bn, ".zip")
	
	zipf <- file.path(path, basename(uu))
	dir.create(path, showWarnings=FALSE)
	ok <- try(utils::download.file(uu, zipf, mode="wb", quiet=TRUE))
	if (inherits(ok, "try-error")) {
		print("cannot download ", uname)
		done <- FALSE
	} else { 
		done <- TRUE
		if (unzip) {
			utils::unzip(zipf, junkpaths=TRUE, exdir=path)
		}
		writeOK(path, uu)
	}	
	list.files(path, full.names = TRUE)
}


.getdomain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
.getprotocol <- function(x) paste0(strsplit(x, "/")[[c(1, 1)]], "//")


http_address <- function(uri) {
	if (grepl("^doi:", uri)) {
		gsub("^doi:", "https://dx.doi.org/", uri)
	} else if (grepl("^hdl:", uri)) {
		gsub("^hdl:", "https://hdl.handle.net/", uri)
	}
}


dataURI <- function(uri, path, cache=TRUE, unzip=TRUE, recursive=FALSE, filter=TRUE) {

	uname <- simpleURI(uri)
	uri <- simpleURI(uname, reverse=TRUE)
	
	
	#uripath=TRUE
	#if (uripath) 
	path <- file.path(path, uname)
	
	if (!file.exists(file.path(path, "ok.txt"))) {
		cache <- FALSE
	}

	dir.create(path, FALSE, TRUE)

	if (cache && file.exists(file.path(path, "ok.txt"))) {
		ff <- list.files(path, full.names=TRUE, recursive=recursive, include.dirs = !recursive)
		if (filter) ff <- filter_files(ff)
		return(ff)
	}
	
	zipf <- file.path(path, paste0(uname, ".zip"))
	if (cache & file.exists(zipf)) {
		zipf <- list.files(path, paste0(uname, ".*zip$"), full.names=TRUE)		
		ff <- .dataverse_unzip(zipf, path, unzip)
		if (filter) ff <- filter_files(ff)
		return(ff)
	}

	uri <- yuri:::http_address(uri)
	
	if (!file.exists(path)) {
		stop(paste("cannot create path:", path))
	}
	
	# temporary fix because WorldAgroFor https cert has expired
	httr::set_config(httr::config(ssl_verifypeer = 0L))

	# For CIRAD dataverse
	if (grepl("18167", uri)) {
		x <- httr::GET(uri, httr::add_headers("user-agent" = "Mozilla/5.0", "Cache-Control" = "no-cache"))
	} else {
		x <- httr::GET(uri)
	}

	if (!x$status_code %in% c(200, 202)) {
		message(paste("Dataset or resource not reachable.\nStatus code: ", x$status_code))
		return()
	}
	u <- x$url
	domain <- yuri:::.getdomain(u)
	protocol <- yuri:::.getprotocol(u)
	baseu <- paste0(protocol, domain)
	
	if (grepl("/stash/|datadryad", u)) {	
		ff <- .download_dryad_files(u, baseu, path, uname, unzip)
	} else if (grepl("rothamsted", u)) {
		ff <- .download_rothamsted_files(u, path, uname, unzip)
	} else if (grepl("/dataset/", u)) {	
		ff <- .download_ckan_files(u, baseu, path, uname, unzip)
	} else if (grepl("zenodo", u)) {
		ff <- .download_zenodo_files(u, path, uname, unzip)
	} else if (grepl("figshare", u)) {
		ff <- .download_figshare_files(u, path, uname, unzip)
	} else {
		ff <- .download_dataverse_files(u, baseu, path, uname, domain, protocol, unzip, zipf)
	}
	if (filter) ff <- filter_files(ff)
	ff
}


# uri <- "doi:10.5061/dryad.pj76g30"
# path <- getwd()
# group <- "fertilizer"
# ff <- get_data(uri, path, group)


