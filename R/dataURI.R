# Author: Robert J. Hijmans
# Sept 2019
# version 1
# license 	


.yuri_environment <- new.env(parent=emptyenv())

authenticate <- function(x) {
	stopifnot(ncol(x) == 3)
	stopifnot(all(names(x) == c("service", "username", "password")))
	x$service <- toupper(x$service)
	stopifnot(all(x$service %in% c("DRYAD", "LSMS")))
	for (i in 1:nrow(x)) {
		.yuri_environment[[x[i,1]]] <- list(username=x$username[i], password=x$password[i])
	}
}


filter_files <- function(x) { 
	x <- grep("\\.json$|\\.pdf$|\\.doc$|\\.docx$|\\.zip$|\\.gz$|\\.7z$|\\.tar$|\\.tgz$|\\.tar\\.gz$", x, value=TRUE, invert=TRUE)
	# remove opened excel files
	grep("/~$", x, fixed=TRUE, invert=TRUE, value=TRUE)
}


writeOK <- function(path, uu) {
	stamp <- utils::timestamp(quiet=TRUE)
	writeLines(c(stamp, uu), file.path(path, "ok.txt"))
}

list_files <- function(path, recursive) {
	list.files(path, full.names=TRUE, all.files=TRUE, recursive=recursive, include.dirs=!recursive, no..=TRUE)
}


.dataverse_insecure_dl <- function(uu, fu) {
	grepl("worldagroforestry", uu) || grepl("cirad.fr", uu) || grepl("cipotato", uu) ||
		grepl("worldagroforestry", fu) || grepl("cirad.fr", fu) || grepl("cipotato", fu)
}


.dataverse_pick_guestbook_field <- function(opt, field, env_names, default, arg = NULL) {
	if (!is.null(arg)) {
		ch <- as.character(arg)[1]
		if (nzchar(ch)) return(ch)
	}
	if (length(env_names) > 0) {
		ens <- if (is.list(env_names)) {
			as.character(unlist(env_names, use.names = FALSE))
		} else {
			as.character(env_names)
		}
		for (i in seq_along(ens)) {
			en <- ens[i]
			ev <- Sys.getenv(en, "")
			if (nzchar(ev)) return(ev)
		}
	}
	v <- opt[[field]]
	if (is.null(v) || (length(v) == 1L && !nzchar(as.character(v)))) {
		return(default)
	}
	as.character(v)[1]
}


.dataverse_resolve_api_token <- function(password = NULL) {
	if (!is.null(password)) {
		ch <- as.character(password)[1]
		if (nzchar(ch)) return(ch)
	}
	for (en in c("YURI_DATAVERSE_PASSWORD", "DATAVERSE_API_TOKEN", "DATAVERSE_KEY")) {
		ev <- Sys.getenv(en, "")
		if (nzchar(ev)) return(ev)
	}
	NULL
}


.dataverse_http_get_to_file <- function(url, dest, insecure, api_token) {
	cfg <- if (isTRUE(insecure)) httr::config(ssl_verifypeer = 0L) else NULL
	wd <- httr::write_disk(dest, overwrite = TRUE)
	if (!is.null(api_token) && nzchar(api_token)) {
		r <- httr::GET(url, cfg, wd, httr::add_headers(`X-Dataverse-key` = api_token))
	} else {
		r <- httr::GET(url, cfg, wd)
	}
	if (r$status_code < 200 || r$status_code >= 300) {
		stop("Dataverse request failed (HTTP ", r$status_code, "): ", url)
	}
	invisible(TRUE)
}


.dataverse_guestbook_response_body <- function(guestbook_id, baseu, uu, name_arg = NULL, email_arg = NULL, institute_arg = NULL, api_token = NULL) {
	tmpf <- tempfile()
	on.exit(unlink(tmpf), add = TRUE)
	gbu <- paste0(baseu, "/api/guestbooks/", guestbook_id)
	insecure <- .dataverse_insecure_dl(uu, gbu)
	if (!is.null(api_token) && nzchar(api_token)) {
		.dataverse_http_get_to_file(gbu, tmpf, insecure, api_token)
	} else if (insecure) {
		utils::download.file(gbu, tmpf, quiet = TRUE, mode = "wb", method = "curl", extra = "-k")
	} else {
		utils::download.file(gbu, tmpf, quiet = TRUE, mode = "wb")
	}
	gb_raw <- jsonlite::fromJSON(tmpf)
	if (!identical(gb_raw$status, "OK")) {
		stop("could not retrieve Dataverse guestbook ", guestbook_id)
	}
	gb <- gb_raw$data
	opt <- getOption("yuri.dataverse.guestbook")
	if (!is.list(opt)) opt <- list()
	out <- list(
		name = .dataverse_pick_guestbook_field(opt, "name", c("YURI_DATAVERSE_USERNAME", "DATAVERSE_GUESTBOOK_NAME"), "unspecified", arg = name_arg),
		email = .dataverse_pick_guestbook_field(opt, "email", c("YURI_DATAVERSE_EMAIL", "DATAVERSE_GUESTBOOK_EMAIL"), "guest@localhost", arg = email_arg),
		institution = .dataverse_pick_guestbook_field(opt, "institution", c("YURI_DATAVERSE_INSTITUTE", "DATAVERSE_GUESTBOOK_INSTITUTION"), "unspecified", arg = institute_arg),
		position = .dataverse_pick_guestbook_field(opt, "position", c("YURI_DATAVERSE_POSITION", "DATAVERSE_GUESTBOOK_POSITION"), "", arg = NULL)
	)
	cq <- gb$customQuestions
	if (is.data.frame(cq) && nrow(cq) > 0) {
		if (is.null(opt$answers)) {
			stop("this dataset uses a Dataverse guestbook with custom questions; set options(yuri.dataverse.guestbook = list(..., answers = list(list(id = ID, value = \"...\"), ...)))", call. = FALSE)
		}
		out$answers <- opt$answers
	} else {
		out$answers <- list()
	}
	out
}


.dataverse_dataset_guestbook_id <- function(js) {
	gid <- js$data$guestbookId
	if (is.null(gid) || (length(gid) == 1L && is.na(gid))) {
		gid <- js$data$latestVersion$guestbookId
	}
	if (is.null(gid) || (length(gid) == 1L && is.na(gid))) {
		return(NULL)
	}
	as.integer(gid[[1]])
}


.dataverse_download_access_bundle <- function(fu, dest, uu, guestbook_body, api_token = NULL) {
	insecure <- .dataverse_insecure_dl(uu, fu)
	if (is.null(guestbook_body)) {
		if (!is.null(api_token) && nzchar(api_token)) {
			.dataverse_http_get_to_file(fu, dest, insecure, api_token)
		} else if (insecure) {
			utils::download.file(fu, dest, quiet = TRUE, mode = "wb", method = "curl", extra = "-k")
		} else {
			utils::download.file(fu, dest, mode = "wb", quiet = TRUE)
		}
		return(invisible(TRUE))
	}
	fu_post <- paste0(fu, "&signed=true")
	body <- list(guestbookResponse = guestbook_body)
	cfg <- if (insecure) httr::config(ssl_verifypeer = 0L) else NULL
	post_h <- httr::add_headers(`Content-Type` = "application/json")
	if (!is.null(api_token) && nzchar(api_token)) {
		post_h <- httr::add_headers(`Content-Type` = "application/json", `X-Dataverse-key` = api_token)
	}
	r <- httr::POST(fu_post,
		body = jsonlite::toJSON(body, auto_unbox = TRUE),
		post_h,
		config = cfg
	)
	txt <- httr::content(r, as = "text", encoding = "UTF-8")
	j <- try(jsonlite::fromJSON(txt), silent = TRUE)
	if (inherits(j, "try-error") || is.null(j$status)) {
		stop("Dataverse guestbook POST failed (HTTP ", httr::status_code(r), "): ", substr(txt, 1, 500))
	}
	if (!identical(j$status, "OK")) {
		stop("Dataverse guestbook: ", if (!is.null(j$message)) j$message else txt)
	}
	su <- j$data$signedUrl
	if (is.null(su) || !nzchar(su)) {
		stop("Dataverse did not return a signed download URL after guestbook submission")
	}
	if (!is.null(api_token) && nzchar(api_token)) {
		.dataverse_http_get_to_file(su, dest, insecure, api_token)
	} else if (insecure) {
		utils::download.file(su, dest, quiet = TRUE, mode = "wb", method = "curl", extra = "-k")
	} else {
		utils::download.file(su, dest, mode = "wb", quiet = TRUE)
	}
	invisible(TRUE)
}


.download_dataverse_files <- function(u, baseu, path, uname, domain, protocol, unzip, zipf, recursive=TRUE, name_arg = NULL, email_arg = NULL, institute_arg = NULL, password_arg = NULL) {
	pid <- unlist(strsplit(u, "\\?"))[2]
	uu <- paste0(baseu, "/api/datasets/:persistentId?", pid)
	api_token <- .dataverse_resolve_api_token(password_arg)

	# the nice way
	#r <- httr::GET(uu)
	#httr::stop_for_status(r)
	#js <- httr::content(r, as = "text", encoding = "UTF-8")
	# but for cimmyt...
	tmpf <- tempfile()

	insecure_uu <- grepl("worldagroforestry", uu) || grepl("cirad.fr", uu) || grepl("cipotato", uu)
	if (!is.null(api_token) && nzchar(api_token)) {
		.dataverse_http_get_to_file(uu, tmpf, insecure_uu, api_token)
	} else if (insecure_uu) {
		# temporary fix because WorldAgroFor https cert has expired
		utils::download.file(uu, tmpf, quiet=TRUE, method="curl", extra="-k", mode="wb")
	} else {
		utils::download.file(uu, tmpf, quiet=TRUE, mode="wb")
	}
	js <- readLines(tmpf, encoding = "UTF-8", warn=FALSE)
	if (js[1] == "<?xml version='1.0' encoding='UTF-8' ?>") {
		stop("The Harvard Dataverse API is not available")
	}

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
	gb_id <- .dataverse_dataset_guestbook_id(js)
	guestbook_body <- NULL
	if (!is.null(gb_id)) {
		guestbook_body <- .dataverse_guestbook_response_body(gb_id, baseu, uu, name_arg = name_arg, email_arg = email_arg, institute_arg = institute_arg, api_token = api_token)
	}

	if (sum(f$originalFileSize, na.rm=TRUE) < 10000000) {
		files <- paste0(f$id, collapse = ",")
		fu <- paste0(protocol, domain, "/api/access/datafiles/", files, "?format=original")
		.dataverse_download_access_bundle(fu, zipf, uu, guestbook_body, api_token = api_token)
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
			.dataverse_download_access_bundle(fu, zipi, uu, guestbook_body, api_token = api_token)
			f <- f[-k,]
			zipf <- c(zipf, zipi)
			if (nrow(f) == 0) break
			i <- i + 1
		}
	}	
	if (unzip) {
		ff <- .dataverse_unzip(zipf, path)
		.dataverse_extract_archives(path)
	}

	writeOK(path, uu)
	list_files(path, recursive)
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
#		u <- file.path(baseu, "dataset", d$package_id[i], "resource", d$id[i], "download", d$name[i])
#		#if (d$available[i] == "yes") { "active" ?
#		outf <- file.path(path, d$name[i])
		if (!is.null(d$url)) {
			u <- d$url[i]
			outf <- file.path(path, basename(u))
		} else {
			u <- file.path(baseu, "dataset", d$package_id[i], "resource", d$id[i], "download", d$name[i])
			outf <- file.path(path, d$name[i])
		}
		#if (d$available[i] == "yes") { "active" ?
		
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


get_dryad_token <- function(username=NULL, password=NULL) {		
	dtok <- .yuri_environment$DRYAD$token
	if (!is.null(dtok)) {
		if (is.null(dtok$error)) {
			expires <- as.POSIXct(as.Date("1970-01-01"), tz="") + dtok$created_at + dtok$expires_in
			if (Sys.time() < expires) {
				return(dtok$access_token)
			}
		}
	}
	
	if (is.null(username) || is.null(password)) {
		if (is.null(.yuri_environment$DRYAD$username) || is.null(.yuri_environment$DRYAD$password)) {
			stop("you need to provide your DRYAD username and password or set them with yuri::authenticate")
		}
		username <- .yuri_environment$DRYAD$username
		password <- .yuri_environment$DRYAD$password
	}

	response <- httr::POST("https://datadryad.org/oauth/token",
		body = list(client_id=username, client_secret=password, grant_type="client_credentials"), encode="form")

	dtok <- httr::content(response)
	if (!is.null(dtok$error)) {
		stop(paste("DRYAD: ", dtok$token$error_description))
	} 
	.yuri_environment$DRYAD$token <- dtok
	dtok$access_token
}



.download_dryad_files <- function(u, baseu, path, uname, unzip, recursive=TRUE, username=NULL, password=NULL){ 

	token <- yuri:::get_dryad_token(username, password)

	pid <- gsub(":", "%253A", gsub("/", "%252F", unlist(strsplit(u, "dataset/"))[2]))
	uu <- paste0(baseu, "/api/v2/datasets/", pid)
	y <- httr::GET(uu)
	if (y$status_code != 200) {
		return(NULL)
	}
  
	meta <- httr::content(y, as="raw")
	meta <- rawToChar(meta)
	writeLines(meta, file.path(path, paste0(uname, ".json")))
	js <- jsonlite::fromJSON(meta)
	#d <- js$id
	
	href <- paste0("https://datadryad.org", js[["_links"]][["stash:download"]]$href)	
	res <- httr::GET(href, httr::add_headers(Authorization = paste("Bearer", token)), httr::config(followlocation = TRUE))
	if (res$status_code != 200) {
		msg <- httr::content(res, as="raw")
		msg <- rawToChar(msg)	
		stop(paste0("cannot download ", uname, ".zip\nstatus code = ", res$status_code, ": ", msg), call.=FALSE)
	} else {
		outf <- file.path(path, paste0(uname, ".zip"))
		writeBin(httr::content(res, "raw"), outf)	
		if (unzip) {
			utils::unzip(outf, exdir = file.path(path))	
		}
		writeOK(path, uu)
	}
	list_files(path, recursive)
}

.download_zenodo_files <- function(u, path, uname, unzip, recursive=TRUE){
  
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
	list_files(path, recursive)
}


download_size <- function(url) as.numeric(httr::HEAD(url)$headers[["content-length"]])

.download_figshare_files <- function(u, path, uname, unzip, recursive=TRUE){

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
				message(paste("   ", basename(this_file[j])))
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
	list_files(path, recursive)
}


.download_rothamsted_files <- function(u, path, uname, unzip, recursive=TRUE) {

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
	list_files(path, recursive)
}


.getdomain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
.getprotocol <- function(x) paste0(strsplit(x, "/")[[c(1, 1)]], "//")


http_address <- function(uri) {
	if (grepl("^doi:", uri)) {
		gsub("^doi:", "https://doi.org/", uri)
	} else if (grepl("^hdl:", uri)) {
		gsub("^hdl:", "https://hdl.handle.net/", uri)
	}
}


dataURI <- function(uri, path, cache=TRUE, unzip=TRUE, recursive=FALSE, filter=TRUE, username=NULL, password=NULL, institute=NULL, email=NULL) {

	uname <- yuri::simpleURI(uri)	
	uri <- yuri::simpleURI(uname, reverse=TRUE)
	
	#uripath=TRUE
	#if (uripath) 
	path <- file.path(path, uname)
	
	if (!file.exists(file.path(path, "ok.txt"))) {
		cache <- FALSE
	}

	dir.create(path, FALSE, TRUE)
	if (!file.exists(path)) {
		stop(paste("cannot create path:", path))
	}
	
	if (cache && file.exists(file.path(path, "ok.txt"))) {
		if (unzip) {
			.dataverse_extract_archives(path)
		}
		ff <- list_files(path, recursive)
		if (filter) ff <- filter_files(ff)
		return(ff)
	}
	
	zipf <- file.path(path, paste0(uname, ".zip"))
	if (cache & file.exists(zipf)) {
		zipf <- list.files(path, paste0(uname, ".*zip$"), full.names=TRUE)		
		ff <- .dataverse_unzip(zipf, path, unzip)
		if (isTRUE(unzip)) {
			.dataverse_extract_archives(path)
		}
		ff <- list_files(path, recursive)
		if (filter) ff <- filter_files(ff)
		return(ff)
	}


#	if (isTRUE(grepl("^hdl:11529", uri))) {
#		uri <- paste0("https://data.cimmyt.org/dataset.xhtml?persistentId=", uri)
#	} else {
		uri <- yuri:::http_address(uri)
#	}
	
	# temporary fix because WorldAgroFor https cert has expired
	httr::set_config(httr::config(ssl_verifypeer = 0L))

	# For CIRAD / figshare
	# if (grepl("18167|figshare", uri)) {
	if (isTRUE(grepl("figshare", uri))) {
		x <- httr::GET(uri, httr::add_headers("user-agent" = "Mozilla/5.0", "Cache-Control" = "no-cache"))
	} else {
		x <- httr::GET(uri)
	}

	if (!x$status_code %in% c(200, 202, 307)) {
		message(paste("Dataset or resource not reachable.\nStatus code: ", x$status_code))
		return()
	}
	u <- x$url
	domain <- yuri:::.getdomain(u)
	protocol <- yuri:::.getprotocol(u)
	baseu <- paste0(protocol, domain)
	
	if (grepl("/stash/|datadryad", u)) {	
		ff <- .download_dryad_files(u, baseu, path, uname, unzip, recursive, username, password)
	} else if (grepl("rothamsted", u)) {
		ff <- .download_rothamsted_files(u, path, uname, unzip, recursive)
	} else if (grepl("/dataset/", u)) {	
		ff <- .download_ckan_files(u, baseu, path, uname, unzip, recursive)
	} else if (grepl("zenodo", u)) {
		ff <- .download_zenodo_files(u, path, uname, unzip, recursive)
	} else if (grepl("figshare", u)) {
		ff <- .download_figshare_files(u, path, uname, unzip, recursive)
	} else {
		ff <- .download_dataverse_files(u, baseu, path, uname, domain, protocol, unzip, zipf, recursive,
			name_arg = username, email_arg = email, institute_arg = institute, password_arg = password)
	}
	# 
	if (filter) {
		filter_files(ff)
	} else {
		ff
	}
}


# uri <- "doi:10.5061/dryad.pj76g30"
# path <- getwd()
# group <- "fertilizer"
# ff <- get_data(uri, path, group)


