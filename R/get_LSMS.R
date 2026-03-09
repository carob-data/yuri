



get_LSMS <- function(uri, path, username, password, cache=TRUE) {

	fok <- file.path(path, "ok.txt")
	if (cache && file.exists(fok)) {
		return(list.files(path, recursive=TRUE, full.names=TRUE))
	}
	if (is.null(username) | is.null(password)) {
		return(NULL)
	}


	dir.create(path, FALSE, FALSE)
	lsms_login = "https://microdata.worldbank.org/index.php/auth/login"
	p <- httr::POST(lsms_login, body = list('email' = username, 'password' = password))
	if (p$status != 200) {
		stop(paste0("bad POST response, status=", p$status, ". invalid email/password?"))
	} else if (!(is.null(p$headers$refresh))) {
		if (grepl("login", p$headers$refresh)) {
			stop("invalid email/password")
		}
	}
	uhtml <- paste0("https://doi.org/", gsub("doi:", "", uri))
	z <- httr::GET(uhtml)

	zurl <- gsub("0;url=", "", z$headers$refresh)
	url <- paste0(zurl, "/get-microdata")
	r <- httr::content(httr::GET(url), as="text")
	x <- unlist(strsplit(gsub("\t|\r", "", r), "\n"))
	i <- grep("Data in CSV", x)
	if (length(i) == 0) {
		i <- grep("Data in ASCII", x)
	}
	if (length(i) == 0) {
		haveData <- FALSE
		warning("these files need to be downloaded manually")
	} else {
		haveData <- TRUE
		x <- x[i:(i+10)]
		i <- grep("href=", x)
		x <- x[i:(i+1)]
		x <- gsub("href=|title=", "", x)
		durl <- paste0(trimws(gsub('\"', "", x)), collapse="/")
		g <- httr::GET(durl)
		fout <- file.path(path, basename(durl))
		writeBin(httr::content(g, "raw"), fout)
		utils::unzip(fout, exdir = path)
	}
	
	url <- paste0(zurl, "/study-description")
	r <- httr::content(httr::GET(url), as="text")
	writeLines(r, file.path(path, "study-description.html"))

	url <- paste0(zurl, "/related-materials")
	r <- httr::content(httr::GET(url), as="text")
	x <- trimws(unlist(strsplit(gsub("\t|\r", "", r), "\n")))
	x <- x[grep(paste0('href=\"', zurl, "/download"), x)]
	p <- grep("pdf", x, value=TRUE)
	s <- sapply(strsplit(p, 'title=\"'), function(i) i[[2]])
	z <- strsplit(s, '\" href=\"')	
	pdf <- sapply(z, function(i) i[[1]])
	updf <- sapply(z, function(i) strsplit(i[[2]], '\">')[[1]][1])	
	fpdf <- gsub(" ", "%20", paste0(updf, "/", pdf))
	dp <- file.path(path, "docs")
	dir.create(dp, FALSE, FALSE)
	for (f in fpdf) {
		utils::download.file(f, file.path(dp, basename(f)), mode="wb", quiet=TRUE)
	}
	
	writeLines(c(utils::timestamp(quiet=TRUE), uri), fok)

	if (haveData) {
		return(list.files(path, recursive=TRUE, full.names=TRUE))
	} else {
		return(NULL)
	}
}



LSMS_metadata <- function(uri, group, path, major, minor, ...) {

	suri <- yuri::simpleURI(uri)
	mf <- readLines(file.path(path, "data/raw", group, suri, "study-description.html"))
	tit <- gsub("<title>|</title>", "", grep("<title>", mf, value=TRUE))
	dat <- trimws(mf[grep("Date of Metadata Production", mf)+2])
	dat <- gsub("<p>|</p>|<span>|</span>", "", dat)
	des <- trimws(grep("Abstract", mf, value=TRUE)[1])
	des <- strsplit(des, "Geographic coverage")[[1]][1]
	des <- gsub('\"|\\\\r|\\\\n', "", des)
	des <- gsub("description: Abstract---------------------------", "", des)

	m <- data.frame(
		dataset_id = suri,
		uri=uri,
		group=group,
		license="acknowledge source, no redistribution, report data use",
		title= tit,
		authors=NA,
		publication=NA,
		date_published=dat,
		description=des,
		data_citation=NA,
		project = "LSMS-ISA",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none"
	)
	
	d <- data.frame(list(...))
	d$draft <- NULL
	
	if (nrow(d) == 1) {
		m[names(d)] <- d
	} else if (nrow(d) > 1) {
		warning("additional arguments must all have length 1")
	}
	m
		
}

