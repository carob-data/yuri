



## NADA (CodeIgniter) sends client-side redirects via the `Refresh:` HTTP
## header (e.g. `Refresh: 0;url=https://...`). libcurl/httr do NOT follow
## those automatically -- only browsers do, by treating it as the equivalent
## of <meta http-equiv="refresh">. As a result, every authenticated POST or
## GET that uses CodeIgniter's `redirect($url, 'refresh')` helper lands us
## on a 200 with `Content-Length: 0` and a Refresh header. This helper
## follows up to `max` such headers using the same handle/cookie jar so we
## end up with the real page content.
.follow_refresh <- function(resp, hndl, max = 5) {
	for (i in seq_len(max)) {
		ref <- resp$headers[["refresh"]]
		if (is.null(ref) || !nzchar(ref)) break
		# Format is `<seconds>;url=<absolute or relative URL>` (case-insensitive).
		m <- regmatches(ref, regexec("(?i)url=(.+)$", ref))[[1]]
		if (length(m) < 2) break
		url <- trimws(m[2])
		# Strip surrounding quotes if present.
		url <- gsub('^["\']|["\']$', "", url)
		if (!nzchar(url)) break
		resp <- httr::GET(url, handle = hndl)
	}
	resp
}

## Save a verbose dump of an httr response: status, headers, body length and
## the raw body (which we then also try to decode as UTF-8 text). Useful when
## the high-level `httr::content(..., as="text")` returns nothing so we can
## tell whether the body really was empty or whether decoding failed.
.lsms_dump <- function(resp, debug_path, name) {
	if (is.null(debug_path)) return(invisible(NULL))
	f <- file.path(debug_path, paste0(name, ".txt"))
	body <- tryCatch(httr::content(resp, as = "raw"), error = function(e) raw(0))
	if (is.null(body)) body <- raw(0)
	text <- tryCatch(rawToChar(body), error = function(e) sprintf("<%d non-text bytes>", length(body)))
	hdrs <- paste(sprintf("%s: %s", names(resp$headers), unlist(resp$headers, use.names = FALSE)),
	              collapse = "\n")
	url <- if (is.null(resp$url)) "?" else resp$url
	cat(sprintf("URL: %s\nStatus: %s\nBody length (bytes): %d\n--- Headers ---\n%s\n--- Body ---\n%s\n",
	            url, resp$status_code, length(body), hdrs, text),
	    file = f)
	# Also save the raw body next to the dump for inspection in a browser.
	writeBin(body, file.path(debug_path, paste0(name, ".bin")))
	invisible(NULL)
}

## Probe whether the current handle is authenticated against the WB microdata
## catalog. NADA's user-bar template (themes/nada52/user-bar.php) only renders
## an `auth/logout` link when `$this->session->userdata('username')` is set,
## so its presence is a reliable "logged in" signal.
.lsms_is_logged_in <- function(hndl, debug_path = NULL) {
	# /index.php redirects (via Refresh:) to /home for everybody, so probe
	# /home directly. The user-bar template only renders the `auth/logout`
	# link for sessions that are actually authenticated.
	r <- httr::GET("https://microdata.worldbank.org/home", handle = hndl)
	r <- .follow_refresh(r, hndl)
	.lsms_dump(r, debug_path, "lsms_probe")
	html <- httr::content(r, as = "text", encoding = "UTF-8")
	if (is.null(html) || !nzchar(html)) return(FALSE)
	grepl('href="[^"]*/auth/logout"', html, perl = TRUE)
}

## Pull the (csrf_name, csrf_value) pair out of any NADA-rendered login form.
## Returns a 2-element character vector or NULL if the form cannot be parsed.
.lsms_extract_csrf <- function(html) {
	if (is.null(html) || !nzchar(html)) return(NULL)
	# Tolerant of arbitrary whitespace between attributes.
	cn <- regmatches(html, regexpr('(?<=name="csrf_name")\\s+value="[^"]+',  html, perl = TRUE))
	cv <- regmatches(html, regexpr('(?<=name="csrf_value")\\s+value="[^"]+', html, perl = TRUE))
	cn <- sub('^\\s*value="', "", cn)
	cv <- sub('^\\s*value="', "", cv)
	if (length(cn) != 1 || length(cv) != 1 || !nzchar(cn) || !nzchar(cv)) return(NULL)
	c(cn, cv)
}

.lsms_login <- function(username, password, hndl, debug_path = NULL) {
	# The World Bank microdata catalog login is a TWO-STEP wizard, both steps
	# CSRF-protected via Slim\Csrf\Guard (see application/libraries/
	# Nada_csrf.php upstream). Each form render mints a fresh one-time token
	# stored in the PHP session; both steps must share the `ihsn_nada` cookie.
	#
	#   Step 1: POST /auth/login   with `email` + csrf_name + csrf_value
	#           -> Refresh: ...    /auth/password
	#   Step 2: POST /auth/password with `password` + csrf_name + csrf_value
	#                              (a NEW pair, freshly minted on the password page)
	#           -> Refresh: ...    /home or original destination
	#
	# Posting `password` to step 1 is silently ignored by NADA, which is why
	# a single-shot POST appears to "succeed" yet leaves the session unauth'd.
	wb            <- "https://microdata.worldbank.org"
	url_email     <- paste0(wb, "/index.php/auth/login")
	url_password  <- paste0(wb, "/index.php/auth/password")

	# --- Step 0: fetch the email form to seed the session + grab CSRF -------
	g <- httr::GET(url_email, handle = hndl)
	.lsms_dump(g, debug_path, "lsms_login_get")
	loginpage <- httr::content(g, as = "text", encoding = "UTF-8")
	if (is.null(loginpage) || !nzchar(loginpage)) {
		stop("LSMS login: empty response from ", url_email, " (status ", g$status_code, ").")
	}
	cj <- tryCatch(curl::handle_cookies(hndl$handle), error = function(e) NULL)
	if (!is.null(cj) && !any(cj$name == "ihsn_nada")) {
		stop("LSMS login: no session cookie set; check that 'httr' and 'curl' are up to date.")
	}
	csrf <- .lsms_extract_csrf(loginpage)
	if (is.null(csrf)) {
		stop("LSMS login: could not extract CSRF token from the email form.")
	}

	# --- Step 1: POST email -------------------------------------------------
	p1 <- httr::POST(url_email,
	                 body = list(email = username, submit = "Login",
	                             csrf_name = csrf[1], csrf_value = csrf[2]),
	                 encode = "form", handle = hndl)
	p1 <- .follow_refresh(p1, hndl)
	.lsms_dump(p1, debug_path, "lsms_login_post")
	if (p1$status != 200) {
		stop("LSMS login (email step): HTTP status ", p1$status, ".")
	}
	pwpage <- httr::content(p1, as = "text", encoding = "UTF-8")
	if (is.null(pwpage)) pwpage <- ""

	# Was the email accepted? On success NADA renders the password form (which
	# has a `name="password"` input). On rejection it re-renders the email form.
	if (!grepl('name="password"', pwpage, fixed = TRUE)) {
		stop("LSMS login: email '", username, "' was rejected.")
	}

	csrf2 <- .lsms_extract_csrf(pwpage)
	if (is.null(csrf2)) {
		stop("LSMS login: could not extract CSRF token from the password form.")
	}

	# --- Step 2: POST password ---------------------------------------------
	p2 <- httr::POST(url_password,
	                 body = list(password = password, submit = "Login",
	                             csrf_name = csrf2[1], csrf_value = csrf2[2]),
	                 encode = "form", handle = hndl)
	p2 <- .follow_refresh(p2, hndl)
	.lsms_dump(p2, debug_path, "lsms_login_post2")
	if (p2$status != 200) {
		stop("LSMS login (password step): HTTP status ", p2$status, ".")
	}

	# --- Verify -------------------------------------------------------------
	if (!.lsms_is_logged_in(hndl, debug_path = debug_path)) {
		stop("LSMS login: password rejected. Verify the credentials in passwords.R ",
		     "by logging in manually at https://microdata.worldbank.org/auth/login")
	}
	invisible(TRUE)
}


## Find the best microdata download URL on a NADA /get-microdata page.
## Returns a single absolute URL or NULL if none could be located.
##
## Tries the modern (NADA 5.4+) markup first, where each microdata file is
## rendered as
##   <div ... data-file-type="microdata">
##       <span class="resource-info" ...> ... <title text> ...</span>
##       ...
##       <a class="download ..." href="<base>/catalog/<sid>/download/<rid>"
##          data-filename="<filename>" data-extension="<ext>" ...>
## Falls back to the old layout (a "Data in CSV" / "Data in ASCII" label
## followed within ~10 lines by an `href="..."` and `title="..."`).
.lsms_find_data_url <- function(html) {
	## --- Modern markup -------------------------------------------------
	## Pull out every <a class="download" ...> whose href points to a
	## /catalog/<sid>/download/<rid> URL on this catalog.
	a_pat <- '<a[^>]*\\bclass="[^"]*\\bdownload\\b[^"]*"[^>]*href="(https?://[^"]+/catalog/\\d+/download/\\d+[^"]*)"[^>]*>'
	hits  <- regmatches(html, gregexpr(a_pat, html, perl = TRUE))[[1]]
	if (length(hits) > 0) {
		hrefs <- sub('.*href="([^"]+)".*', "\\1", hits)
		fnames <- ifelse(grepl('data-filename="', hits),
		                 sub('.*data-filename="([^"]*)".*', "\\1", hits), "")
		exts <- ifelse(grepl('data-extension="', hits),
		               sub('.*data-extension="([^"]*)".*', "\\1", hits), "")
		## Prefer CSV, then ASCII, then anything else.
		score <- ifelse(grepl("csv",   fnames, ignore.case = TRUE) | tolower(exts) == "csv",   3L,
		         ifelse(grepl("ascii", fnames, ignore.case = TRUE) | tolower(exts) == "asc",   2L, 1L))
		i <- which.max(score)
		return(hrefs[i])
	}
	## --- Legacy markup -------------------------------------------------
	x <- unlist(strsplit(gsub("\t|\r", "", html), "\n"))
	i <- grep("Data in CSV", x)
	if (length(i) == 0) i <- grep("Data in ASCII", x)
	if (length(i) == 0) return(NULL)
	x <- x[i[1]:min(length(x), i[1] + 10)]
	j <- grep("href=", x)
	if (length(j) == 0) return(NULL)
	x <- x[j[1]:min(length(x), j[1] + 1)]
	x <- gsub("href=|title=", "", x)
	paste0(trimws(gsub('\"', "", x)), collapse = "/")
}

## Detect whether a NADA tab page is showing the "Login to access data" wall,
## i.e. the user is not authenticated. Returns TRUE if so.
.lsms_login_wall <- function(html) {
	grepl("Login to access data", html, fixed = TRUE) ||
	grepl('href="[^"]*/auth/login[^"]*">\\s*Login\\s*</a>', html, perl = TRUE)
}


## End-to-end probe to diagnose authentication problems against the WB
## microdata catalog. Saves a verbose dump of every request (login GET,
## login POST and the post-login probe) into `path` and returns invisibly
## a TRUE/FALSE indicating whether the handle ended up authenticated.
##
## Usage from an interactive R session, e.g.:
##   source("passwords.R")
##   yuri:::debug_lsms(p$username, p$password, "C:/temp/lsms-debug")
debug_lsms <- function(username, password, path = tempdir()) {
	dir.create(path, FALSE, TRUE)
	cat("Writing debug dumps to: ", normalizePath(path, mustWork = FALSE), "\n")
	hndl <- httr::handle("https://microdata.worldbank.org")
	ok <- tryCatch({
		.lsms_login(username, password, hndl, debug_path = path)
		TRUE
	}, error = function(e) {
		cat("login error: ", conditionMessage(e), "\n", sep = "")
		FALSE
	})
	cj <- tryCatch(curl::handle_cookies(hndl$handle), error = function(e) NULL)
	if (!is.null(cj)) {
		cat("--- cookies on handle after login ---\n")
		print(cj[, c("domain", "name", "value")])
	}
	cat("--- authenticated according to probe? ", .lsms_is_logged_in(hndl, debug_path = path), "\n", sep = "")
	cat("Files written:\n")
	print(list.files(path, full.names = FALSE))
	invisible(ok)
}


get_LSMS <- function(uri, path, username, password, cache=TRUE) {

	fok <- file.path(path, "ok.txt")
	if (cache && file.exists(fok)) {
		return(list.files(path, recursive=TRUE, full.names=TRUE))
	}
	if (is.null(username) || is.null(password)) {
		stop("LSMS credentials not found. Add them to passwords.R, e.g.:\n",
		     "  pwds <- function() list(LSMS = c(username=\"you@example.com\", password=\"...\"))\n",
		     "Register for an account at https://microdata.worldbank.org/auth/register",
		     call. = FALSE)
	}


	dir.create(path, FALSE, FALSE)
	# Share cookies across login + all subsequent GETs.
	hndl <- httr::handle("https://microdata.worldbank.org")
	# Save HTTP dumps to a temp dir; if login fails, copy them next to the
	# dataset so the user can share them. On success, they're discarded.
	dbg <- tempfile("lsms-dbg-"); dir.create(dbg)
	on.exit(unlink(dbg, recursive = TRUE), add = TRUE)
	tryCatch(.lsms_login(username, password, hndl, debug_path = dbg),
	         error = function(e) {
	             try(file.copy(list.files(dbg, full.names = TRUE), path,
	                           overwrite = TRUE), silent = TRUE)
	             stop(e)
	         })

	uhtml <- paste0("https://doi.org/", gsub("doi:", "", uri))
	z <- httr::GET(uhtml, handle = hndl)

	zurl <- gsub("0;url=", "", z$headers$refresh)
	url <- paste0(zurl, "/get-microdata")
	gm <- .follow_refresh(httr::GET(url, handle = hndl), hndl)
	r <- httr::content(gm, as="text", encoding = "UTF-8")
	if (is.null(r) || !nzchar(r)) {
		.lsms_dump(gm, path, "get-microdata-empty")
		stop("LSMS: empty response from ", url, " (status ", gm$status_code, ").")
	}
	if (.lsms_login_wall(r)) {
		stop("LSMS login: not authenticated when fetching ", url,
		     " (the page shows 'Login to access data').")
	}

	durl <- .lsms_find_data_url(r)
	if (is.null(durl)) {
		haveData <- FALSE
		# Save the page so we can adapt the parser if the markup changes again.
		try(writeLines(r, file.path(path, "get-microdata.html")), silent = TRUE)
		warning("these files need to be downloaded manually")
	} else {
		haveData <- TRUE
		g <- httr::GET(durl, handle = hndl)
		# Prefer the server-supplied filename if available.
		cd <- g$headers[["content-disposition"]]
		fname <- if (!is.null(cd) && grepl('filename=', cd)) {
			gsub('.*filename="?([^";]+)"?.*', "\\1", cd)
		} else {
			basename(sub("\\?.*$", "", durl))
		}
		fout <- file.path(path, fname)
		writeBin(httr::content(g, "raw"), fout)
		if (grepl("\\.zip$", fout, ignore.case = TRUE)) {
			try(utils::unzip(fout, exdir = path), silent = TRUE)
		}
	}

	url <- paste0(zurl, "/study-description")
	r <- httr::content(.follow_refresh(httr::GET(url, handle = hndl), hndl),
	                   as="text", encoding = "UTF-8")
	if (is.null(r)) r <- ""
	writeLines(r, file.path(path, "study-description.html"))

	url <- paste0(zurl, "/related-materials")
	r <- httr::content(.follow_refresh(httr::GET(url, handle = hndl), hndl),
	                   as="text", encoding = "UTF-8")
	if (is.null(r)) r <- ""
	x <- trimws(unlist(strsplit(gsub("\t|\r", "", r), "\n")))
	x <- x[grep(paste0('href=\"', zurl, "/download"), x)]
	p <- grep("pdf", x, value=TRUE)
	# only attempt PDF parsing/download if entries match the expected
	# `title="..." href="..."` pattern; some studies expose related materials
	# in a different layout that would otherwise crash strsplit.
	p <- p[grepl('title=\"', p) & grepl('\" href=\"', p)]
	if (length(p) > 0) {
		s <- sapply(strsplit(p, 'title=\"'), function(i) i[[2]])
		z <- strsplit(s, '\" href=\"')
		pdf <- sapply(z, function(i) i[[1]])
		updf <- sapply(z, function(i) strsplit(i[[2]], '\">')[[1]][1])
		fpdf <- gsub(" ", "%20", paste0(updf, "/", pdf))
		dp <- file.path(path, "docs")
		dir.create(dp, FALSE, FALSE)
		for (f in fpdf) {
			try(utils::download.file(f, file.path(dp, basename(f)), mode="wb", quiet=TRUE), silent=TRUE)
		}
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

