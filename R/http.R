# HTTP helpers: retry over IPv4 after a connect/timeout failure.
# Some hosts (e.g. dataverse.icrisat.org) are slow to accept connections or
# unreachable over IPv6; libcurl's default connect timeout (10s) is too short.
# If httr/libcurl still cannot connect, fall back to the curl binary with -4.


.url_host <- function(url) {
	h <- try(httr::parse_url(url)$hostname, silent = TRUE)
	if (inherits(h, "try-error") || is.null(h) || !nzchar(h)) {
		return(NA_character_)
	}
	h
}


.host_from_error <- function(x) {
	msg <- if (inherits(x, "try-error")) {
		conditionMessage(attr(x, "condition"))
	} else {
		paste(as.character(x), collapse = " ")
	}
	m <- regmatches(msg, regexpr("\\[([^\\]]+)\\]", msg, perl = TRUE))
	if (length(m) != 1L || !nzchar(m)) return(NA_character_)
	gsub("\\[|\\]", "", m)
}


.use_ipv4 <- function(host) {
	if (is.null(host) || is.na(host) || !nzchar(host)) return(FALSE)
	known <- .yuri_environment$ipv4_hosts
	if (is.null(known)) return(FALSE)
	host %in% known
}


.remember_ipv4 <- function(host) {
	if (is.null(host) || is.na(host) || !nzchar(host)) return(invisible())
	.yuri_environment$ipv4_hosts <- unique(c(.yuri_environment$ipv4_hosts, host))
	invisible()
}


.http_is_connect_fail <- function(x) {
	msg <- if (inherits(x, "try-error")) {
		conditionMessage(attr(x, "condition"))
	} else {
		paste(as.character(x), collapse = " ")
	}
	grepl("Timeout was reached|timed out|Could not connect|Failed to connect|Connection refused|Network is unreachable|Could not resolve host|Resolving timed out|SSL connection timeout|curl IPv4",
		msg, ignore.case = TRUE)
}


.curl_exe_available <- function() {
	nzchar(Sys.which("curl"))
}


.curl_exe_run <- function(url, dest, insecure = FALSE, headers = NULL, timeout = 90, write_info = FALSE) {
	args <- c("-4", "-L", "--silent", "--show-error",
		"--connect-timeout", "60",
		"--max-time", as.character(max(60L, as.integer(timeout))),
		"-o", dest)
	wf <- NULL
	if (isTRUE(write_info)) {
		# @file so Windows cmd.exe does not eat %{http_code} as an env var
		wf <- tempfile()
		writeLines("%{http_code}\n%{url_effective}", wf)
		args <- c(args, "-w", paste0("@", wf))
	}
	if (isTRUE(insecure)) args <- c(args, "-k")
	if (!is.null(headers)) {
		nms <- names(headers)
		for (i in seq_along(headers)) {
			args <- c(args, "-H", paste0(nms[i], ": ", headers[[i]]))
		}
	}
	args <- c(args, url)
	errf <- tempfile()
	on.exit({
		unlink(errf)
		if (!is.null(wf)) unlink(wf)
	}, add = TRUE)
	out <- suppressWarnings(system2(Sys.which("curl"), args,
		stdout = if (isTRUE(write_info)) TRUE else FALSE, stderr = errf))
	status <- attr(out, "status")
	if (is.null(status)) status <- 0L
	if (status != 0L) {
		err <- paste(readLines(errf, warn = FALSE), collapse = "\n")
		stop(if (nzchar(err)) err else paste("curl exited", status), call. = FALSE)
	}
	out
}


.curl_exe_response <- function(url, dest = NULL, insecure = FALSE, headers = NULL, timeout = 90) {
	own_dest <- is.null(dest)
	if (own_dest) {
		dest <- tempfile()
		on.exit(unlink(dest), add = TRUE)
	}
	wr <- .curl_exe_run(url, dest, insecure = insecure, headers = headers,
		timeout = timeout, write_info = TRUE)
	http <- suppressWarnings(as.integer(wr[1]))
	if (is.na(http)) http <- 200L
	final <- if (length(wr) >= 2L && nzchar(wr[2])) wr[2] else url
	if (!own_dest && (!file.exists(dest) || isTRUE(file.info(dest)$size < 1))) {
		stop("curl IPv4 download returned an empty file: ", url, call. = FALSE)
	}
	structure(list(status_code = http, url = final), class = "response")
}


# libcurl CURLOPT_IPRESOLVE: 1 = IPv4 only.
# First attempt uses a short connect timeout; the IPv4 retry uses a long one
# because some Dataverse servers take >20s to accept a TCP connection.
.http_request <- function(verb, url, ..., insecure = FALSE, timeout = NULL) {
	host <- .url_host(url)
	once <- function(ipv4, connecttimeout) {
		opts <- list(connecttimeout = connecttimeout)
		tot <- connecttimeout + 30
		if (!is.null(timeout) && timeout > tot) tot <- timeout
		opts$timeout <- tot
		if (isTRUE(insecure)) opts$ssl_verifypeer <- 0L
		if (isTRUE(ipv4)) opts$ipresolve <- 1L
		args <- c(list(url), list(do.call(httr::config, opts)), list(...))
		do.call(verb, args)
	}
	prefer_v4 <- isTRUE(.use_ipv4(host))
	n <- if (prefer_v4) 1L else 2L
	r <- NULL
	for (i in seq_len(n)) {
		ipv4 <- prefer_v4 || i > 1L
		ct <- if (ipv4) 60 else 15
		if (i > 1L) {
			who <- .host_from_error(r)
			if (is.na(who)) who <- host
			message("connection failed; retrying over IPv4",
				if (!is.na(who)) paste0(" (", who, ")"))
		}
		r <- try(once(ipv4, ct), silent = TRUE)
		if (!inherits(r, "try-error")) {
			if (ipv4) {
				.remember_ipv4(host)
				.remember_ipv4(.url_host(r$url))
			}
			return(r)
		}
		if (!.http_is_connect_fail(r)) {
			stop(conditionMessage(attr(r, "condition")), call. = FALSE)
		}
		.remember_ipv4(host)
		.remember_ipv4(.host_from_error(r))
	}
	r
}


.http_curl_fallback <- function(url, dest = NULL, insecure = FALSE, headers = NULL, timeout = 90, quiet = FALSE) {
	if (!.curl_exe_available()) return(NULL)
	who <- .url_host(url)
	if (!isTRUE(quiet)) {
		message("connection failed; fetching with curl -4",
			if (!is.na(who)) paste0(" (", who, ")"))
	}
	r <- try(.curl_exe_response(url, dest = dest, insecure = insecure,
		headers = headers, timeout = timeout), silent = TRUE)
	if (!inherits(r, "try-error") && !is.null(r)) {
		.remember_ipv4(who)
		.remember_ipv4(.url_host(r$url))
	}
	r
}


.http_GET <- function(url, ..., insecure = FALSE, timeout = NULL) {
	tot <- if (is.null(timeout)) 90 else timeout
	if (.use_ipv4(.url_host(url)) && .curl_exe_available()) {
		fb <- .http_curl_fallback(url, insecure = insecure, timeout = tot, quiet = TRUE)
		if (!inherits(fb, "try-error") && !is.null(fb)) return(fb)
	}
	r <- .http_request(httr::GET, url, ..., insecure = insecure, timeout = timeout)
	if (!inherits(r, "try-error")) return(r)
	fb <- .http_curl_fallback(url, insecure = insecure, timeout = tot)
	if (!inherits(fb, "try-error") && !is.null(fb)) return(fb)
	stop(conditionMessage(attr(r, "condition")), call. = FALSE)
}


.http_POST <- function(url, ..., insecure = FALSE, timeout = NULL) {
	r <- .http_request(httr::POST, url, ..., insecure = insecure, timeout = timeout)
	if (!inherits(r, "try-error")) return(r)
	stop(conditionMessage(attr(r, "condition")), call. = FALSE)
}


.http_download <- function(url, dest, insecure = FALSE, headers = NULL, timeout = 300) {
	if (.use_ipv4(.url_host(url)) && .curl_exe_available()) {
		fb <- .http_curl_fallback(url, dest = dest, insecure = insecure,
			headers = headers, timeout = timeout, quiet = TRUE)
		if (!inherits(fb, "try-error") && !is.null(fb)) return(fb)
	}
	wd <- httr::write_disk(dest, overwrite = TRUE)
	r <- if (is.null(headers)) {
		.http_request(httr::GET, url, wd, insecure = insecure, timeout = timeout)
	} else {
		.http_request(httr::GET, url, wd, do.call(httr::add_headers, as.list(headers)),
			insecure = insecure, timeout = timeout)
	}
	if (!inherits(r, "try-error")) return(r)
	fb <- .http_curl_fallback(url, dest = dest, insecure = insecure,
		headers = headers, timeout = timeout)
	if (!inherits(fb, "try-error") && !is.null(fb)) return(fb)
	stop(conditionMessage(attr(r, "condition")), call. = FALSE)
}
