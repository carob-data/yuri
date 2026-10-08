
.file_ignored <- function(x, ignore) {
	if (length(x) == 0L) return(logical(0))
	ign <- unique(basename(as.character(ignore)))
	ign <- ign[!is.na(ign) & nzchar(ign)]
	if (length(ign) == 0L) return(rep(FALSE, length(x)))
	out <- tolower(basename(as.character(x))) %in% tolower(ign)
	out[is.na(out)] <- FALSE
	out
}


.safe_unzip <- function(zipfile, ..., ignore = NULL) {
	out <- try(utils::unzip(zipfile, ...), silent = TRUE)
	if (inherits(out, "try-error")) {
		msg <- paste0("could not unzip ", basename(zipfile), ": ", as.character(out))
		if (.file_ignored(zipfile, ignore)) {
			warning(msg, call. = FALSE)
			return(NULL)
		}
		stop(msg, call. = FALSE)
	}
	out
}


.dataverse_unzip <- function(files, path, unzip_more=TRUE, junkpaths=TRUE, ignore=NULL) {
	allf <- NULL
	files <- files[file.exists(files)]
	files <- files[!.file_ignored(files, ignore)]
	i <- grepl("zip$", files, ignore.case=TRUE)
	if (any(i)) {
		zipf <- files[i]
		for (z in zipf) {
			zf <- .safe_unzip(z, list=TRUE, ignore = ignore)
			if (is.null(zf)) next
			zf <- zf$Name[zf$Name != "MANIFEST.TXT"]
			zf <- grep("/$", zf, invert=TRUE, value=TRUE)
			zf <- zf[!.file_ignored(zf, ignore)]
			allf <- c(allf, zf)
			if (unzip_more) {
				ff <- list.files(path, recursive=TRUE, include.dirs=TRUE)
				on_disk <- if (isTRUE(junkpaths)) basename(zf) else zf
				there <- on_disk %in% ff
				if (!all(there)) {
					todo <- zf[!there]
					ok <- .safe_unzip(z, todo, exdir = path, junkpaths = junkpaths, ignore = ignore)
					if (!is.null(ok)) {
						## zipfiles in zipfile...
						zipzip <- grep("\\.zip$", todo, ignore.case=TRUE, value=TRUE)
						zipzip <- zipzip[!.file_ignored(zipzip, ignore)]
						if (length(zipzip) > 0) {
							zipzip <- file.path(path, if (isTRUE(junkpaths)) basename(zipzip) else zipzip)
							for (zz in zipzip) {
								.safe_unzip(zz, exdir = path, junkpaths = junkpaths, ignore = ignore)
							}
							listed <- .safe_unzip(zz, list=TRUE, ignore = ignore)
							if (!is.null(listed)) allf <- c(allf, listed)
						}
					}
				}
			}
		}
	}

	## .7z / .rar via libarchive (R package "archive")
	i <- grepl("\\.7z$|\\.rar$", files, ignore.case=TRUE)
	if (any(i)) {
		f7 <- files[i]
		for (f in f7) {
			fext <- try(archive::archive_extract(f, path), silent = TRUE)
			if (inherits(fext, "try-error")) {
				warning("could not extract ", basename(f), ": ",
					as.character(fext), call. = FALSE)
				next
			}
			allf <- c(allf, file.path(path, fext))
		}
	}

	## tar / compressed tar (before plain .gz — avoids gunzip on .tar.gz)
	i <- grepl("\\.tar$|\\.tgz$|\\.tar\\.gz$", files, ignore.case=TRUE)
	if (any(i)) {
		ft <- files[i]
		for (f in ft) {
			nms <- try(utils::untar(f, list = TRUE, tar = "internal"), silent = TRUE)
			if (inherits(nms, "try-error") || length(nms) < 1) {
				next
			}
			ok <- try(utils::untar(f, exdir = path, tar = "internal"), silent = TRUE)
			if (inherits(ok, "try-error")) {
				warning("could not untar ", basename(f), call. = FALSE)
				next
			}
			allf <- c(allf, nms)
		}
	}

	i <- grepl("\\.gz$", files, ignore.case=TRUE) & !grepl("\\.tar\\.gz$", files, ignore.case=TRUE)
	if (any(i)) {
		fgz <- files[i]
		for (f in fgz) {
			fext <- try(R.utils::gunzip(f, remove = FALSE, skip = TRUE), silent = TRUE)
			if (inherits(fext, "try-error")) {
				warning("could not gunzip ", basename(f), ": ",
					as.character(fext), call. = FALSE)
				next
			}
			allf <- c(allf, fext)
			## gzip of a zip (e.g. Dataverse foo.zip.gz) — unzip the gunzipped file
			if (unzip_more && grepl("\\.(zip|7z|rar|tar|tgz)$", fext, ignore.case = TRUE) &&
				!.file_ignored(fext, ignore)) {
				allf <- c(allf, .dataverse_unzip(fext, path, unzip_more = unzip_more, junkpaths = junkpaths, ignore = ignore))
			}
		}
	}

	allf
}


## After zip download: extract nested .zip, .7z, .rar, .gz, .tar, .tgz, .tar.gz until stable or max_iter.
.dataverse_extract_archives <- function(path, unzip_more = TRUE, max_iter = 5L, junkpaths = TRUE, ignore = NULL) {
	seen <- character(0)
	for (iter in seq_len(max_iter)) {
		fz <- list.files(path, pattern = "\\.zip$|\\.7z$|\\.rar$|\\.gz$|\\.tar$|\\.tgz$|\\.tar\\.gz$", full.names = TRUE, ignore.case = TRUE)
		fz <- fz[!.file_ignored(fz, ignore)]
		fz <- setdiff(fz, seen)
		if (length(fz) == 0) {
			break
		}
		seen <- c(seen, fz)
		n0 <- length(list.files(path, recursive = TRUE, include.dirs = FALSE))
		.dataverse_unzip(fz, path, unzip_more = unzip_more, junkpaths = junkpaths, ignore = ignore)
		n1 <- length(list.files(path, recursive = TRUE, include.dirs = FALSE))
		if (n1 <= n0) {
			break
		}
	}
	invisible(TRUE)
}
