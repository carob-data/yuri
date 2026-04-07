
.dataverse_unzip <- function(files, path, unzip_more=TRUE) {
	allf <- NULL
	files <- files[file.exists(files)]
	i <- grepl("zip$", files, ignore.case=TRUE)
	if (any(i)) {
		zipf <- files[i]
		for (z in zipf) {
			zf <- utils::unzip(z, list=TRUE)
			zf <- zf$Name[zf$Name != "MANIFEST.TXT"]
			zf <- grep("/$", zf, invert=TRUE, value=TRUE)
			allf <- c(allf, zf)
			if (unzip_more) {
				ff <- list.files(path, recursive=TRUE, include.dirs=TRUE)
				there <- (zf %in% ff)
				if (!all(there)) {
					utils::unzip(z, zf[!there], exdir = path)
					## zipfiles in zipfile...
					zipzip <- grep("\\.zip$", zf[!there], ignore.case=TRUE, value=TRUE)
					if (length(zipzip) > 0) {
						zipzip <- file.path(path, zipzip)
						for (zz in zipzip) {
							utils::unzip(zz, exdir = path)
						}
						allf <- c(allf, utils::unzip(zz, list=TRUE))
					}
				}
			}
		}
	}
	i <- grepl("\\.7z$", files, ignore.case=TRUE)
	if (any(i)) {
		f7 <- files[i]
		for (f in f7) {
			fext <- archive::archive_extract(f, path)
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
			fext <- R.utils::gunzip(f, remove=FALSE)
			allf <- c(allf, fext)
		}
	}

	allf
}


## After Dataverse zip download: extract nested .7z, .gz, .tar, .tgz, .tar.gz until stable or max_iter.
.dataverse_extract_archives <- function(path, unzip_more = TRUE, max_iter = 5L) {
	for (iter in seq_len(max_iter)) {
		fz <- list.files(path, pattern = "\\.7z$|\\.gz$|\\.tar$|\\.tgz$|\\.tar\\.gz$", full.names = TRUE, ignore.case = TRUE)
		if (length(fz) == 0) {
			break
		}
		n0 <- length(list.files(path, recursive = TRUE, include.dirs = FALSE))
		.dataverse_unzip(fz, path, unzip_more = unzip_more)
		n1 <- length(list.files(path, recursive = TRUE, include.dirs = FALSE))
		if (n1 <= n0) {
			break
		}
	}
	invisible(TRUE)
}

