
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
	i <- grepl("7z$", files, ignore.case=TRUE)
	if (any(i)) {
		f7 <- files[i]
		for (f in f7) {
			fext <- archive::archive_extract(f, path)
			allf <- c(allf, file.path(path, fext))
		}
	}
	allf
}

