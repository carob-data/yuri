
clone_github <- function(name, path) {
	fgz <- tempfile()
	url <- paste0("https://api.github.com/repos/", name, "/tarball/HEAD")
	utils::download.file(url, fgz, mode="wb", quiet = TRUE)

	dzip <- tempfile()
	untar(fgz, exdir=dzip)
	relff <- list.files(dzip, recursive=TRUE)
	rem <- strsplit(relff[1], "/")[[1]][1]

	outf <- file.path(path, repo, gsub(remove, "", relff))
	outd <- unique(dirname(outf))
	for (d in outd) dir.create(d, FALSE, TRUE)
	
	oldff <- list.files(path, recursive=TRUE, full.names=TRUE)
	file.remove(oldff)	
	
	ff <- list.files(dzip, recursive=TRUE, full.names=TRUE)
	all(file.rename(ff, outf))
}
