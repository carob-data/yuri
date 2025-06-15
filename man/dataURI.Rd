\name{dataURI}

\alias{dataURI}

\title{
Get a data set
}

\description{
Download the files associated with a URI and return the filenames. 
}

\usage{
dataURI(uri, path, cache=TRUE, unzip=TRUE, filter=TRUE)
}
 
\arguments{
  \item{uri}{character. The URI of a dataset}
  \item{path}{character. The path to where the data should be saved. The simplified uri is always appended to this path}
  \item{cache}{logical. if \code{TRUE} the data are only downloaded again if they are not locally available}
  \item{unzip}{logical. if \code{TRUE} donwloaded zip files are unzipped}
  \item{filter}{logical. if \code{TRUE} files that probably do not have the primary data are filtered out (files ending on .txt, .pdf, .doc(x), .json)}
}

\value{
character, the downloaded file names
}
