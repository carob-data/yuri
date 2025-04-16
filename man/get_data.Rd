\name{get_data}

\alias{get_data}

\title{
Download a data set
}

\description{
Download the files associated with a URI. 
}

\usage{
get_data(uri, path, cache=TRUE, unzip=TRUE)
}
 
\arguments{
  \item{uri}{character. The URI of a dataset}
  \item{path}{character. The path to where the data should be saved. The simplified uri is always appended to this path}
  \item{cache}{logical. if \code{TRUE} the data are only downloaded again if they are not locally available}
  \item{unzip}{logical. if \code{TRUE} donwloaded zip files are unzipped}
}

\value{
character, the downloaded file names
}
