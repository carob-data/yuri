\name{simpleURI}


\alias{simpleURI}

\title{
Clean an URI so that it can be used as a folder name
}

\description{
Replace "/" and ":" in an URI with "_" so that they can be used as filenames
}


\usage{
simpleURI(uri, reverse=FALSE)
}
 
\arguments{
  \item{uri}{character, a URI}
  \item{reverse}{logical. If \code{TRUE}, the reverse operation is done}
}

\value{
character
}


\examples{
uri <- "doi:10.7910/DVN/UNLRGC"
u <- simpleURI(uri)
u
simpleURI(u, reverse=TRUE)
}


