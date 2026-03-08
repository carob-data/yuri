\name{authenticate}

\alias{authenticate}

\title{
Provide username and password to authenticate
}

\description{
Authenthication is required for DRYAD and LSMS.

To do so, you can provide a three-column data.frame with these variables: service, username, password
}

\usage{
authenticate(x)
}
 
\arguments{
  \item{x}{data.frame}
}

\examples{

d <- do.call(rbind, list(
		c(service="LSMS", username="name@email.com", password="password"),
		c(service="DRYAD", username="Account ID", password="Secret")
	))

authenticate(d)
}
