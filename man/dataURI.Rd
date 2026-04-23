\name{dataURI}

\alias{dataURI}

\title{
Get a data set
}

\description{
Download the files associated with a URI and return the filenames. 
}

\usage{
dataURI(uri, path, cache=TRUE, unzip=TRUE, recursive=FALSE, filter=TRUE, username=NULL, password=NULL, institute=NULL, email=NULL)
}
 
\arguments{
  \item{uri}{character. The URI of a dataset}
  \item{path}{character. The path to where the data should be saved. The simplified uri is always appended to this path}
  \item{cache}{logical. if \code{TRUE} the data are only downloaded again if they are not locally available}
  \item{unzip}{logical. if \code{TRUE} downloaded zip files are unzipped}
  \item{recursive}{logical. if \code{TRUE} also return filenames in sub-directories}
  \item{filter}{logical. if \code{TRUE} files that probably do not have the primary data are filtered out (files ending on .pdf, .doc(x), .json)}
  \item{username}{optional. For Dataverse guestbooks, used as the \code{name} field when given; for Dryad, the OAuth client id. If \code{NULL} or empty, the value is taken from credentials registered via \code{\link{authenticate}} (\code{DATAVERSE$name}), then environment variables \code{YURI_DATAVERSE_USERNAME} / \code{DATAVERSE_GUESTBOOK_NAME}, then a built-in default.}
  \item{password}{optional. For Dataverse, treated as an API token if set (\code{X-Dataverse-key}); if not given, the token registered via \code{\link{authenticate}} (\code{DATAVERSE$token}) or the environment variables \code{YURI_DATAVERSE_PASSWORD}, \code{DATAVERSE_API_TOKEN}, or \code{DATAVERSE_KEY} are used. For Dryad, the OAuth client secret.}
  \item{institute}{optional. For Dataverse guestbooks, the \code{institution} field. If \code{NULL} or empty, the value is taken from credentials registered via \code{\link{authenticate}} (\code{DATAVERSE$institution}), then \code{YURI_DATAVERSE_INSTITUTE} / \code{DATAVERSE_GUESTBOOK_INSTITUTION}, then a built-in default.}
  \item{email}{optional. For Dataverse guestbooks, the \code{email} field. If \code{NULL} or empty, the value is taken from credentials registered via \code{\link{authenticate}} (\code{DATAVERSE$email}), then \code{YURI_DATAVERSE_EMAIL} / \code{DATAVERSE_GUESTBOOK_EMAIL}, then a built-in default.}
}

\details{
Some Harvard Dataverse (and other Dataverse) datasets require a guestbook before bulk file download. In that case, \code{dataURI} POSTs to the Dataverse Access API with \code{signed=true} and a guestbook payload, then downloads via the returned signed URL (see the Data Access API guide).

Guestbook field resolution order is: function arguments (\code{username} for name, \code{email}, \code{institute}, \code{password} for API token), then credentials registered via \code{\link{authenticate}} (\code{DATAVERSE$<field>}), then environment variables (\code{YURI_DATAVERSE_*} and the historical \code{DATAVERSE_GUESTBOOK_*}), then built-in defaults.

Register the fields the server requires with \code{authenticate(list(DATAVERSE = list(name, email, institution, position, token, answers)))} (use \verb{GET /api/guestbooks/{id}} to inspect what the server asks for).

Custom guestbook questions must be supplied in \code{answers} as a list of lists with \code{id} and \code{value} elements (see Dataverse API examples).
}

\value{
character, the downloaded file names
}
