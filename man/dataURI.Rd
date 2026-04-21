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
  \item{username}{optional. For Dataverse guestbooks, used as the \code{name} field when given; for Dryad, the OAuth client id. If \code{NULL} or empty, the value is taken from environment variables \code{YURI_DATAVERSE_USERNAME} then \code{DATAVERSE_GUESTBOOK_NAME}, then \code{options("yuri.dataverse.guestbook")}, then defaults.}
  \item{password}{optional. For Dataverse, treated as an API token if set (\code{X-Dataverse-key}); if not given, \code{YURI_DATAVERSE_PASSWORD}, \code{DATAVERSE_API_TOKEN}, or \code{DATAVERSE_KEY} is read from the environment. For Dryad, the OAuth client secret.}
  \item{institute}{optional. For Dataverse guestbooks, the \code{institution} field. If \code{NULL} or empty, \code{YURI_DATAVERSE_INSTITUTE} then \code{DATAVERSE_GUESTBOOK_INSTITUTION} are used, then \code{options("yuri.dataverse.guestbook")}, then defaults.}
  \item{email}{optional. For Dataverse guestbooks, the \code{email} field. If \code{NULL} or empty, \code{YURI_DATAVERSE_EMAIL} then \code{DATAVERSE_GUESTBOOK_EMAIL} are used, then \code{options("yuri.dataverse.guestbook")}, then defaults.}
}

\details{
Some Harvard Dataverse (and other Dataverse) datasets require a guestbook before bulk file download. In that case, \code{dataURI} POSTs to the Dataverse Access API with \code{signed=true} and a guestbook payload, then downloads via the returned signed URL (see the Data Access API guide).

Guestbook field resolution order is: function arguments (\code{username} for name, \code{email}, \code{institute}, \code{password} for API token), environment variables, \code{options(yuri.dataverse.guestbook)}, then built-in defaults. Environment names include \code{YURI_DATAVERSE_USERNAME}, \code{YURI_DATAVERSE_INSTITUTE}, \code{YURI_DATAVERSE_EMAIL}, and the historical \code{DATAVERSE_GUESTBOOK_*} names.

Set \code{options(yuri.dataverse.guestbook = list(name, email, institution, position, answers))} with the fields required by the dataset (use \verb{GET /api/guestbooks/{id}} to inspect).

Custom guestbook questions must be supplied in \code{answers} as a list of lists with \code{id} and \code{value} elements (see Dataverse API examples).
}

\value{
character, the downloaded file names
}
