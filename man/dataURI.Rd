\name{dataURI}

\alias{dataURI}

\title{
Get a data set
}

\description{
Download the files associated with a URI and return the filenames. 
}

\usage{
dataURI(uri, path, cache=TRUE, unzip=TRUE, filter=TRUE, authentication=NULL, keep_folders=TRUE)
}
 
\arguments{
  \item{uri}{character. The URI of a dataset}
  \item{path}{character. The path to where the data should be saved. The simplified uri is always appended to this path}
  \item{cache}{logical. if \code{TRUE} the data are only downloaded again if they are not locally available}
  \item{unzip}{logical. if \code{TRUE} downloaded zip files are unzipped}
  \item{filter}{logical. if \code{TRUE} files that probably do not have the primary data are filtered out (files ending on .pdf, .doc(x), .json)}
  \item{authentication}{optional named list of credentials for the data provider. May be a flat list of fields for the URI's provider (e.g. \code{list(token = "...", email = "...")}), or a service-keyed list like \code{\link{authenticate}} (e.g. \code{list(DATAVERSE = list(token = "..."), DRYAD = list(username = "...", password = "..."))}). Recognized fields depend on the provider (see Details). If \code{NULL}, credentials registered via \code{\link{authenticate}} and environment variables are used.}
  \item{keep_folders}{logical. if \code{TRUE} (default) and \code{unzip=TRUE}, preserve the directory structure inside zip archives. If \code{FALSE}, files are extracted into a flat folder (equivalent to \code{utils::unzip(..., junkpaths=TRUE)}). The returned file list always includes files in sub-directories.}
}

\details{
Some Harvard Dataverse (and other Dataverse) datasets require a guestbook before bulk file download. In that case, \code{dataURI} POSTs to the Dataverse Access API with \code{signed=true} and a guestbook payload, then downloads via the returned signed URL (see the Data Access API guide).

Credential resolution order is: fields in \code{authentication}, then credentials registered via \code{\link{authenticate}}, then environment variables (\code{YURI_DATAVERSE_*} and the historical \code{DATAVERSE_GUESTBOOK_*}), then built-in defaults.

For Dataverse, recognized authentication fields include \code{name} (or \code{username}), \code{email}, \code{institution} (or \code{institute}), \code{token} (or \code{api_token} / \code{password}), \code{position}, and \code{answers}. Register persistent credentials with \code{authenticate(list(DATAVERSE = list(name, email, institution, position, token, answers)))} (use \verb{GET /api/guestbooks/{id}} to inspect what the server asks for).

For Dryad, recognized fields are \code{username} (or \code{client_id}) and \code{password} (or \code{client_secret} / \code{secret}) — the OAuth client id and secret.

Custom guestbook questions must be supplied in \code{answers} as a list of lists with \code{id} and \code{value} elements (see Dataverse API examples).
}

\value{
character, the downloaded file names
}
