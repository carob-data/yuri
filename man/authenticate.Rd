\name{authenticate}

\alias{authenticate}

\title{
Provide credentials for data services
}

\description{
Register credentials for data services that \code{yuri::dataURI} can use. Supported services are \code{"DRYAD"}, \code{"LSMS"}, and \code{"DATAVERSE"}.

\code{x} is a named \code{list} keyed by service name. For \code{DRYAD} and \code{LSMS}, each entry must supply \code{username} and \code{password}. In the case of \code{DRYAD} is the username is "Client ID" and the password is "Secret" as provided on your  \code{https://datadryad.org/account} page

Most \code{DATAVERSE} do not require a "guestbook" authentication, but some do. For \code{DATAVERSE} the supported fields are: \code{email} (required and must be a valid address), \code{name} and \code{institution} (recommended; both fall back to \code{"unspecified"} which most Dataverse instances accept but some reject), \code{position} (optional; defaults to empty), \code{token} (only required for restricted files or other token-gated Dataverse APIs), and \code{answers} (only required when the dataset's guestbook defines custom questions). \code{answers} accepts several shapes: a single string (used as the answer to every custom question), a character vector or unkeyed list (paired with the questions positionally, in the order returned by the Dataverse \code{customQuestions} API; extra entries beyond the number of questions are ignored, so the same \code{answers} can serve multiple datasets), or the explicit \code{list(list(id = , value = ), ...)} form.

Credentials are kept in an internal package environment and are used by the relevant download functions; they are not written to the global options or to disk.
}

\usage{
authenticate(x)
}

\arguments{
  \item{x}{a named \code{list} of per-service field lists.}
}

\examples{
authenticate(list(
	DRYAD = list(username = "Account ID", password = "Secret"),
	LSMS  = list(username = "name@email.com", password = "password"),
	DATAVERSE = list(
		name        = "Jane Doe",
		email       = "jane@example.org",
		institution = "Example University",
		token       = "<DATAVERSE_API_TOKEN>",
		# Shorthand: one string answers every custom question of any guestbook.
		answers     = "Research / data harmonization"
	)
))
}
