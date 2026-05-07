\name{set_auth_advice}

\alias{set_auth_advice}

\title{
Customize the authentication advice in yuri error messages
}

\description{
Authentication-related errors raised by \code{\link{dataURI}} (and its
back-end download helpers) include actionable advice that, by default,
points stand-alone callers to \code{\link{authenticate}} or to the
\code{password} argument of \code{dataURI}. Wrapper packages that manage
credentials differently can register an alternative advice string with
\code{set_auth_advice} so users see directions appropriate for their
workflow instead.

Pass \code{NULL} to clear the override and restore the default messages.

\code{advice} can be either a character string (used verbatim in place of
the default advice everywhere) or a function. When a function is supplied
it is called as \code{advice(default, ...)} where \code{default} is
yuri's default advice text and additional named arguments describe the
context (e.g. \code{service = "DRYAD"}, \code{kind = "credentials"}); it
should return a character string. If the function errors or returns a
non-character value the default advice is used.
}

\usage{
set_auth_advice(advice = NULL)
}

\arguments{
  \item{advice}{\code{NULL}, a character string, or a function
    \code{function(default, ...)} returning a character string.}
}

\value{
  Invisibly \code{NULL}. Called for its side effect.
}

\examples{
\dontrun{
# Replace yuri's default authentication advice with a single fixed string.
set_auth_advice("Set credentials in 'passwords.R' in the project root.")

# Provide context-aware advice via a callback.
set_auth_advice(function(default, service = NA, kind = NA, ...) {
	if (identical(service, "DRYAD")) {
		"Add DRYAD = list(username = ..., password = ...) to passwords.R"
	} else {
		default
	}
})

# Restore the default messages.
set_auth_advice(NULL)
}
}

\seealso{\code{\link{authenticate}}, \code{\link{dataURI}}}
