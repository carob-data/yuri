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
credentials differently can replace that advice with a single string via
\code{set_auth_advice}, so users see directions appropriate for their
workflow instead.

Pass \code{NULL} to clear the override and restore the default messages.
}

\usage{
set_auth_advice(advice = NULL)
}

\arguments{
  \item{advice}{\code{NULL} or a character string used in place of yuri's
    default authentication advice.}
}

\value{
  Invisibly \code{NULL}. Called for its side effect.
}

\examples{
\dontrun{
set_auth_advice("Set credentials in 'passwords.R' in the project root.")

# Restore the default messages.
set_auth_advice(NULL)
}
}

\seealso{\code{\link{authenticate}}, \code{\link{dataURI}}}
