\name{extract_metadata}

\alias{extract_metadata}

\title{
Extract dataset level meta-data
}

\description{
Extract some meta-data associated with a data set
}

\usage{
extract_metadata(uri, path)
}
 
\arguments{
  \item{uri}{character. The URI of the dataset}
  \item{path}{character. The path to the data folder}
} 

\value{
data.frame with dataset-level metadata including \code{authors} and
\code{authors_email}. Emails are semicolon-separated in the same order as
author names; missing emails are recorded as \code{NA} in that slot
(or the whole field is \code{NA} if none are available).
}

