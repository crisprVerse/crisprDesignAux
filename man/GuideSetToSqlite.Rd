% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GuideSetToSqlite.R
\name{GuideSetToSqlite}
\alias{GuideSetToSqlite}
\title{Save GuideSet object as a SQLite database}
\usage{
GuideSetToSqlite(
  guideSet,
  dbfile = NULL,
  useSpacerCoordinates = TRUE,
  primaryOnly = FALSE,
  overwrite = FALSE
)
}
\arguments{
\item{guideSet}{A \code{GuideSet} object from crisprDesign.}

\item{dbfile}{String specifying filename of the SQLite database.}

\item{useSpacerCoordinates}{Should the spacer coordinates be used
as start and end coordinates? TRUE by default. If FALSE,
the PAM site coordinate is used for both start and end.}

\item{primaryOnly}{Should only the primary table (on-targets) be saved?
FALSE by default.}

\item{overwrite}{Should data be overwritten if it exists?
FALSE by default.}
}
\description{
Save GuideSet object as a SQLite database.
}
\examples{
library(crisprDesign)
data(guideSetExampleFullAnnotation, package="crisprDesign")
GuideSetToSqlite(guideSetExampleFullAnnotation,
                 tempfile())

}
\author{
Jean-Philippe Fortin
}
