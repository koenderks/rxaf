% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xaf_income_statement.R
\name{xaf_income_statement}
\alias{xaf_income_statement}
\title{Create an Income Statement from a Cleaned XAF File}
\usage{
xaf_income_statement(x, date = NULL)
}
\arguments{
\item{x}{a data frame resulting from a call to \code{read_xaf()}.}

\item{date}{a character specifying the date up until the income statement
should be made up. The required format is day-month-year (i.e., 02-01-2016
is january second, 2016).}
}
\value{
A data frame containing the income statement.
}
\description{
This function creates an income statement at a specific point in
  time from a cleaned XAF file.
}
\examples{
\dontrun{
df <- read_xaf("path/to/xaf/file.xaf")
xaf_income_statement(df, date = "31-12-2022")
}
}
\author{
Koen Derks, \email{k.derks@nyenrode.nl}
}
\keyword{income}
\keyword{statement}
\keyword{xaf}
