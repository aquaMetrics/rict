% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare-test.R
\name{compare_test}
\alias{compare_test}
\title{A Statistical Test to Compare Two Results}
\usage{
compare_test(a, b)
}
\arguments{
\item{a}{numeric vector containing results 'a'}

\item{b}{numeric vector constainng results 'b'}
}
\value{
dataframe containing stats
}
\description{
Statistics to compare whether there is a real difference in EQR values of
paired results. It is a low-level function used by the higher-level
\code{\link{rict_compare}} function to compare the EQRs of two paired
results.
}
\examples{
\dontrun{
test <- compare_test(
  a = c(0.3, 0.4, 0.6),
  b = c(1, 0.9, 0.6)
)
}
}
