% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_acs_blank.R
\name{compare_acs_blank}
\alias{compare_acs_blank}
\title{Compare blank file generated by \code{\link{analyse.ACs.blank}}}
\usage{
compare_acs_blank(NF = 2, output.path, SAVE = TRUE)
}
\arguments{
\item{NF}{is the number of file to compare. The default is 2.}

\item{output.path}{is the path where the PNG plots will be save.}

\item{SAVE}{is a logical paramter indicating whether or not the plot
will save. If TRUE, two PNG file will be saved in output.path. Default is TRUE.}
}
\description{
Allows the user to compare the evolution of the blank values of
the ACs over time.
}
\details{
The user will prompt to select the files manually.
}
\author{
Simon Belanger
}
