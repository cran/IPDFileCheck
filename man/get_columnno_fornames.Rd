% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IPDFilecheck.R
\name{get_columnno_fornames}
\alias{get_columnno_fornames}
\title{Function to return the column number for column name}
\usage{
get_columnno_fornames(data, column_name)
}
\arguments{
\item{data}{a data frame}

\item{column_name}{column names of the data frame}
}
\value{
column number, if success error, if failure
}
\description{
Function to return the column number for column name
}
\examples{
get_columnno_fornames(data.frame("Age" = c(21, 15),
"Name" = c("John", "Dora")), "Name")
}
