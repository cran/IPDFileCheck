% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IPDFilecheck.R
\name{convert_date_string_stdform}
\alias{convert_date_string_stdform}
\title{Helper function to keep date formats in year-month-date}
\usage{
convert_date_string_stdform(entry, orderby)
}
\arguments{
\item{entry}{a date e.g 1 Jan 2020 with no commas}

\item{orderby}{give the order such as mdy, dmy etc where d refers to day,
m to month and y to year}
}
\value{
entry corrected entries as in standard date format
}
\description{
Helper function to keep date formats in year-month-date
}
\examples{
convert_date_string_stdform("Jan-1-2020", "mdy")
}
