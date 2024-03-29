% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IPDFilecheck.R
\name{keep_required_columns}
\alias{keep_required_columns}
\title{############################################################################
 Function to keep only certain variables}
\usage{
keep_required_columns(variables, the_data)
}
\arguments{
\item{variables}{list of variables}

\item{the_data}{data to be sub setting}
}
\value{
subset
}
\description{
############################################################################
 Function to keep only certain variables
}
\examples{
the_data <- data.frame("Age" = c(21, 15), "sex" = c("m", "f"))
variable <- "Age"
keep_required_columns(variable, the_data)
}
