% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataParse.R
\name{recursiveElementSearch}
\alias{recursiveElementSearch}
\title{INTERNAL FUNCTION: Recursivly search for element in list}
\usage{
recursiveElementSearch(listIn, name)
}
\arguments{
\item{listIn}{The list for which to search through}

\item{name}{Name condition to which to compare all list names to match}
}
\value{
List of all elements found at first level (not exhaustive)
}
\description{
Will recursivly search through input list, till finds first level that contains
at least one entry that matches name parameter
}
\author{
{George Zorn, \email{george.zorn@mail.utoronto.ca}}
}
