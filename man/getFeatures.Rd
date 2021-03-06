% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataDrawing.R
\name{getFeatures}
\alias{getFeatures}
\title{INTERNAL FUNCTION: Helper function for standardizing input for before drawing}
\usage{
getFeatures(data, typeParse, colors, condition, yStart, yStop, indent = TRUE)
}
\arguments{
\item{data}{Dataframe containing proteins' features}

\item{typeParse}{Specific entires that will search through the features
dataframe to find matchs based upon condition}

\item{colors}{Colors matched to typeParse features by interation number}

\item{condition}{Condition function for when to return true for match}

\item{yStart}{Height element should begin drawing from}

\item{yStop}{Height element should stop drawing at}

\item{indent}{If the input should be indented, used to indent features of a protein}
}
\value{
Returns figure will all matching features drawn on
}
\description{
Corrects input varables to proper form and used to interate through entries
that were found based upon input condition.
}
\author{
{George Zorn, \email{george.zorn@mail.utoronto.ca}}
}
