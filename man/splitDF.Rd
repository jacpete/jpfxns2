% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitDF.R
\name{splitDF}
\alias{splitDF}
\title{Splits data frame into a list of data frames}
\usage{
splitDF(data, sep = 1000, byCol = NULL)
}
\arguments{
\item{data}{Input data frame you want to split into a list.}

\item{sep}{Integer of length 1 that defines how many rows or how many groups
to keep in each data frame object in the returned list. Default is '1000'.}

\item{byCol}{Optional. Character string defining the column from the input
data that will be used to group the data for splitting. Default is 'NULL'.}
}
\value{
Returns a list of data frames. When 'byCol = NULL', each data frame
  object will have nrow() equal to the 'sep' argument. When
  '!is.null(byCol)', each object in the returned list will likely have
  different lengths, but the length of the unique values in the column
  suggested by the 'byCol' argument will be equal to the 'sep' argument.
  However, in both cases, the last object in the returned list may have a
  different size as it is made up of the leftover rows or groups.
}
\description{
\code{splitDF()} splits data frames into a list of data frames.
  This was created to speed up operations when working with large data frames
  by allowing small sections to be run through base::apply and purr::map
  families of functions.
}
\details{
By default it will split a data.frame up into a list of data frame
  objects, each having \code{nrow()} defined by the \code{sep} argument.
  Optionally, when given a \code{byCol} argument, it will group data by the
  column name given and seperate the data keeping groups together where the
  argument \code{sep} now defines how many unique values from \code{byCol}
  are included in each data frame object within the output list.
}
\section{Creation notes}{
 First created on 2019-Feb-2 in FIA_Test.R for my
  BigDataFNR class project in my purdueResearch repo. It was a way to
  handle a data frame with millions of lines and speed up operations to pivot
  a species column and sum by a plot_year ID using the USFS FIA database.
}

\examples{

}
