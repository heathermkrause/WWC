% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acstables.R
\docType{data}
\name{acsagetable}
\alias{acsagetable}
\title{Demographic and sex by age data from ACS tables for the states and the U.S.
as a whole}
\format{A data frame with 7280 rows and 8 variables:
\describe{
\item{sex}{Male or female.}
\item{age}{Under 5 years, 5 to 
9 years, 10 to 14 years, 15 to 17 years, 18 and 19 years, 20 to 24 years, 25 
to 29 years, 30 to 34 years, 35 to 44 years, 45 to 54 years, 55 to 64 years, 
65 to 74 years, 75 to 84 years, or 85 years and over.}
\item{raceethnicity}{White alone, Hispanic or Latino, black alone, Asian 
alone, or other.}
\item{population}{The number of people in this demographic bin.}
\item{sextotal}{The number of people in this sex and age bin.}
\item{geototal}{The total number of people tabulated in this geographic 
region.}
\item{prob}{\code{population/geototal}, or the proportion of the total that
is in this demographic bin.}
\item{region}{Geographic region, such as "US" for the entire United States or
a two-letter abbrevation for a state ("TX" for Texas)}
}}
\usage{
acsagetable
}
\description{
Demographic and sex by age data from ACS tables B01001 and B01001B/D/H/I for 
the states and the U.S., fetched and processed by \code{process_acs_age}.
}
\details{
Uses ACS 1-year estimate for 2014; the data from table B01001 is 
rebinned and used to find the "other" population
}
\keyword{datasets}
