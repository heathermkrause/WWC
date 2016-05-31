#' FIPS codes for all counties in the United States
#' 
#' 2010 FIPS codes for counties and county equivalent entities in the United 
#' States, as a data frame. A combination of state and county FIPS codes is a
#' unique identifier for a U.S. county, while a county name is not necessarily.
#' 
#' @format A data frame with 3235 rows and 5 variables:
#' \describe{
#' \item{state}{Two-letter abbrevation for state, such as "TX" for Texas}
#' \item{stateFIPS}{Integer FIPS code for state, such as 17 for Illinois}
#' \item{countyFIPS}{Integer FIPS code for county, such as 31 for Cook County
#' in Illinois}
#' \item{countyname}{Name of county as characters, such as "Cook County". Not 
#' necessarily unique.}
#' \item{classfp}{FIPS class code; see details.}
#' }
#' 
#' @details The FIPS class code is one of five options:
#' \itemize{
#' \item H1: identifies an active county or statistically equivalent entity 
#' that does not qualify under subclass C7 or H6.
#' \item H4: identifies a legally defined inactive or nonfunctioning county or 
#' statistically equivalent entity that does not qualify under subclass H6.
#' \item H5: identifies census areas in Alaska, a statistical county equivalent 
#' entity.
#' \item H6: identifies a county or statistically equivalent entity that is 
#' areally coextensive or governmentally consolidated with an incorporated 
#' place, part of an incorporated place, or a consolidated city. 
#' \item C7: identifies an incorporated place that is an independent city; that 
#' is, it also serves as a county equivalent because it is not part of any 
#' county, and a minor civil division (MCD) equivalent because it is not part 
#' of any MCD.
#' }
#' 
#' @source \url{https://www.census.gov/geo/reference/codes/cou.html}
"countyFIPS"