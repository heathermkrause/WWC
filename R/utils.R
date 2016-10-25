## utility function from tidyr::col_name
col_name <- function (x, default = stop("Please supply row/column name", call. = FALSE))
{
        if (is.character(x)) return(x)
        if (identical(x, quote(expr = ))) return(default)
        if (is.name(x)) return(as.character(x))
        if (is.null(x)) return(x)
        stop("Invalid row/column specification", call. = FALSE)
}

## error handling for for weighting indicators
test_indicators <- function(dots, force_edu) {
        if (any(purrr::map(dots, function(x) 
        {x[[1]] %in% c("sex", "raceethnicity", "age", "education")}) == FALSE)) {
                stop("indicators must be one or more of sex, raceethnicity, age, and education") }
        if (sum(purrr::map_lgl(dots, function(x) {x[[1]] %in% c("age", "education")})) > 1) {
                stop("indicators cannot include both age and education") }
        if ("age" %in% dots & force_edu) {
                stop("force_edu cannot be TRUE if age is one of the indicators") }
        return(TRUE)
}