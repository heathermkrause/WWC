# utility function from tidyr::col_name
col_name <- function (x, default = stop("Please supply row/column name", call. = FALSE))
{
        if (is.character(x)) return(x)
        if (identical(x, quote(expr = ))) return(default)
        if (is.name(x)) return(as.character(x))
        if (is.null(x)) return(x)
        stop("Invalid row/column specification", call. = FALSE)
}