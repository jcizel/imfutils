##' @export 
imfDate2freq <- function(x){
    y <- rep(NA,length(x))

    cond <- grepl(x, pattern = "Q")
    y[cond] <- "Q"

    cond <- grepl(x, pattern = "M")
    y[cond] <- "M"

    cond <- !grepl(x, pattern = "M|Q")
    y[cond] <- "A"

    return(y)
}

##' @export 
imfDate2year <- function(x){
    y <- rep(NA,length(x))

    cond <- grepl(x, pattern = "Q")
    y[cond] <- substr(x[cond],1,4) %>>% as.numeric

    cond <- grepl(x, pattern = "M")
    y[cond] <- substr(x[cond],1,4) %>>% as.numeric

    cond <- !grepl(x, pattern = "M|Q")
    y[cond] <- substr(x[cond],1,4) %>>% as.numeric

    return(y)
}

##' @export
imfDate2month <- function(x){
    y <- rep(NA,length(x))

    cond <- grepl(x, pattern = "Q")
    y[cond] <- substr(x[cond],6,7) %>>% as.numeric %>>% (x~ x * 3)

    cond <- grepl(x, pattern = "M")
    y[cond] <- substr(x[cond],6,8) %>>% as.numeric

    cond <- !grepl(x, pattern = "M|Q")
    y[cond] <- 12

    return(y)
}

##' @export
imfDate2date <- function(x){
    require(lubridate)
    t = x %>>% unique
    
    data.table(
        t = t,
        date = sprintf(
            fmt = "%s-%s-1",
            t %>>% imfDate2year,
            t %>>% imfDate2month
        ) %>>%
            as.Date %>>%
            (. + months(1) - 1)
    ) %>>%
        setkey(t) ->
        lookup
    
    lookup[x][[2L]] ->
        out

    return(out)
}

##' @export
period2months <- function(period)
{
    x <- period
    y <- rep(12, times = length(x))

    y[grepl('M',x)] <- substr(x[grepl('M',x)],2,4) %>>% as.numeric
    y[grepl('Q',x)] <- substr(x[grepl('Q',x)],2,4) %>>% as.numeric %>>% (. * 3)

    return(y)
}


.periodicity <- function(x)
{
    y <- rep("A", times = length(x))   
    y[grepl('M',x)] <- 'M'
    y[grepl('Q',x)] <- 'Q'
    return(y)
}

##' @export 
imfDate2periodicity <- function(x){
    t = x %>>% unique
    
    data.table(
        t = t,
        per = t %>>% .periodicity
    ) %>>%
        setkey(t) ->
        lookup
    
    lookup[x][[2L]] ->
        out

    return(out)
}
