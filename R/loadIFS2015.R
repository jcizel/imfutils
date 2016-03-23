##' @export 
loadIFS2015 <- function(){
    FILE <- system.file('./imfifs2015/ifs_final.RData',
                        package = 'imfutils')
    load(FILE)

    return(ifs_final)
}

##' @export 
loadIFS2016 <- function(){
    FILE <- system.file('./imfifs2016march/imfifs2016march_clean.RData',
                        package = 'imfutils')
    load(FILE)

    return(ifs_final)
}
