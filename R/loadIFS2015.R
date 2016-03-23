##' @export 
loadIFS2015 <- function(){
    FILE <- system.file('./imfifs2015/ifs_final.RData',
                        package = 'imfutils')
    load(FILE)

    return(ifs_final)
}
