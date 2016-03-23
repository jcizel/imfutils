##'@export
imfRefCtry <- function(){
    FILE.EXCEL <- system.file('./refinfo/WEO Countries and Country Groups.xlsx',
                              package = 'imfutils')
    FILE.EXCEL %>>%
        read_excel(
            sheet = 'All Countries and Country Codes'
            
        ) %>>%
        data.table %>>%
        select(
            name = Name,
            imfctry = Code,
            iso2 = `ISO-2 code`,
            iso3 = `ISO-3 code`
            
        ) %>>%
        subset(
            !is.na(iso2)
        ) ->
        ref

    return(ref)
}

##' @export
imfRefGroup <- function(){
    FILE.EXCEL <- system.file('./refinfo/WEO Countries and Country Groups.xlsx',
                              package = 'imfutils')
    FILE.EXCEL %>>%
        read_excel(
            sheet = 'WEO Country Groups',
            skip = 3
        ) %>>%
        data.table %>>%
        select(
            group = `Group Name`,
            name = Country,
            imfctry = `Country Code`,
            iso2 = `Country 2 digit ISO code`,
            iso3 = `Country 3 digit ISO code`
        ) %>>%
        subset(
            !is.na(iso2)
        ) ->
        ref

    return(ref)
}


