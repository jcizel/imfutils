##' @export 
loadWEO2015oct <- function(){
    FILE <- system.file('./imfweo2015oct/WEOOct2015all.xls',
                        package = 'imfutils')

    FILE %>>%
        fread(
            header = TRUE,
            na.strings = 'n/a'
        ) %>>%
        select(
            - `Estimates Start After`,
            - `ISO`,
            - `Country`,
            - `Country/Series-specific Notes`
        ) %>>%
        rename(
            imfctry = `WEO Country Code`,
            concept_id = `WEO Subject Code`,
            concept_label = `Subject Descriptor`,
            concept_descr = `Subject Notes`,
            units = `Units`,
            scale = `Scale`
        ) %>>%
        (~ . %>>%
         select(concept_id,concept_label) %>>%
         setkey(concept_id) %>>%
         unique -> lookup) %>>%
        select(
            - concept_descr
        ) %>>%
        melt.data.table(
            id.vars = c('imfctry','concept_id','concept_label','units','scale')
        ) ->
        data

    data %>>%
        mutate(
            value = value %>>% as.numeric,
            date = sprintf('%s-%s-%s',
                           variable,
                           12,
                           31) %>>% as.Date
        ) ->
        data2

    attr(data2,'lookup') <- lookup
    
    return(data2)
}

## loadWEO2015oct() ->
##     weo

