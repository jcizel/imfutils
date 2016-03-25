##' @export
loadFSI2016 <- function(){
    FILE <- system.file('./imffsi2016/imffsi2016_clean.RData',
                        package = 'imfutils')
    load(FILE)

    return(fsi_final)
    
}

prepeareFSI <- function(){
    FILE.TS <- system.file('./imffsi2016/FSI_03-25-2016 15-10-03-46_timeSeries.csv',
                           package = 'imfutils')


    FILE.TS %>>%
        read.csv(check.names = FALSE) %>>%
        data.table ->
        data

    ## save(data, file = "~/Downloads/imfifs.RData")

    data[, 1:5, with = FALSE] %>>% names ->
        cols.exclude

    ## names(data) %>>% length
    ## names(data) %>>% unique %>>% length
    ## data %>>% (V1165) %>>% table

    data %>>%
        mutate(
            imfctry = `Country Code`,
            concept_id = `Indicator Code` %>>% gsub(pattern = "\\_", replacement = "\\."),
            concept_label = `Indicator Name`,
            attribute = Attribute
        ) ->
        data

    cols.id = c('imfctry','concept_id','concept_label','attribute')
    cols.exclude = c(cols.exclude, 'Base Year', 'V1165')
    cols.data = setdiff(names(data),c(cols.exclude,cols.id))
    data[,.SD, .SDcols = c(cols.id,cols.data)] ->
        data

    data %>>%
        subset(attribute != 'Status') %>>% 
        melt.data.table(
            id.vars = cols.id,
            variable.factor = FALSE
        ) %>>%
        subset(!is.na(value)) ->
        fsi_long

    ## rm(data)

    fsi_long[, date := variable %>>% imfDate2date]
    fsi_long[, value := value %>>% as.numeric]
    fsi_long[, periodicity := variable %>>% imfDate2periodicity]
    ## fsi_long %>>% subset(is.na(value2)) %>>% (value) %>>% table
    ## fsi_long %>>% (attribute) %>>% table


    fsi_long %>>%
        subset(!is.na(value)) %>>%
        arrange(
            imfctry,concept_id,date,periodicity
        ) %>>%
        (df~df[,periods:= {
            periodicity %>>%
                paste(collapse = ';')
        }, by = list(imfctry,concept_id,date)]) ->
        fsi_long


    fsi_long %>>% (periods) %>>% table

    fsi_long[,keep1:=TRUE]
    fsi_long[(periods %in% c('A;A;M;M;Q;Q',
                             'M;M',
                             'M;M;Q;Q') & (value==0)), keep1:=FALSE]

    ## fsi_long %>>% subset(periods == 'M;M')

    fsi_long %>>%
        mutate(
            drop =
                (periods == 'A;M' & periodicity == 'A') |
                (periods == 'A;M;Q' & periodicity == 'A') |
                (periods == 'A;M;Q' & periodicity == 'Q') |                        
                (periods == 'A;Q' & periodicity == 'A') |
                (periods == 'M;Q' & periodicity == 'Q')
        ) ->
        fsi_long

    fsi_long %>>%
        subset(drop == FALSE) %>>%
        select(imfctry,concept_id,date,value)->
        fsi_final

    fsi_long %>>%
        select(concept_id,concept_label) %>>%
        setkey(concept_id) %>>%
        unique ->
        lookup

    attr(fsi_final,'lookup') <- lookup

    save(fsi_long, file = '~/Downloads/imffsi2016_full.RData')
    save(fsi_final, file = '~/Downloads/imffsi2016_clean.RData')

    return(fsi_final)    
}
