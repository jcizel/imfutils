require(dplyr)

ifssource2final <- function(){
  ## Extract IMFIFS.zip for this to work
  FILE.TS <- system.file('./imfifs2016march/IFS_03-05-2016 06-53-39-38_timeSeries.csv.zip',
                         package = 'imfutils')

  unzip(FILE.TS, list = TRUE)

  FILE.TS %>>%
    unzip(files = 'IFS_03-05-2016 06-53-39-38_timeSeries.csv')
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
    ifs_long

  rm(data)

  ifs_long[, date := variable %>>% imfDate2date]
  ifs_long[, value := value %>>% as.numeric]
  ifs_long[, periodicity := variable %>>% imfDate2periodicity]
  ## ifs_long %>>% subset(is.na(value2)) %>>% (value) %>>% table
  ## ifs_long %>>% (attribute) %>>% table


  ifs_long %>>%
    subset(!is.na(value)) %>>%
    arrange(
      imfctry,concept_id,date,periodicity
    ) %>>%
    (df~df[,periods:= {
      periodicity %>>%
        paste(collapse = ';')
    }, by = list(imfctry,concept_id,date)]) ->
    ifs_long


  ifs_long %>>% (periods) %>>% table

  ifs_long %>>%
    mutate(
      drop =
        (periods == 'A;M' & periodicity == 'A') |
        (periods == 'A;M;Q' & (periodicity == 'A' | periodicity == 'Q')) |
        (periods == 'A;Q' & periodicity == 'A') |
        (periods == 'M;Q' & periodicity == 'Q')
    ) ->
    ifs_long

  ifs_long %>>%
    subset(drop == FALSE) ->
    ## select(imfctry,concept_id,date,value)->
    ifs_final

  ifs_final %>>%
    select(concept_id,concept_label) %>>%
    setkey(concept_id) %>>%
    unique ->
    lookup

  attr(ifs_final,'lookup') <- lookup

  save(ifs_long, file = '~/Downloads/imfifs2016march_full.RData')
  save(ifs_final, file = '~/Downloads/imfifs2016march_clean.RData')

  createWorkbook() ->
    wb

  return(ifs_final)
}


## save(ifs_final,file = '~/Downloads/ifs_final.RData')

## ifs_long %>>%
##     subset(concept_id %like% "^NGDP") %>>%
##     subset(imfctry == '111') %>>% (concept_id) %>>% table

## ifs_final %>>%
##     subset(grepl(pattern = "^NGDP\\..*\\.?XDC",concept_id)) %>>%
##     subset(imfctry == '122') %>>%
##     data.frame
