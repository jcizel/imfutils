;require(imfutils)
require(pipeR)
require(data.table)
require(openxlsx)

ls(package:imfutils)

imfRefCtry() ->
    ref

loadFSI2016() ->
    fsi

fsi %>>% attr('lookup')
fsi %>>% attributes %>>% names


FILEXL <- 'h:/Desktop/lookup.xlsx'   

createWorkbook()->
    wb

wb %>>% addWorksheet('Lookup Information')

fsi %>>%
    attr('lookup') %>>%
    writeData(wb = wb,
              sheet = 'Lookup Information')


fsi %>>%
    dcast(
        imfctry + date ~ concept_id,
        value.var = 'value'
    ) ->
    fsi_wide


wb %>>% addWorksheet('Data - Wide')

fsi_wide %>>%
    writeData(wb = wb,
              sheet = 'Data - Wide')


wb %>>% saveWorkbook(file = FILEXL,
                     overwrite = TRUE)

