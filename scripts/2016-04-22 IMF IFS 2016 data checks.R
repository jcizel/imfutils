require(imfutils)
require(data.table)
require(dplyr)
require(rlist)
require(pipeR)
require(ggplot2)

imfRefCtry() ->
  ref

loadIFS2016() ->
  data

(ref %>>% mutate(imfctry = imfctry %>>% as.numeric) %>>% setkey(imfctry))[
  data %>>% setkey(imfctry)
  ] %>>%
  subset(
    !is.na(iso2)
  ) ->
  data

data[, concept := gsub("(.+)\\.(.+)","\\1",concept_id)]
data[, unit := gsub("(.+)\\.(.+)","\\2",concept_id)]

data[, units := unit %>>% unique %>>% paste(collapse = ";") , by = c('concept','iso2','date')]

## If given concept for a given acountry at a given point in time is available in multiple currencies, only keep one.

data %>>% (unit) %>>% table
data %>>% (units) %>>% table
data %>>% (concept) %>>% unique

data %>>%
  subset(units == 'EUR;USD;XDC')

data %>>%
    mutate(
      drop =
        (units == 'EUR;USD' &  unit == 'EUR') |
        (units == 'EUR;USD;XDC' &  (unit == 'EUR' | unit == 'USD')) |
        (units == 'EUR;XDC' &  unit == 'EUR') |
        (units == 'USD;XDC' &  unit == 'USD') |
        (units == 'USD;XDR' &  unit == 'USD')
    ) ->
  data

data %>>%
  subset(
    drop == FALSE
  ) ->
  data2


data %>>%
  ## subset(concept_id == 'FITB.PA') %>>%
  ## subset(concept_id == 'FIGB.PA') %>>%
  ## subset(concept_id == 'FILIBOR.1M.PA') %>>%
  subset(concept_id == 'ESNA.XDR.XDC.RATE') %>>%
  (? . %>>% (iso2) %>>% unique) %>>%
  subset(iso2 == 'NL') %>>%
  ggplot(
    aes(
      x = date,
      y = value
    )
  ) +
  geom_line() +
  theme_bw()
