ifssource2final <- function(){
    ## Extract IMFIFS.zip for this to work
    FILE.TS <- system.file('./imfifs2015/IFS -- TimeSeries.csv',
                           package = 'imfutils')

    FILE.TS %>>%
        fread %>>%
        mutate(
            imfctry = substr(TimeSeriesKey,1,3),
            concept_3 = substr(TimeSeriesKey,4,6),
            concept_5 = substr(TimeSeriesKey,4,8),
            concept_10 = substr(TimeSeriesKey,4,13),
            subject = substr(TimeSeriesKey,4,4),
            transformation = substr(TimeSeriesKey,8,8),
            version = substr(TimeSeriesKey,9,9),
            publication = substr(TimeSeriesKey,10,10),
            partner = substr(TimeSeriesKey,11,13)
            
        ) ->
        ifs.static

    FILE.OBS <- system.file('./imfifs2015/IFS -- Observations.csv',
                            package = 'imfutils')
    FILE.OBS %>>%
        fread %>>%
        select(
            OID,
            year = OStartYY,
            contains('OValue')
        ) ->
        ifs.obs

    FILE.OBS.LOOKUP <- system.file('./imfifs2015/IFS -- Observations -- Lookup.csv',
                                   package = 'imfutils')

    FILE.OBS.LOOKUP %>>%
        fread  ->
        ifs.obs.lookup

    ifs.obs %>>%
        melt.data.table(
            id.vars = c('OID','year'),
            variable.factor = FALSE
        ) ->
        ifs.obs_long

    ifs.obs_long %>>% setkey(variable)
    ifs.obs.lookup %>>% setkey(concept)
    ifs.obs.lookup[ifs.obs_long] %>>%
        subset(!is.na(value)) %>>%
        mutate(
            month = period %>>% period2months,
            periodicity =  period %>>% substr(1,1)
        )->
        ifs.obs_long_v2

    ifs.obs_long_v2 %>>%
        arrange(
            OID,year,month,periodicity
        ) %>>%
        (df~df[,periods:= {
            periodicity %>>%
                paste(collapse = ';')
        }, by = list(OID, year, month)]) ->
        ifs.obs_long_v3

    ifs.obs_long_v3 %>>%
        mutate(
            keep =
                (periods == 'A') |
                (periods == 'A;M' & periodicity == 'M') |
                (periods == 'A;M;Q' & periodicity == 'M') |
                (periods == 'A;Q' & periodicity == 'Q') |
                (periods == 'M;Q' & periodicity == 'M')
            
        ) %>>%
        subset(
            keep == TRUE
        ) %>>%
        select(
            -keep,
            -concept,
            -period
        ) ->
        ifs.obs_long_v4

    ifs.static %>>%
        select(
            OID = TimeSeriesKey,
            country_label = CountryName,
            concept_label = EnglishDescription,
            unit_label = MagnitudeName,
            subject = SubjectMatter,
            consol = CodeConsolidation,
            imfctry,
            concept_3,
            concept_5,
            concept_10,
            version,
            partner,
            subject_code = subject,
            scale = ScaleFactor,
            type = DataType
        ) ->
        ifs.static_v2

    ifs.static_v2 %>>% setkey(OID)
    ifs.obs_long_v4 %>>% setkey(OID)

    ifs.static_v2[ifs.obs_long_v4] ->
        ifs_production

    ifs_production %>>%
        mutate(
            date = sprintf('%s-%s-%s',year,month,1) %>>% as.Date %>>%
                (x ~ x + months(1) - 1)
        ) %>>%
        select(
            imfctry,
            name = concept_10,
            label = concept_label,
            date,
            value
        ) ->
        ifs_final

    ifs_final %>>%
        select(name,label) %>>%
        setkey(name) %>>%
        unique ->
        lookup

    attr(ifs_final,'lookup') <- lookup
    
    return(ifs_final)
}


## save(ifs_final,file = '~/Downloads/ifs_final.RData')
