
# Get WDI data ------------------------------------------------------------

indicators <-
  c(
    "SH.MED.BEDS.ZS",
    "SH.MED.NUMW.P3",
    "SH.MED.PHYS.ZS",
    "SH.UHC.SRVS.CV.XD",
    "SH.DYN.NCOM.ZS",
    "SH.DYN.NCOM.FE.ZS",
    "SH.DYN.NCOM.MA.ZS",
    "SH.STA.DIAB.ZS",
    "SH.PRV.SMOK",
    "SH.PRV.SMOK.FE",
    "SH.PRV.SMOK.MA",
    "SH.DTH.COMM.ZS",
    "SH.DTH.INJR.ZS",
    "SH.DTH.NCOM.ZS",
    "SH.XPD.OOPC.CH.ZS",
    "SH.XPD.OOPC.PC.CD",
    "SH.XPD.OOPC.PP.CD",
    "SH.UHC.OOPC.10.ZS",
    "SH.UHC.OOPC.25.ZS",
    "SH.STA.HYGN.ZS",
    "SH.STA.HYGN.UR.ZS",
    "SH.STA.HYGN.RU.ZS",
    "SP.POP.80UP.FE.5Y",
    "SP.POP.80UP.MA.5Y",
    "SP.POP.65UP.FE.ZS",
    "SP.POP.65UP.MA.ZS",
    "SP.POP.65UP.TO.ZS",
    "SP.POP.0014.FE.ZS",
    "SP.POP.0014.MA.ZS",
    "SP.POP.0014.TO.ZS",
    "SP.POP.1564.FE.ZS",
    "SP.POP.1564.MA.ZS",
    "SP.POP.1564.TO.ZS"
  )

refresh_wbcache()
df <- wbgdata(
  "all",
  indicators,
  year=c(1990:2019)
)
readr::write_rds(df, "input/wbgdata.rds")


# Get COVID data ----------------------------------------------------------

daturl = paste("http://cvapi.zognet.net/all.json")

# get world covid-19 data
dat_raw_1 = RJSONIO::fromJSON(daturl, nullValue=NA)[[1]]
dat_raw_2 = RJSONIO::fromJSON(daturl, nullValue=NA)[[2]]

readr::write_rds(dat_raw_1, "input/dat_raw_1.rds")
readr::write_rds(dat_raw_2, "input/dat_raw_2.rds")

# get country list
country_list = dat_raw_1$countries

# country level covid-19 data
full_country_data <- data.frame()
for (country in country_list){ # loop through countries
  daturl = paste("http://cvapi.zognet.net/", country, ".json", sep = "")
  country_info = RJSONIO::fromJSON(daturl, nullValue=NA)[[1]]
  iso = country_info[["iso"]]
  name = country_info[["name"]]
  country_raw = RJSONIO::fromJSON(daturl, nullValue=NA)[[2]]
  country_data = lapply(country_raw, function(j) cbind(j$date, j$confirmed, j$deaths, j$recovered))
  country_data = data.frame(do.call('rbind', country_data), stringsAsFactors = FALSE)
  colnames(country_data) = c( "date", "confirmed", "deaths", "recovered")
  country_data <- cbind(iso, name, country_data) # add country info to the data set
  full_country_data<-rbind(full_country_data, country_data) # attach the country info to the full list
}

readr::write_rds(full_country_data, "input/full_country_data.rds")

# indicator list metadata
json_file <- "https://raw.githubusercontent.com/worldbank/decdg-covid19/master/covid-indicators.json"
dat = fromJSON(paste(readLines(json_file), collapse=""))

indicator_list <- data.frame()
for (subdat in dat) {
  ind = data.frame(do.call('rbind', subdat$indicators), stringsAsFactors = FALSE)
  topic = subdat$topic
  ind <- cbind(code = rownames(ind), ind)
  #rownames(ind) <- 1:nrow(ind)
  rownames(ind) <- NULL
  ind <- cbind(topic, ind)
  colnames(ind) = c( "topic", "code", "name")
  indicator_list<-rbind(indicator_list, ind)
}


