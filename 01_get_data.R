
# Get WDI data ------------------------------------------------------------

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




