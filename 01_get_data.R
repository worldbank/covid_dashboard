# indicator list metadata
json_file <- "https://raw.githubusercontent.com/worldbank/decdg-covid19/master/covid-indicators.json"
dat = RJSONIO::fromJSON(paste(readLines(json_file), collapse=""))

indicator_list <- data.frame()
for (subdat in dat) {
  ind = data.frame(do.call('rbind', as.list(subdat$indicators)), stringsAsFactors = FALSE)
  topic = subdat$topic
  ind <- cbind(code = rownames(ind), ind)
  #rownames(ind) <- 1:nrow(ind)
  rownames(ind) <- NULL
  ind <- cbind(topic, ind)
  colnames(ind) = c( "topic", "code", "name")
  indicator_list<-rbind(indicator_list, ind)
}

readr::write_rds(indicator_list, "input/indicator_list.rds")

# Get WDI data ------------------------------------------------------------

indicators <- as.character(indicator_list$code)

updated_cache <- wbcache()
# refresh_wbcache(force = TRUE)
df <- wb(
  indicator = indicators,
  startdate = 1990,
  enddate = 2019,
  return_wide = TRUE,
  cache = updated_cache
) %>%
  select(-iso2c, -country)

readr::write_rds(df, "input/wbgdata.rds")


# Get COVID data ----------------------------------------------------------
daturl = paste("http://cvapi.zognet.net/all.json")

# get world covid-19 data
country_url = paste("http://cvapi.zognet.net/manifest.json")
world_url = paste("http://cvapi.zognet.net/world.json")


# get world covid-19 data
ctry_data <- jsonlite::fromJSON(country_url)
wld_data <- jsonlite::fromJSON(world_url)$data
wld_data$iso <- "WLD"
wld_data$name <- "World"
wld_data <- wld_data[, c("iso", "name", "date", "confirmed", "deaths", "recovered")]
wld_data$`COVID-19 cases: Active` <- wld_data$confirmed - wld_data$deaths - wld_data$recovered
wld_data <- wld_data %>%
              rename("COVID-19 cases: Confirmed" = "confirmed",
                     "COVID-19 cases: Deaths" = "deaths",
                    "COVID-19 cases: Recovered" = "recovered"
)

readr::write_rds(ctry_data, "input/ctry_data.rds")
readr::write_rds(wld_data, "input/wld_data.rds")

# get country list
country_list = names(ctry_data)

# country level covid-19 data
full_country_data <- vector(mode = "list", length = length(country_list))

for ( i in seq_along(country_list)) { # loop through countries
  daturl = paste("http://cvapi.zognet.net/", country_list[i], ".json", sep = "")
  country_info = RJSONIO::fromJSON(daturl, nullValue=NA)[[1]]
  iso = country_info[["iso"]]
  name = country_info[["name"]]
  country_raw = RJSONIO::fromJSON(daturl, nullValue=NA)[[2]]
  country_data = lapply(country_raw, function(j) cbind(j$date, j$confirmed, j$deaths, j$recovered))
  country_data = data.frame(do.call('rbind', country_data), stringsAsFactors = FALSE)
  colnames(country_data) = c( "date", "confirmed", "deaths", "recovered")
  country_data <- cbind(iso, name, country_data) # add country info to the data set
  country_data$iso <- as.character(country_data$iso)
  country_data$name <- as.character(country_data$name)
  full_country_data[[i]] <- country_data # attach the country info to the full list
}

full_country_data <- dplyr::bind_rows(full_country_data)

readr::write_rds(full_country_data, "input/full_country_data.rds")
