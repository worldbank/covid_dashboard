
# WDI data ----------------------------------------------------------------
# Replace with local data
df <- readr::read_rds("input/wbgdata.rds")

dfm <- melt(df, id = c("iso3c", "date"))

dfmo <- dfm[!is.na(dfm$value),]

# MRV only 2005 onwards
dfmo <- dfmo[dfmo$date > 2004, ] 

mry <- dfmo %>% 
  group_by(iso3c, variable) %>% # filter to max year, each indicator, each country
  filter(date == max(date))

mry <- mry %>%
  rename("mrv" = "value") %>%
  rename("mry" = "date")

mry$date <- 2019

dfm <- merge(dfm, mry, by = c("date", "iso3c", "variable"), all.x = T)

# merging indicator metadata
indicator_list <- readr::read_rds("input/indicator_list.rds")

indicator_list <- indicator_list %>%
  rename("variable" = "name")
dfm <- dfm %>%
  rename("varcode" = "variable")

dfm <- merge(dfm, indicator_list, by.x = "varcode", by.y = "code", all.x = T)

# COVID data --------------------------------------------------------------

wld_data <- readr::read_rds("input/wld_data.rds")

# get country list
full_country_data <- readr::read_rds("input/full_country_data.rds")

full_country_data$date <- as.Date(full_country_data$date, "%Y/%m/%d")

full_country_data$confirmed <- as.numeric(full_country_data$confirmed)
full_country_data$recovered <- as.numeric(full_country_data$recovered)
full_country_data$deaths <- as.numeric(full_country_data$deaths)

full_country_data$`COVID-19 Cases: Active` <- full_country_data$confirmed - full_country_data$deaths - full_country_data$recovered
full_country_data <- full_country_data %>%
                        rename("COVID-19 Cases: Confirmed" = "confirmed",
                               "COVID-19 Cases: Recovered" = "recovered",
                               "COVID-19 Cases: Deaths" = "deaths")


fullcd <- melt(full_country_data, id = c("iso", "name", "date"))

fullcd$value <- as.numeric(fullcd$value)

cc <- fullcd[fullcd$date == max(fullcd$date),]
cc <- cc %>%
  rename("mrv" = "value")

cc$date2 <- 2019

dfm <- merge(dfm, cc, by.x = c("iso3c", "variable", "date"),  by.y = c("iso", "variable", "date2"), 
             all = T)

dfm$mrv.x <- ifelse(!is.na(dfm$mrv.y), dfm$mrv.y, dfm$mrv.x)
dfm$mrv.y <- name <- NULL
dfm <- dfm %>%
  rename("mrv" = "mrv.x")

# Country metadata --------------------------------------------------------

cm <- read_excel("input/country_metadata.xlsx",
                 sheet = "Country - Metadata")
cm1 <- cm[, c(1, 3, 4, 30)]

dfm <- merge(dfm, cm1, by.x = "iso3c", by.y = "Code", all.x = T)
dfm <- dfm[!is.na(dfm$Region),]

#fullcd <- merge(fullcd, cm1, by.x = "iso", by.y = "Code", all.x = T)
#fullcd <- fullcd[fullcd$iso != "TWN",]

wdi_dat <- WDI(indicator = c("SP.POP.TOTL"), start = 2018, end = 2018, extra = TRUE) 
wdi <- wdi_dat[, c(3, 5, 8, 9)]

#ccc <- fullcd[fullcd$variable == "confirmed",]
ccc <- cc[cc$variable == "COVID-19 Cases: Confirmed",]

ccc <- merge(ccc, wdi, by.x = "iso", by.y = "iso3c", all.x = T)
ccc$longitude <- as.numeric(as.character(ccc$longitude))
ccc$latitude <- as.numeric(as.character(ccc$latitude))
ccc$longlab <- paste0(ccc$name, ": ", ccc$mrv)
ccc <- na.omit(ccc)

## Treemap prep
fullcd2 <- merge(fullcd, unique(dfm[,c('iso3c', 'Region', 'Short Name')]), 
                 by.x = 'iso', by.y = 'iso3c', all.x = T)
fullcd2 <- fullcd2[!is.na(fullcd2$`Short Name`),]

treemap_dat <- function(case_type = "COVID-19 Cases: Confirmed") {
  
  tmp <- fullcd2[fullcd2$variable == case_type,]
  tmp <- tmp[which(tmp$value > 0),]
  tmp$date <- as.Date(tmp$date)
  
  return(tmp)
}

tmp <- treemap_dat()
