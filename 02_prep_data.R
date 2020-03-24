
# WDI data ----------------------------------------------------------------
# Replace with local data
dfm <-  readr::read_rds("input/wbgdata.rds") %>%
  melt(id = c("iso3c", "date"))

mry <- dfm %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(iso3c, variable) %>% # filter to max year, each indicator, each country
  dplyr::filter(date == max(date)) %>%
  rename(mrv = value,
         mry = date) %>%
  mutate(
    date = 2019
  )

mry <- mry[mry$mry > 2009,] ## drops less than 150 obs

dfm <- merge(dfm, mry, by = c("date", "iso3c", "variable"), all.x = T) %>%
  rename(varcode = variable)

# merging indicator metadata
indicator_list <- readr::read_rds("input/indicator_list.rds") %>%
  rename(variable = name)

dfm <- merge(dfm, indicator_list, by.x = "varcode", by.y = "code", all.x = T)

# COVID data --------------------------------------------------------------

wld_data <- readr::read_rds("input/wld_data.rds")

# get country list
full_country_data <- readr::read_rds("input/full_country_data.rds") %>%
  mutate(
    date                     = as.Date(date, "%Y/%m/%d"),
    confirmed                = as.numeric(confirmed),
#    recovered                = as.numeric(recovered),
    deaths                   = as.numeric(deaths)
#    `COVID-19 Cases: Active` = confirmed - deaths - recovered
  ) %>%
  rename("COVID-19 Cases: Confirmed" = confirmed,
#         "COVID-19 Cases: Recovered" = recovered,
         "COVID-19 Cases: Deaths"    = deaths)

# New indicators including populatio, lat, long merged with corona data
wdi <- WDI(indicator = c("SP.POP.TOTL"), start = 2018, end = 2018, extra = TRUE) %>%
  select(SP.POP.TOTL,
         iso3c,
         longitude,
         latitude)

full_country_data <- full_country_data %>%
  merge(wdi, by.x = "iso", by.y = "iso3c", all.x = T) %>%
  mutate(
    longitude = as.numeric(as.character(longitude)),
    latitude  = as.numeric(as.character(latitude)),
    `COVID-19 Cases: Confirmed (per 1,000 people)` =  (`COVID-19 Cases: Confirmed`/SP.POP.TOTL)*1000,
    `COVID-19 Cases: Deaths (per 1,000 people)` =  (`COVID-19 Cases: Deaths`/SP.POP.TOTL)*1000
  ) 

full_country_data <- full_country_data %>%
  group_by(iso) %>% 
  arrange(date) %>%
  mutate(`COVID-19 Cases: New Cases (Daily)` = `COVID-19 Cases: Confirmed` - dplyr::lag(`COVID-19 Cases: Confirmed`, n = 1, default = NA),
         `COVID-19 Cases: New Deaths (Daily)`   = `COVID-19 Cases: Deaths`    - dplyr::lag(`COVID-19 Cases: Deaths`,    n = 1, default = NA))

full_country_data$`COVID-19 Cases: New Deaths (Daily)` <- ifelse(full_country_data$`COVID-19 Cases: New Deaths (Daily)`, 
                                                                 NA, full_country_data$`COVID-19 Cases: New Deaths (Daily)`)

fullcd <- melt(full_country_data, id = c("iso", "name", "date", "latitude", "longitude")) %>%
  mutate(
    value = as.numeric(value)
  ) 

cc <- fullcd %>%
  dplyr::filter(date ==  max(date)) %>%
  rename(mrv = value) %>%
  mutate(
    date2 = 2019
  )

dfm <- merge(dfm, cc[, -c(4, 5)], 
             by.x = c("iso3c", "variable", "date"), 
             by.y = c("iso", "variable", "date2"), 
             all = T) %>%
  mutate(
    mrv.x = ifelse(!is.na(mrv.y), mrv.y, mrv.x)
  ) %>%
  select(-mrv.y) %>%
  rename(mrv = mrv.x)

# Country metadata --------------------------------------------------------

cm <- read_excel("input/country_metadata.xlsx", sheet = "Country - Metadata") %>%
  select("Code",
         "Income Group",
         "Region",
         "Short Name"
         )

dfm <- merge(dfm, cm, by.x = "iso3c", by.y = "Code", all.x = T) %>%
  dplyr::filter(!is.na(Region))

dfm <- dfm[, -c(4, 9, 10, 11)]
dfm <- dfm[!is.na(dfm$value) | !is.na(dfm$mry) | !is.na(dfm$mrv), ]
dfm$topic<- as.character(dfm$topic)
dfm$topic <- ifelse(dfm$variable == "COVID-19 Cases: Confirmed" | 
                    dfm$variable == "COVID-19 Cases: Confirmed (per 1,000 people)" |
                    dfm$variable == "COVID-19 Cases: Deaths (per 1,000 people)" |
                    dfm$variable == "COVID-19 Cases: New Cases (Daily)" |
                    dfm$variable == "COVID-19 Cases: New Deaths (Daily)" |
#                   dfm$variable == "COVID-19 Cases: Active" |
#                   dfm$variable == "COVID-19 Cases: Recovered" | 
                    dfm$variable == "COVID-19 Cases: Deaths", 
                    "COVID-19", dfm$topic)

ccc <- cc %>%
  filter(variable == "COVID-19 Cases: Confirmed") %>%
  mutate(
    longitude = as.numeric(as.character(longitude)),
    latitude  = as.numeric(as.character(latitude)),
    longlab   = paste0(name, ": ", mrv)
  ) %>%
  na.omit()

## Treemap prep
fullcd2 <- fullcd %>%
  merge(unique(dfm[,c('iso3c', 'Region', 'Short Name')]), 
                 by.x = 'iso', by.y = 'iso3c', all.x = T) %>%
  filter(!is.na("Short Name"))

fullcd2 <- fullcd2[!is.na(fullcd2$`Short Name`),]

fullcd2 <- fullcd2[fullcd2$variable == "COVID-19 Cases: Confirmed" |
                   fullcd2$variable == "COVID-19 Cases: Deaths", ]

tmp <- treemap_dat(df = fullcd2,
                   case_type = "COVID-19 Cases: Confirmed")

# Precompute select inputs ------------------------------------------------

age_pop <- sort(unique(dfm[dfm$topic == "Age & Population",]$variable))
covid   <- c("COVID-19 Cases: Confirmed", "COVID-19 Cases: Deaths")
#covid   <- sort(unique(dfm[dfm$topic == "COVID-19",]$variable))
health  <- sort(unique(dfm[dfm$topic == "Health",]$variable))
water   <- sort(unique(dfm[dfm$topic == "Water & Sanitation",]$variable))


# Save data ---------------------------------------------------------------

fst::write_fst(dfm, "input/prod/dfm.fst")
fst::write_fst(ccc, "input/prod/ccc.fst")
fst::write_fst(fullcd2, "input/prod/fullcd2.fst")
fst::write_fst(wld_data, "input/prod/wld_data.fst")
fst::write_fst(indicator_list, "input/prod/indicator_list,fst")
#fst::write_fst(tmp, "input/prod/temporary_file,fst")
readr::write_rds(tmp, "input/prod/temporary_file,rds")
readr::write_rds(age_pop, "input/prod/age_pop.RDS")
readr::write_rds(covid, "input/prod/covid.RDS")
readr::write_rds(health, "input/prod/health.RDS")
readr::write_rds(water, "input/prod/water.RDS")

