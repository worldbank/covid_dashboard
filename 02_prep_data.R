
# WDI data ----------------------------------------------------------------

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
# Replace with local data
df <- readr::read_rds("input/wbgdata.rds")

dfm <- melt(df, id = c("iso3c", "date"))

dfmo <- dfm[!is.na(dfm$value),]

mry <- dfmo %>% 
  group_by(iso3c, variable) %>% # filter to max year, each indicator, each country
  filter(date == max(date))

mry <- mry %>%
  rename("mrv" = "value") %>%
  rename("mry" = "date")

mry$date <- 2019

dfm <- merge(dfm, mry, by = c("date", "iso3c", "variable"), all.x = T)


# COVID data --------------------------------------------------------------

dat_raw_1 <- readr::read_rds("input/dat_raw_1.rds")
dat_raw_2 <- readr::read_rds("input/dat_raw_2.rds")

wld_data = lapply(dat_raw_2, function(j) cbind(j$date, j$confirmed, j$deaths, j$recovered))
wld_data = data.frame(do.call('rbind', wld_data), stringsAsFactors = FALSE)
colnames(wld_data) = c( "date", "confirmed", "deaths", "recovered")
iso = dat_raw_1[["iso"]]
name = dat_raw_1[["name"]]
wld_data <- cbind(iso, name, wld_data) 

# get country list
full_country_data <- readr::read_rds("input/full_country_data.rds")

full_country_data$date <- as.Date(full_country_data$date, "%Y/%m/%d")

fullcd <- melt(full_country_data, id = c("iso", "name", "date"))

fullcd$value <- as.numeric(fullcd$value)

cc <- fullcd[fullcd$date == max(fullcd$date),]
cc <- cc %>%
  rename("mrv" = "value")

cc$date2 <- 2019

dfm <- merge(dfm, cc, by.x = c("iso3c", "variable", "date"),  by.y = c("iso", "variable", "date2"), 
             all = T)



# Country metadata --------------------------------------------------------

cm <- read_excel("input/country_metadata.xlsx",
                 sheet = "Country - Metadata")
cm1 <- cm[, c(1, 3, 4, 30)]

dfm <- merge(dfm, cm1, by.x = "iso3c", by.y = "Code", all.x = T)

dfm$mrv.x <- ifelse(!is.na(dfm$mrv.y), dfm$mrv.y, dfm$mrv.x)
dfm$mrv.y <- name <- NULL
dfm <- dfm %>%
  rename("mrv" = "mrv.x")

wdi_dat <- WDI(indicator = c("SP.POP.TOTL"), start = 2018, end = 2018, extra = TRUE) 
wdi <- wdi_dat[, c(3, 5, 8, 9)]

#ccc <- fullcd[fullcd$variable == "confirmed",]
ccc <- cc[cc$variable == "confirmed",]

ccc <- merge(ccc, wdi, by.x = "iso", by.y = "iso3c", all.x = T)
ccc$longitude <- as.numeric(as.character(ccc$longitude))
ccc$latitude <- as.numeric(as.character(ccc$latitude))
ccc$longlab <- paste0(ccc$name, ": ", ccc$mrv)
ccc <- na.omit(ccc)