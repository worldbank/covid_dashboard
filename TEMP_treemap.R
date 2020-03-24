source("00_setup.R")
source("03_load_data.R")

tmp <- tmp[tmp$date == max(tmp$date),]
tmp <- tmp[!is.na(tmp$Region),]
tmp$Region <- gsub(" ", "", tmp$Region, fixed = TRUE)
tmp$name <- gsub(" ", "", tmp$name, fixed = T)

# Compute regional aggregates
tmp2 <- tmp %>%
  group_by(Region) %>%
  summarise(
    name = unique(Region),
    value = sum(value)
  ) %>%
  mutate(
    Region = "World"
  ) %>%
  ungroup()

# Combine Regional aggregates with country level data
tmp3 <- bind_rows(tmp, tmp2) %>%
  select(Region, name, value) %>%
  filter(
    Region %in% c("World", "Europe&CentralAsia", "EastAsia&Pacific", 
                  "NorthAmerica", "MiddleEast&NorthAfrica", "LatinAmerica&Caribbean", 
                  "SouthAsia", "Sub-SaharanAfrica")
  )
  

tmp3 %>%
  plot_ly( 
        type    = "treemap",
        labels  = tmp3$name,
        parents = tmp3$Region,
        values  = tmp3$value
        )