treemap_dat <- function(df,
                        case_type = "COVID-19 Cases: Confirmed") {
  
  tmp <- df[df$variable == case_type,]
  tmp <- tmp[which(tmp$value > 0),]
  tmp$date <- as.Date(tmp$date)
  
  return(tmp)
}


# Color palette -----------------------------------------------------------

regions <- c(
  "South Asia",
  "Sub-Saharan Africa",
  "Europe & Central Asia",
  "Middle East & North Africa",
  "Latin America & Caribbean",
  "North America",
  "East Asia & Pacific"
)

# palette <- c("#055bb3", "#fdb714", "#eb1c2d", "#872b90", "#00ab51", "#515151", "#e16a2d")

palette <- c("#5F4690", "#1D6996", "#38A6A5", "#0F8554", "#73AF48", "#EDAD08", 
             "#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377", 
             "#66C5CC", "#F6CF71", "#F89C74", "#DCB0F2", "#87C55F", "#9EB9F3",
             "#E17C05", "#CC503E", "#94346E", "#6F4070", "#994E95", "#666666",
             "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C",
             "#FE88B1", "#C9DB74", "#8BE0A4", "#B497E7", "#D3B484", "#B3B3B3")


reg_palette <- palette[1:length(regions)]

# scales::show_col(palette)
# scales::show_col(reg_palette)

reg_color   <- setNames(
  reg_palette,
  regions
)

inc_color   <- c(
  "Low income" = "#1b9e77", 
  "Lower middle income" = "#d95f02",
  "Upper middle income" = "#7570b3", 
  "High income" = "#1f78b4")


color_map <- c(
  `South Asia`                 = "#055bb3",
  `Sub-Saharan Africa`         = "#fdb714",
  `Europe & Central Asia`      = "#eb1c2d",
  `Middle East & North Africa` = "#872b90",
  `Latin America & Caribbean`  = "#00ab51",
  `North America`              = "#515151",
  `East Asia & Pacific`        = "#e16a2d"
)

color_map2 <- tibble(
  Region = regions, 
  color  = reg_palette
)
