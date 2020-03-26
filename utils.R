treemap_dat <- function(df,
                        case_type = "COVID-19 Cases: Confirmed") {
  
  tmp <- df[df$variable == case_type,]
  tmp <- tmp[which(tmp$value > 0),]
  tmp$date <- as.Date(tmp$date)
  
  return(tmp)
}


# Color palette -----------------------------------------------------------

reg_palette <- c("#055bb3", "#fdb714", "#eb1c2d", "#872b90", "#00ab51", "#515151", "#e16a2d")
reg_color   <- setNames(
  reg_palette,
  c(
    "South Asia",
    "Sub-Saharan Africa",
    "Europe & Central Asia",
    "Middle East & North Africa",
    "Latin America & Caribbean",
    "North America",
    "East Asia & Pacific"
  )
)

color_map <- c(
  `South Asia`                 = "#055bb3",
  `Sub-Saharan Africa`         = "#fdb714",
  `Europe & Central Asia`      = "#eb1c2d",
  `Middle East & North Africa` = "#872b90",
  `Latin America & Caribbean`  = "#00ab51",
  `North America`              = "#515151",
  `East Asia & Pacific`        = "#e16a2d"
)

color_map2 <- tribble(
  ~Region                      , ~color,
  "South Asia"                 , "#055bb3",
  "Sub-Saharan Africa"         , "#fdb714",
  "Europe & Central Asia"      , "#eb1c2d",
  "Middle East & North Africa" , "#872b90",
  "Latin America & Caribbean"  , "#00ab51",
  "North America"              , "#515151",
  "East Asia & Pacific"        , "#e16a2d",
)
