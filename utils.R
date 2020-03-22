treemap_dat <- function(df,
                        case_type = "COVID-19 Cases: Confirmed") {
  
  tmp <- df[df$variable == case_type,]
  tmp <- tmp[which(tmp$value > 0),]
  tmp$date <- as.Date(tmp$date)
  
  return(tmp)
}