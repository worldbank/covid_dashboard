# ==================================================
# project:       growth
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    Date
# Modification Date: 
# Script version:    01
# References:
# 
# 
# Output:             output
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("janitor")
library("plotly")

library("changepoint")
#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#----------------------------------------------------------
#   Set up
#----------------------------------------------------------


cv <- fullcd2 %>%  # New cases
  arrange(iso, variable, date)        %>% 
  group_by(iso, variable)             %>% 
  mutate(
    value_lag  = lag(value),        # previous obs
    abs_gr     = value - value_lag,        # new cases
    per_gr     = abs_gr/value_lag,             # percentage change
    per_gr2    = abs_gr/lag(abs_gr) - 1,    
    status = gsub("COVID-19 Cases: ", "", variable) 
  )                                   %>% 
  ungroup()                 %>% 
  select(-latitude, -longitude, -`Short Name`) %>% 
  as_tibble() %>% 
  filter(status  %in% c("Confirmed", "Deaths"))

countrycode <- c("ITA", "COL", "CHN", "IND")
countrycode <- c("ITA", "COL", "CHN")
countrycode <- c("ITA", "COL")
countrycode <- c("ITA")
countrycode <- c("CHN")
status_s    <- "Confirmed"
var         <- "per_gr"
ytitle      <- case_when(
  var == "abs_gr"  ~ "New cases",
  var == "per_gr"  ~ "Percentage growth of new cases",
  var == "per_gr2" ~ "Speed of change of new cases",
  TRUE ~ ""
)

pd_cv <- filter(cv, 
                iso  %in% countrycode,
                status == status_s,
                !is.na(per_gr2),
                !is.infinite(per_gr2))

fig <- plot_ly(data = pd_cv, x = ~date)
fig <- fig %>% 
  add_trace(y =     ~get(var), 
            color = ~iso,
            mode = 'lines+markers', 
            type = "scatter") %>% 
  layout(
    yaxis = list(
      tickformat = "%",
      title      =  ytitle
    ),
    xaxis = list(
      title      =  "Date"
    )
  )
fig 

ggplot(data = pd_cv,
       aes(
         x = date,
         y = get(var),
         color = iso
       )) +
  geom_line() +
  geom_point() 



mvalue = cpt.mean(pd_cv$abs_gr, method = "PELT")
mvalue = cpt.mean(pd_cv$abs_gr, method = "BinSeg")
cpts(mvalue)

#----------------------------------------------------------
#   
#----------------------------------------------------------

#----------------------------------------------------------
#   
#----------------------------------------------------------

#----------------------------------------------------------
#   
#----------------------------------------------------------

#----------------------------------------------------------
#   
#----------------------------------------------------------



plot_ly(
  type="treemap",
  labels=c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
  parents=c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")
)



new_c <- fullcd2 %>%  # New cases
  arrange(iso, variable, date)        %>% 
  group_by(iso, variable)             %>% 
  mutate(
    new    = value - lag(value, 
                         order_by = date),
    status = gsub("COVID-19 Cases: ", "", variable)
  )                                   %>% 
  ungroup()                 %>% 
  select(iso, status, new, date)

new_wl <- new_c    %>% 
  group_by(status) %>% 
  summarise(con = sum(new))



con <- new_wl %>% 
  filter(status == "Confirmed") %>% 
  select(con) %>% 
  pull() %>% 
  format(big.mark=",")


tmp1 <- tmp[tmp$date == max(tmp$date),] 
tmp2 <- tmp[tmp$date == (max(tmp$date) - 1), c(1, 5)] 
tmp2 <- tmp2 %>% 
  rename("val2" = "value") 

tmp1 <- merge(tmp1, tmp2, by = "iso", all.x = T) 

tmp1$new <- tmp1$value - tmp1$val2 