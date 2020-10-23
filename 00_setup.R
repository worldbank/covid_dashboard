
library(flexdashboard)
library(WDI)
library(plotly)
library(shiny)
library(reshape2)
library(readxl)
library(treemap)
#library(d3treeR)
library(Hmisc)
library(tidyverse)
library(leaflet)
#library(fst)
library(zoo)
library(curl)
library(wbstats)
library(ggrepel)
library(ggiraph)

source("utils.R")

data_path <- paste0(Sys.getenv("connect_ext_data_folder"), "covid_data/")
data_path_raw <- paste0(data_path, "input/")
# Handles both Windows and Linux (RS Connect server)
if (str_detect(sessioninfo::os_name(), "^Windows")) {
  data_path <- str_replace(data_path, "^/", "")
  data_path_raw <- str_replace(data_path, "^/", "")
}




#--------- make sure everyvody has the required packages

# #CRAN packages
# pkg_needed <- c("flexdashboard",
#                 "WDI",
#                 "plotly",
#                 "shiny",
#                 "reshape2",
#                 "readxl",
#                 "treemap",
#                 "Hmisc",
#                 "leaflet",
#                 "rjson",
#                 "ggiraph"
# )
# 
# a <- pkg_needed  %in% installed.packages()[,1]
# pkg_toinstall <- pkg_needed[!a]  # packages not avaialble in pc
# if (length(pkg_toinstall) > 0) {
#   install.packages(pkg_toinstall)
# }
# 
# lapply(pkg_needed, library, character.only = TRUE)
# 
# # WB packages
# pkg_needed <- c("wbgdata",
#                 "wbgcharts",
#                 "wbgmaps",
#                 "wbggeo"
#                 )
# 
# a <- pkg_needed  %in% installed.packages()[,1]
# pkg_toinstall <- pkg_needed[!a]  # packages not avaialble in pc
# for (i in seq_along(pkg_toinstall)) {
#   
#   devtools::install_github("worldbank/wbgviz", 
#                            subdir = pkg_toinstall[i])
# }
# lapply(pkg_needed, library, character.only = TRUE)
# 
# # third party packages
# if (!("d3treeR" %in% installed.packages()[,1])) {
#   devtools::install_github("timelyportfolio/d3treeR")
# }
# 
# library("d3treeR")

