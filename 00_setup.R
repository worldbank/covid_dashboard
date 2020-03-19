
#--------- make sure everyvody has the required packages

pkg_needed <- c("flexdashboard",
                "WDI",
                "wbgdata",
                "plotly",
                "shiny",
                "reshape2",
                "readxl",
                "treemap",
                "d3treeR",
                "Hmisc",
                "wbgcharts",
                "wbgmaps",
                "wbggeo",
                "leaflet")

a <- pkg_needed  %in% installed.packages()[,1]
pkg_toinstall <- pkg_needed[!a]  # packages not avaialble in pc
if (length(pkg_toinstall) > 0) {
  install.packages(pkg_toinstall)
}
  