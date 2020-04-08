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

#' plot_cog_map
#'
#' @param mapdf data.frame: Map boundaries 
#' @param cgdf data.frame: Center of Gravity data
#' @param cg_x object: Variable to be mapped to x aesthetic
#' @param cg_y object: Variable to be mapped to y aesthetic
#' @param cg_size object: Variable to be mapped to size aesthetic
#' @param cg_color object: Variable to be mapped to color aesthetic
#' @param cg_tooltip object: Variable to be displayed by tooltip
#' @param title character: Plot title
#'
#' @return ggplot
#' @export
#'
plot_cog_map <- function(mapdf, 
                         cgdf,
                         cg_x,
                         cg_y,
                         cg_size,
                         cg_color,
                         cg_tooltip,
                         title) {
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = mapdf,
      ggplot2::aes(x = long, y = lat, group = group),
      color = 'white',
      size = 0.1,
      fill = "grey"
    ) +
    ggiraph::geom_point_interactive(
      data = cgdf,
      ggplot2::aes(
        x = {{cg_x}},
        y = {{cg_y}},
        size = {{cg_size}},
        alpha = {{cg_color}},
        color = {{cg_color}},
        tooltip = paste("Value:", {{cg_tooltip}},
                        "<br />Year:", {{cg_color}})
      )
    ) +
    ggplot2::scale_colour_date(low = "#FF7878", high = "#780000") +
    ggplot2::scale_size_continuous(range = c(1, 10)) +
    ggplot2::labs(size = "Daily deaths", color = "Date") +
    ggplot2::guides(alpha = FALSE, size = FALSE, color = FALSE) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = 'white', color = 'white'),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 20),
      legend.position = "bottom")
  
  return(p)
}

