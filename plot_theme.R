cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind palette

ColorBlocksFacet <- c("#e0e0e0")
my.theme = theme(panel.grid.major = element_line(size = 0.3,
                                                 linetype = 'solid',
                                                 colour = ColorBlocksFacet),
                 panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                 panel.grid.minor = element_blank(), 
                 plot.background = element_rect(fill = "white",colour = NA),
                 axis.line = element_line(colour = "black", size = 0.3),
                 panel.border = element_rect(color = "black", fill = "transparent"),
                 axis.text = element_text(colour = "black"),
                 axis.ticks.length = unit(0.25, "cm"), 
                 legend.key = element_rect(fill = "white"), 
                 legend.background = element_rect(fill = "white"),
                 strip.background = element_rect(color="black", fill="transparent", linetype="solid", size = 0.5, 
                                                 
                 ))

col = c("grey98", (RColorBrewer::brewer.pal(n = 12, name = "Paired")),RColorBrewer::brewer.pal(n = 8, name = "Dark2"))
col_fam = c(RColorBrewer::brewer.pal(n = 12, name = "Paired"),"grey98")
conf3 =function(x) { #x is a dataframe
  r <- c(mean(x) - sd(x),
         mean(x)-sd(x),
         mean(x),
         mean(x)+sd(x), 
         mean(x) + sd(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 3, spaceLegend = 0.1) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}
