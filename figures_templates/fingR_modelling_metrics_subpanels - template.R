# Code to generate Modelling metrics vs theoretical contribution
## cf. figure 5 and 7 - https://doi.org/10.5194/soil-10-109-2024

library(ggplot2)
library(ggthemes)

path.to.graph <- ".../VM_graphics/Modelling_metrics/"
pt.size <- 2
lwidth <- .8
y.max <- 57 # adapt to the maximum value of the metric across tracer selection approaches
y.breaks.steps <- 10

## Example using CRPS values
# dataframe : OP

for (grp in c("Cropland", "Forest", "Subsoil")) {
  
  # Graphic
  plt.CRPS <- ggplot() +
    aes(x = OCRPS[[colnames(OCRPS)[grepl(grp, colnames(OCRPS)) & grepl("Obs", colnames(OCRPS))]]]*100) +
    
    # points
    geom_point(aes(y = OCRPS[[colnames(OCRPS)[grepl(grp, colnames(OCRPS)) & grepl("_NSW.MM$", colnames(OCRPS))]]]*100), shape = 21, size = pt.size, colour = "#ff8288", fill = "#ff8288") +
    geom_point(aes(y = OCRPS[[colnames(OCRPS)[grepl(grp, colnames(OCRPS)) & grepl("_SW.MM$", colnames(OCRPS))]]]*100), shape = 1, size = pt.size, colour = "#910b1c") +
    # lines
    geom_smooth(aes(y = OCRPS[[colnames(OCRPS)[grepl(grp, colnames(OCRPS)) & grepl("_NSW.MM$", colnames(OCRPS))]]]*100), method = "loess", formula = y~poly(x,2), se = FALSE, colour = "#ff8288", linewidth = lwidth) +
    geom_smooth(aes(y = OCRPS[[colnames(OCRPS)[grepl(grp, colnames(OCRPS)) & grepl("_SW.MM$", colnames(OCRPS))]]]*100), method = "loess", formula = y~poly(x,2), se = FALSE, colour = "#910b1c", linewidth = lwidth) +
    
    # scale graph
    scale_x_continuous("", limits = c(0, 100), breaks = seq(0, 100, 20), sec.axis = dup_axis()) + #Observed
    scale_y_continuous("", limits = c(0, y.max), breaks = seq(0, 100, y.breaks.steps), sec.axis = dup_axis()) + #Predicted
    
    # theme
    theme_bw() + theme(legend.position = "none", axis.title = element_text(size = 13),
                       axis.text.y = element_text(size = 12),
                       axis.text.x = element_text(size = 12), #, angle = 45, hjust = 1
                       panel.grid = element_line(linewidth = 0.5), axis.ticks = element_line(linewidth = 0.3), plot.background = element_rect(fill='transparent', color=NA))
  
  # Save graphic
  #ggsave(filename = paste0(paste("CRPS_median", "criteria", grp, "all", sep = "_"), ".pdf"), plot = plt.CRPS, path = path.to.graph.CRPS, width = 12, height = 12, units = c("cm"))
  ggsave(filename = paste0(paste("CRPS_median", "criteria", grp, "all", sep = "_"), ".png"), plot = plt.CRPS, path = path.to.graph.CRPS, width = 12, height = 12, units = c("cm"), dpi = 200)
}