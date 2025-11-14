# Code to generate Virtual Mixtures observed vs predicted contributions
## cf. figure 6 - https://doi.org/10.5194/soil-10-109-2024
library(ggplot2)
library(ggthemes)

path.to.graph <- ".../VM_graphics/ObsPred_all/"
pt.size <- 2
lwidth <- .8
acceptance <- 5 # corresponding to 5% error around 1:1 axis

# dataframe : OP

for (grp in c("Cropland", "Forest", "Subsoil")) {
  
  # Grqphic
  plt.OP <- ggplot() +
    aes(x = OP[[colnames(OP)[grepl(grp, colnames(OP)) & grepl("Obs", colnames(OP))]]]*100) +
    
    # 1:1 line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    # acceptance lines
    geom_abline(slope = 1, intercept = acceptance, linetype = "dotted") +
    geom_abline(slope = 1, intercept = -acceptance, linetype = "dotted") +
    
    # points
    # 
    geom_point(aes(y = OP[[colnames(OP)[grepl(grp, colnames(OP)) & grepl("_NSW.mean.sd", colnames(OP))]]]*100), shape = 5, size = pt.size, colour = "#00CCFF", fill = "#00CCFF") +
    geom_point(aes(y = OP[[colnames(OP)[grepl(grp, colnames(OP)) & grepl( "_SW.mean.sd", colnames(OP))]]]*100), shape = 6 , size = pt.size, colour = "#003e82") +
    scale_size_manual(values = c(rep(pt.size, 4))) + # manage points size
    # regression line
    geom_smooth(aes(y = OP[[colnames(OP)[grepl(grp, colnames(OP)) & grepl("_NSW.mean.sd", colnames(OP))]]]*100), method = "lm", formula = y~x, se = FALSE, colour = "#00CCFF", alpha = 0.2, linewidth = lwidth) +
    geom_smooth(aes(y = OP[[colnames(OP)[grepl(grp, colnames(OP)) & grepl("_SW.mean.sd", colnames(OP))]]]*100), method = "lm", formula = y~x, se = FALSE, colour = "#003e82", alpha = 0.2, linewidth = lwidth) +
    
    # graph scales
    # scale_x_continuous("Observed", limits = c(0, 100), breaks = seq(0, 100, 20), sec.axis = dup_axis()) + #Observed
    scale_x_continuous("", limits = c(0, 100), breaks = seq(0, 100, 20), sec.axis = dup_axis()) + #Observed
    # scale_y_continuous("Predicted", limits = c(0, 100), breaks = seq(0, 100, 20), sec.axis = dup_axis()) + #Predicted
    scale_y_continuous("", limits = c(0, 100), breaks = seq(0, 100, 20), sec.axis = dup_axis()) + #Predicted
    
    # theme
    theme_bw() +
    theme(legend.position = "none", axis.title = element_text(size = 13),
          axis.text.y = element_text(size = 12), 
          axis.text.x = element_text(size = 12), #, angle = 45, hjust = 1
          panel.grid = element_line(linewidth = 0.5),
          axis.ticks = element_line(linewidth = 0.3),
          plot.background = element_rect(fill = 'transparent', color = NA))
  
  # Saving graphic
  #ggsave(filename = paste0(paste("obspred_median", "MeanSD", grp, "all", sep = "_"), ".pdf"), plot = plt.OP, path = path.to.graph, width = 12, height = 12, units = c("cm"))
  ggsave(filename = paste0(paste("obspred_median", "MeanSD", grp, "all", sep = "_"), ".png"), plot = plt.OP, path = path.to.graph, width = 12, height = 12, units = c("cm"), dpi = 200)
}