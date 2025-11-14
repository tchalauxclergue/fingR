# Code to generate Predicted contribution along a sediment core
## cf. figure 8 - https://doi.org/10.5194/soil-10-109-2024
library(ggplot2)
library(ggthemes)


path.to.graph.core <- "MixSIAR_modelling/MixSIAR_core_graphics/"
marge <- 19
zero.l.width <- 0.55

for (grp in c("Cropland", "Forest", "Subsoil")) {
  
  grp.val <- paste0("Median_", grp)
  grp.RMSE <- paste0("RMSE_", grp)
  
  ###### Minimum-maximum ###### 
  plot.core <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = zero.l.width, color = "#b5b8b7") +
    geom_hline(yintercept = 100, linetype = "dashed", linewidth = zero.l.width, color = "#b5b8b7") +
    # error
    geom_ribbon(aes(x = graph.core[["NSW.MM"]]$depth, ymin = round(graph.core[["NSW.MM"]][[grp.val]]*100)-round(graph.core[["NSW.MM"]][[grp.RMSE]]*100), ymax = round(graph.core[["NSW.MM"]][[grp.val]]*100)+round(graph.core[["NSW.MM"]][[grp.RMSE]]*100) ), fill = "#ff8288", alpha = 0.1) +
    geom_ribbon(aes(x = graph.core[["SW.MM"]]$depth, ymin = round(graph.core[["SW.MM"]][[grp.val]]*100)-round(graph.core[["SW.MM"]][[grp.RMSE]]*100), ymax = round(graph.core[["SW.MM"]][[grp.val]]*100)+round(graph.core[["SW.MM"]][[grp.RMSE]]*100) ), fill = "#910b1c", alpha = 0.1) +
    # points
    geom_point(aes(x = graph.core[["NSW.MM"]]$depth, y = round(graph.core[["NSW.MM"]][[grp.val]]*100)), colour = "#ff8288", fill = "#ff8288", shape = 21, size = 2) +
    geom_line(aes(x = graph.core[["NSW.MM"]]$depth, y = round(graph.core[["NSW.MM"]][[grp.val]]*100)),  colour = "#ff8288", linewidth = .7) +
    geom_point(aes(x = graph.core[["SW.MM"]]$depth, y = round(graph.core[["SW.MM"]][[grp.val]]*100)), colour = "#910b1c", shape = 1, size = 2) +
    geom_line(aes(x = graph.core[["SW.MM"]]$depth, y = round(graph.core[["SW.MM"]][[grp.val]]*100)),  colour = "#910b1c", linewidth = .7) +
    # error line
    geom_line(aes(x = graph.core[["NSW.MM"]]$depth, y = round(graph.core[["NSW.MM"]][[grp.val]]*100)-round(graph.core[["NSW.MM"]][[grp.RMSE]]*100)),  colour = "#ff8288", linewidth = .4, alpha = .25) + 
    geom_line(aes(x = graph.core[["NSW.MM"]]$depth, y = round(graph.core[["NSW.MM"]][[grp.val]]*100)+round(graph.core[["NSW.MM"]][[grp.RMSE]]*100)),  colour = "#ff8288", linewidth = .4, alpha = .25) +
    geom_line(aes(x = graph.core[["SW.MM"]]$depth, y = round(graph.core[["SW.MM"]][[grp.val]]*100)-round(graph.core[["SW.MM"]][[grp.RMSE]]*100)),  colour = "#910b1c", linewidth = .4, alpha = .25) + 
    geom_line(aes(x = graph.core[["SW.MM"]]$depth, y = round(graph.core[["SW.MM"]][[grp.val]]*100)+round(graph.core[["SW.MM"]][[grp.RMSE]]*100)),  colour = "#910b1c", linewidth = .4, alpha = .25) +
    # scale graph
    scale_x_reverse("", breaks = seq(7, 38, 2), sec.axis = dup_axis()) +
    scale_y_continuous("", breaks = seq(0, 100, 20), limits = c(0-marge, 100+marge), sec.axis = dup_axis()) +
    coord_flip() +
    # theme
    theme_clean() +
    theme(legend.position = "none", axis.title = element_text(size = 15), axis.text.y = element_text(size = 11), axis.text.x = element_text(size = 11), #, angle = 45, hjust = 1
          panel.grid = element_line(linewidth = 0.5), axis.ticks = element_line(linewidth = 0.3), plot.background = element_rect(fill='transparent', color=NA))
  
  #ggsave(filename = paste0(paste("MixSIAR_core", "MinMax", grp, sep = "_"), ".pdf"), plot = plot1.core, path = path.to.graph.core, height = 17, width = 9, units = c("cm"))
  ggsave(filename = paste0(paste("MixSIAR_core", "MinMax", grp, sep = "_"), ".png"), plot = plot.core, path = path.to.graph.core, height = 17, width = 9, units = c("cm"), dpi = 200)
}