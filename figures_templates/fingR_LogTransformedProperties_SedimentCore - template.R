# Code to generate log transfmored properties along a sediment core
## cf. figure 5 and 7 - https://doi.org/10.5194/soil-10-109-2024

library(ggplot2) ; library(ggthemes) ; library(dplyr)

# Data preparation
tracers.all


dir.core <- "Core_tracer_distribution"
if(!dir.exists(file.path( dir.core ))){ #create the folder if it doesn't already exist
  dir.create(file.path(dirname(dir.core),  dir.core))
}

dt.graph <- dplyr::filter(data.prop, Class == "Target")
dt.graph$depth_cm <- seq(6.5, 37.5, 1)

dt.sources  <- dplyr::filter(data.prop, Class != "Target")

dt.srcs <- dt.sources %>%
  dplyr::select(Class, all_of(tracers.all)) %>%
  group_by(Class) %>%
  summarise(across(everything(), list(Mean=mean, sd=sd)))

for(prop in tracers.all){
  dt.srcs[[paste0(prop, "_Mean")]] <- round(dt.srcs[[paste0(prop, "_Mean")]] , fingR::lvl.signif(dt.sources[[prop]]))
  dt.srcs[[paste0(prop, "_sd")]] <- round(dt.srcs[[paste0(prop, "_sd")]] , fingR::lvl.signif(dt.sources[[prop]]))
}

dt.srcs.2 <- dt.sources %>%
  dplyr::select(Class, all_of(tracers.all))
dt.srcs.lg <- dt.srcs.2
dt.srcs.lg[2:ncol(dt.srcs.2)] <- log(dt.srcs.2[2:ncol(dt.srcs.2)])

dt.srcs.lg <- dt.srcs.lg %>%
  group_by(Class) %>%
  summarise(across(everything(), list(Mean=mean, sd=sd)))
dt.srcs.lg[2:ncol(dt.srcs.lg)] <- round(dt.srcs.lg[2:ncol(dt.srcs.lg)], 4)


## Visualisation of dataframes
# dt.srcs
#dt.srcs.lg


# Graphic

rect.alpha <- 0.15
sources.col <- c("#ff6600", "#85cf00", "#003e82")

tracers.all.err <- c("TOC_SD", "TN_SD", "Al_RMSE", "Ca_RMSE", "K_RMSE", "Si_RMSE", "Sr_RMSE", "Zn_RMSE", "Zr_RMSE", "L_star_D65_SD", "a_star_D65_SD", "b_star_D65_SD", "C_star_D65_SD", "A2_uncertainty", "Q7.4_uncertainty", "Goethite_525nm_FstD_uncertainty", "h_D65_SD", "A1_uncertainty", "Gt_uncertainty")

for (i in 1:length(tracers.all)) {
  
  trcr <- tracers.all[i]
  trcr.err <- tracers.all.err[i]
  
  plot.graph <- ggplot(dt.graph) + aes(x = depth_cm) +
    # source properties
    geom_segment(data = dt.srcs.lg, aes(x = 6, xend = 38, y = .data[[paste0(trcr, "_Mean")]], yend = .data[[paste0(trcr, "_Mean")]]), colour = sources.col, linewidth = 0.5) +
    geom_rect(data = dt.srcs.lg, aes(xmin = 6, xmax = 38, ymin = .data[[paste0(trcr, "_Mean")]] - .data[[paste0(trcr, "_sd")]],  ymax = .data[[paste0(trcr, "_Mean")]] + .data[[paste0(trcr, "_sd")]]), fill = sources.col, alpha = rect.alpha, inherit.aes = FALSE) +
    #geom_text(data = dt.srcs.lg, aes(x = 5.2, y = .data[[paste0(trcr, "_Mean")]], label = Class), colour = sources.col) +
    # error
    geom_ribbon(aes(ymin = log(.data[[trcr]] - .data[[trcr.err]]), ymax = log(.data[[trcr]] + .data[[trcr.err]])), alpha = .7, fill = "#f4f8fb") +
    # Measure point and line
    geom_point(aes(y = log(.data[[trcr]])), pch = 16) +
    geom_line(aes(y = log(.data[[trcr]])), linewidth = 0.45) +
    # Flip the graph
    scale_x_reverse("", limits = c(38, 6), breaks = seq(7, 38, 2), sec.axis = dup_axis()) + 
    scale_y_continuous("", position = "right", sec.axis = dup_axis()) +
    #scale_y_continuous("", limits = c(min(dt.srcs.lg[[paste0(trcr, "_Mean")]] - dt.srcs.lg[[paste0(trcr, "_sd")]]), max(dt.srcs.lg[[paste0(trcr, "_Mean")]] + dt.srcs.lg[[paste0(trcr, "_sd")]])), position = "right", sec.axis = dup_axis()) +
    coord_flip() +
    # THEME
    theme_clean() +
    theme(legend.position = "none", axis.title = element_text(size = 15), axis.text.y = element_text(size = 11), axis.text.x = element_text(size = 11), #, angle = 45, hjust = 1
          panel.grid = element_line(linewidth = 0.5), axis.ticks = element_line(linewidth = 0.3), plot.background = element_rect(fill='transparent', color=NA))
  
  # Save graphic
  #ggsave(filename = "Core_TOC.pdf", plot = plot.graph, path = dir.core, height = 17, width = 9, units = c("cm"))
  ggsave(filename = paste0("Core_signature_", trcr, ".png"), plot = plot.graph, path = dir.core, height = 17, width = 9, units = c("cm"), dpi = 200)
}