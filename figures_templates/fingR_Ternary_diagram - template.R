# Code to generate Ternary diagrams
## cf. figure 7d - https://doi.org/10.1016/j.scitotenv.2024.174546


library(Ternary)
library(ggplot2)
library(plyr)


pt.pch <- 4

Ternary::TernaryPlot(alab = "Forest", blab = "Undecontaminated", clab = "Subsoil", lab.offset = .1, grid.lines = 5)
# contour the VM
#Ternary::TernaryDensityContour(VM.contribs$undecontamA[, c("Undecontaminated", "Forest", "Subsoil")], resolution = 50L)
# theo VM multi
Ternary::TernaryPoints(VM.contribs$undecontamA[, c("Forest", "Undecontaminated", "Subsoil")], pch = 15, col = "#9999cc")
# pred VM multi
#Ternary::TernaryDensityContour(MultiM.VM$undecontamA$nDFA.h_KS[, c("MultiMMedian_Undecontaminated", "MultiMMedian_Forest", "MultiMMedian_Subsoil")])
Ternary::TernaryPoints(MultiM.VM$undecontamA$nDFA.h_KS[, c("MultiMMedian_Forest", "MultiMMedian_Undecontaminated", "MultiMMedian_Subsoil")], pch = 18, col = "#6699ff")
# pred actual sediments - MixSIAR
#Ternary::TernaryPoints(preds.actual$undecontamA$nDFA.h_KS[, c("Median_Forest", "Median_Undecontaminated", "Median_Subsoil")], pch = 20, col = all.colours[1])
# pred actual sediments - BMM
#Ternary::TernaryPoints(BMM.preds.actual$undecontamA$nDFA.h_KS[, c("Median_Forest", "Median_Undecontaminated", "Median_Subsoil")], pch = 20, col= all.colours[2])
# pred actual sediments - Multi
Ternary::TernaryPoints(Multi.preds.corrected.actual$undecontamA$nDFA.h_KS[, c("Median_Forest", "Median_Undecontaminated", "Median_Subsoil")], pch = 16, col = "#33cc33")


pdf(".../Fingerprinting_modelling/All_graphics/Ternary/TernaryPrediction_Ternary_Hinge_all.pdf", width = 8.5, height = 8.5)