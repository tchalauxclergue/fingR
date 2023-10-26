#' Organic matter, geochemical and colorimetric properties of potential source material, target sediment and laboratory mixtures for conducting sediment fingerprinting approaches in the Mano Dam Reservoir (Hayama Lake) catchment, Fukushima Prefecture, Japan.
#'
#' The current dataset was compiled to study sediment fingerprintings practices, i.e tracer selection and contribution
#' modelling. Colorimetric properties analysed with a portable diffuse reflectance spectrophotometer (Konica Minolta
#' CM-700d) and geochemical contents obtained with an energy dispersive X-ray fluorescence spectrometer (ED-XRF Epsilon
#' 4) were analysed in potential source material that may supply sediment to coastal rivers draining the main Fukushima
#' radioactive pollution plume (Japan). Three potential soil source materials (n = 56) were considered: cropland (n = 24), as
#' non-decontaminated soil before the application of local decontamination policies: forest soils (n = 22) and subsurface
#' material originating from channel bank collapse or landslides (n = 10). A sediment core was collected in the Mano Dam
#' lake (Hayama lake) on the 6th June 2021 and was sectionned into 1-cm layers (n = 34). Laboratory mixtures (n = 27) were
#' made to assess different contribution levels from the sources. In addition to colorimetric and geochemical properties,
#' organic matter and stable isotopes were analysed by EA-IRMS for sources and sediments samples.
#'
#' The variables are as follows:
#'
#' @docType data
#'
#' @format A data frame with 115 rows and 391 variables:
#' \describe{
#'   \item{Sample_name}{sample unique name}
#'   \item{Material}{fingerprinting type: source or target}
#'   \item{Nature}{general nature of material: soil, sediment, laboratory mixture}
#'   \item{Class}{general land use class: target, cropland, forest, subsoil or laboratory mixture}
#'   \item{Catchment}{river catchment name}
#'   \item{River}{name of the river in the sample vicinity}
#'   \item{Lithology}{lithology (GSJ)}
#'   \item{Lithology_simp}{lithology simplified (GSJ)}
#'   \item{Pedology_SG_CSCS_2011}{pedology soil group (NARO Comprehensive soil classification system, 2011)}
#'   \item{Sample_size}{particle size sieving threshold (< 63 µm (micro meter))}
#'   \item{"TOC_PrC, TN_PrC, ratio_TOC_TN, d13C_PrM, d15N_PrM, Al_mg.kg, Ca_mg.kg, Co_mg.kg, Cr_mg.kg, Cu_mg.kg, Fe_mg.kg, K_mg.kg, Mg_mg.kg, Mn_mg.kg, Ni_mg.kg, Pb_mg.kg,
#'   Rb_mg.kg, Si_mg.kg, Sr_mg.kg, Ti_mg.kg, Zn_mg.kg, Zr_mg.kg, L_star_D65, a_star_D65, b_star_D65, C_star_D65, h_D65, A1, A2, A3, Gt, Q7.4}{property value for each sample}
#'   \item{R_400nm, R_410nm, R_420nm, R_430nm, R_440nm, R_450nm, R_460nm, R_470nm, R_480nm, R_490nm, R_500nm, R_510nm, R_520nm, R_530nm, R_540nm, R_550nm, R_560nm, R_570nm,
#'   R_580nm, R_590nm, R_600nm, R_610nm, R_620nm, R_630nm, R_640nm, R_650nm, R_660nm, R_670nm, R_680nm, R_690nm, R_700nm, FstD_R_400nm, FstD_R_410nm, FstD_R_420nm, FstD_R_430nm,
#'   FstD_R_440nm, FstD_R_450nm, FstD_R_460nm, FstD_R_470nm, FstD_R_480nm, FstD_R_490nm, FstD_R_500nm, FstD_R_510nm, FstD_R_520nm, FstD_R_530nm, FstD_R_540nm, FstD_R_550nm,
#'   FstD_R_560nm, FstD_R_570nm, FstD_R_580nm, FstD_R_590nm, FstD_R_600nm, FstD_R_610nm, FstD_R_620nm, FstD_R_630nm, FstD_R_640nm, FstD_R_650nm, FstD_R_660nm, FstD_R_670nm,
#'   FstD_R_680nm, FstD_R_690nm, FstD_R_700nm, SecD_R_400nm, SecD_R_410nm, SecD_R_420nm, SecD_R_430nm, SecD_R_440nm, SecD_R_450nm, SecD_R_460nm, SecD_R_470nm, SecD_R_480nm,
#'   SecD_R_490nm, SecD_R_500nm, SecD_R_510nm, SecD_R_520nm, SecD_R_530nm, SecD_R_540nm, SecD_R_550nm, SecD_R_560nm, SecD_R_570nm, SecD_R_580nm, SecD_R_590nm, SecD_R_600nm,
#'   SecD_R_610nm, SecD_R_620nm, SecD_R_630nm, SecD_R_640nm, SecD_R_650nm, SecD_R_660nm, SecD_R_670nm, SecD_R_680nm, SecD_R_690nm, SecD_R_700nm, RF_R_400nm, RF_R_410nm, RF_R_420nm,
#'   RF_R_430nm, RF_R_440nm, RF_R_450nm, RF_R_460nm, RF_R_470nm, RF_R_480nm, RF_R_490nm, RF_R_500nm, RF_R_510nm, RF_R_520nm, RF_R_530nm, RF_R_540nm, RF_R_550nm, RF_R_560nm, RF_R_570nm,
#'   RF_R_580nm, RF_R_590nm, RF_R_600nm, RF_R_610nm, RF_R_620nm, RF_R_630nm, RF_R_640nm, RF_R_650nm, RF_R_660nm, RF_R_670nm, RF_R_680nm, RF_R_690nm, RF_R_700nm, SecD_RF_R_400nm,
#'   SecD_RF_R_410nm, SecD_RF_R_420nm, SecD_RF_R_430nm, SecD_RF_R_440nm, SecD_RF_R_450nm, SecD_RF_R_460nm, SecD_RF_R_470nm, SecD_RF_R_480nm, SecD_RF_R_490nm, SecD_RF_R_500nm,
#'   SecD_RF_R_510nm, SecD_RF_R_520nm, SecD_RF_R_530nm, SecD_RF_R_540nm, SecD_RF_R_550nm, SecD_RF_R_560nm, SecD_RF_R_570nm, SecD_RF_R_580nm, SecD_RF_R_590nm, SecD_RF_R_600nm,
#'   SecD_RF_R_610nm, SecD_RF_R_620nm, SecD_RF_R_630nm, SecD_RF_R_640nm, SecD_RF_R_650nm, SecD_RF_R_660nm, SecD_RF_R_670nm, SecD_RF_R_680nm, SecD_RF_R_690nm, SecD_RF_R_700nm"}{visible spectra value}
#'   \item{TOC_SD, TN_SD, ratio_TOC_TN_uncertainty, d13C_SD, d15N_SD, Al_RMSE, Ca_RMSE, Co_RMSE, Cr_RMSE, Cu_RMSE, Fe_RMSE, K_RMSE, Mg_RMSE, Mn_RMSE, Ni_RMSE, Pb_RMSE,
#'   Rb_RMSE, Si_RMSE, Sr_RMSE, Ti_RMSE, Zn_RMSE, Zr_RMSE, L_star_D65_SD, a_star_D65_SD, b_star_D65_SD, C_star_D65_SD, h_D65_SD,A1_uncertainty, A2_uncertainty, A3_uncertainty,
#'   Gt_uncertainty, Q7.4_uncertainty, Goethite_445nm_FstD_uncertainty, Goethite_525nm_FstD_uncertainty}{property measurement error or uncertainty for each sample}
#'   \item{R_400nm_SD, R_410nm_SD, R_420nm_SD, R_430nm_SD, R_440nm_SD, R_450nm_SD, R_460nm_SD, R_470nm_SD, R_480nm_SD, R_490nm_SD, R_500nm_SD, R_510nm_SD, R_520nm_SD, R_530nm_SD,
#'   R_540nm_SD, R_550nm_SD, R_560nm_SD, R_570nm_SD, R_580nm_SD, R_590nm_SD, R_600nm_SD, R_610nm_SD, R_620nm_SD, R_630nm_SD, R_640nm_SD, R_650nm_SD, R_660nm_SD, R_670nm_SD,
#'   R_680nm_SD, R_690nm_SD, R_700nm_SD, FstD_R_400nm_SD, FstD_R_410nm_SD, FstD_R_420nm_SD, FstD_R_430nm_SD, FstD_R_440nm_SD, FstD_R_450nm_SD, FstD_R_460nm_SD, FstD_R_470nm_SD,
#'   FstD_R_480nm_SD, FstD_R_490nm_SD, FstD_R_500nm_SD, FstD_R_510nm_SD, FstD_R_520nm_SD, FstD_R_530nm_SD, FstD_R_540nm_SD, FstD_R_550nm_SD, FstD_R_560nm_SD, FstD_R_570nm_SD,
#'   FstD_R_580nm_SD, FstD_R_590nm_SD, FstD_R_600nm_SD, FstD_R_610nm_SD, FstD_R_620nm_SD, FstD_R_630nm_SD, FstD_R_640nm_SD, FstD_R_650nm_SD, FstD_R_660nm_SD, FstD_R_670nm_SD,
#'   FstD_R_680nm_SD, FstD_R_690nm_SD, FstD_R_700nm_SD, SecD_R_400nm_SD, SecD_R_410nm_SD, SecD_R_420nm_SD, SecD_R_430nm_SD, SecD_R_440nm_SD, SecD_R_450nm_SD, SecD_R_460nm_SD,
#'   SecD_R_470nm_SD, SecD_R_480nm_SD, SecD_R_490nm_SD, SecD_R_500nm_SD, SecD_R_510nm_SD, SecD_R_520nm_SD, SecD_R_530nm_SD, SecD_R_540nm_SD, SecD_R_550nm_SD, SecD_R_560nm_SD,
#'   SecD_R_570nm_SD, SecD_R_580nm_SD, SecD_R_590nm_SD, SecD_R_600nm_SD, SecD_R_610nm_SD, SecD_R_620nm_SD, SecD_R_630nm_SD, SecD_R_640nm_SD, SecD_R_650nm_SD, SecD_R_660nm_SD,
#'   SecD_R_670nm_SD, SecD_R_680nm_SD, SecD_R_690nm_SD, SecD_R_700nm_SD, RF_R_400nm_SD, RF_R_410nm_SD, RF_R_420nm_SD, RF_R_430nm_SD, RF_R_440nm_SD, RF_R_450nm_SD, RF_R_460nm_SD,
#'   RF_R_470nm_SD, RF_R_480nm_SD, RF_R_490nm_SD, RF_R_500nm_SD, RF_R_510nm_SD, RF_R_520nm_SD, RF_R_530nm_SD, RF_R_540nm_SD, RF_R_550nm_SD, RF_R_560nm_SD, RF_R_570nm_SD,
#'   RF_R_580nm_SD, RF_R_590nm_SD, RF_R_600nm_SD, RF_R_610nm_SD, RF_R_620nm_SD, RF_R_630nm_SD, RF_R_640nm_SD, RF_R_650nm_SD, RF_R_660nm_SD, RF_R_670nm_SD, RF_R_680nm_SD,
#'   RF_R_690nm_SD, RF_R_700nm_SD, SecD_RF_R_400nm_SD, SecD_RF_R_410nm_SD, SecD_RF_R_420nm_SD, SecD_RF_R_430nm_SD, SecD_RF_R_440nm_SD, SecD_RF_R_450nm_SD, SecD_RF_R_460nm_SD,
#'   SecD_RF_R_470nm_SD, SecD_RF_R_480nm_SD, SecD_RF_R_490nm_SD, SecD_RF_R_500nm_SD, SecD_RF_R_510nm_SD, SecD_RF_R_520nm_SD, SecD_RF_R_530nm_SD, SecD_RF_R_540nm_SD, SecD_RF_R_550nm_SD,
#'   SecD_RF_R_560nm_SD, SecD_RF_R_570nm_SD, SecD_RF_R_580nm_SD, SecD_RF_R_590nm_SD, SecD_RF_R_600nm_SD, SecD_RF_R_610nm_SD, SecD_RF_R_620nm_SD, SecD_RF_R_630nm_SD, SecD_RF_R_640nm_SD,
#'   SecD_RF_R_650nm_SD, SecD_RF_R_660nm_SD, SecD_RF_R_670nm_SD, SecD_RF_R_680nm_SD, SecD_RF_R_690nm_SD, SecD_RF_R_700nm_SD}{visible spectra measurement error for each sample}
#' }
#'
#' @references Chalaux-Clergue, Thomas, Evrard, Olivier, Durand, Roxanne, Caumon, Alison, Hayashi, Seiji, Tsuji, Hideki, Huon, Sylvain, Vaury, Véronique, Wakiyama, Yoshifumi, Nakao, Atsushi, Laceby, J. Patrick, Lefèvre, Irène, & Onda, Yuichi. (2022). Organic matter, geochemical and colorimetric properties of potential source material, target sediment and laboratory mixtures for conducting sediment fingerprinting approaches in the Mano Dam Reservoir (Hayama Lake) catchment, Fukushima Prefecture, Japan. (Version 1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.7081094

#"Hayama"

