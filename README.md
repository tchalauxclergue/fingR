
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fingR <a href="https://doi.org/10.5281/zenodo.8293595"><img src="man/figures/fingR_logo_ver2_small_300dpi.png" align="right" height="138" /></a>

<!-- badges: start -->

![GitHub
version](https://img.shields.io/github/r-package/v/tchalauxclergue/fingR?logo=github)
![GitHub Release
Date](https://img.shields.io/github/release-date/tchalauxclergue/fingR?color=blue)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1402028.svg)](https://doi.org/10.5281/zenodo.10044404)
[![GitHub Downloads (all assets, all
releases)](https://img.shields.io/github/downloads/tchalauxclergue/fingR/total?style=flat)
![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
<!-- badges: end -->

## Overview

`fingR` is a comprehensive package designed to support Sediment Source
Fingerprinting studies. It provides essentials tools including: dataset
characterisation, tracer selection from analysed properties through the
Three-step method, model source contributions modelling with the
Bayesian Mixing Model (BMM), and assessment of modelling predictions
prediction though the use of virtual mixtures, supporting BMM and
[MixSIAR](http://brianstock.github.io/MixSIAR/index.html) models.

The `fingR` package is available in this
[Github](https://github.com/tchalauxclergue/fingR) repository and
archived on [Zenodo](https://zenodo.org/records/10796375).

### Table of content

<!-- toc -->

- [Installation](#installation)
- [Usage](#usage)
  - [Data preparation](#data-preparation)
  - [Tracer selection](#tracer-selection)
    - [1. Assessment of conservative
      behaviour](#1-assessment-of-conservative-behaviour)
    - [2. Discriminant power](#2-discriminant-power)
    - [Selected tracers](#selected-tracers)
    - [3. Discriminant Function Analysis (DFA) stepwise
      selection](#3-discriminant-function-analysis-dfa-stepwise-selection)
  - [Source contribution modelling](#source-contribution-modelling)
    - [Virtual mixtures](#virtual-mixtures)
    - [Un-mixing models](#un-mixing-models)
      - [Bayesian Mean Model (BMM)](#bayesian-mean-model-bmm)
        - [Run BMM model with or without isotopic
          ratio](#run-bmm-model-with-or-without_isotopic-ratio)
        - [Modelling accuarcy
          statistics](#modelling-accuracy-statistics)
      - [MixSIAR](#mixsiar)
        - [Generate data for MixSIAR](#generate-data-for-mixsiar)
        - [Load mixture, source and discrimination
          data](#load-mixture-source-and-discrimination-data)
        - [Write JAGS model file](#write-jags-model-file)
        - [Run MixSIAR model](#run-mixsiar-model)
        - [Modelling accuracy
          statistics](#modelling-accuracy-statistics-1)
- [Future updates](#future-updates)
- [Getting help](#getting-help)
- [Citation](#citation)
- [References](#references)

<!-- tocstop -->

## Installation

``` r
#install.packages(devtools)
library(devtools)

# Install the lastest version from GitHub
devtools::install_github("https://github.com/tchalauxclergue/fingR/releases/tag/2.1.1", ref = "master", force = T)

# Alternatively, from the downloaded .tar.gz file
devtools::install_local("path_to_file/fingR_2.1.1.tar.gz", repos = NULL) # 'path_to_file' should be modified accordingly to your working environment
```

## Usage

### Data preparation

To illustrate the usage of the package, we are using the database of the
sediment core sampled in the Mano Dam reservoir (Fukushima, Japan) and
associated soil samples. The **38** sediment core layer are used as
target, and **68** soil samples as potential sources. The potential
source include three classes: undecontaminated cropland (n = **24**),
remediated cropland (n = **22**), forest (n = **24**), and subsoil
(mainly granite saprolite; n = **24**).

All samples were sieved to 63 microns and analysed for organic matter,
elemental geochemistry and diffuse reflectance spectrocolourimetry for
sediment source fingerprinting.

The dataset, along with detailed measurement protocols, is available for
download on Zenodo at [Chalaux-Clergue et al., 2024 (Version
2)](https://zenodo.org/doi/10.5281/zenodo.7081093).

``` r
library(fingR)

# Get the dir to data and metadata files within the R package
data.dr <- system.file("extdata", "TCC_MDD_20210608_data_ChalauxClergue_et_al_v240319.csv", package = "fingR")
metadata.dr <- system.file("extdata", "TCC_MDD_20210608_metadata_ChalauxClergue_et_al_v240319.csv", package = "fingR")

# Load the csv files of data and metadata - replace the dir with your file direction
db.data <- read.csv(data.dr, sep = ";", fileEncoding = "latin1", na = "")
db.metadata <- read.csv(metadata.dr, sep = ";", fileEncoding = "latin1", na = "")
```

Verify the different samples classes

``` r
table(db.metadata$Class_decontamination)
#> 
#>           Forest       Remediated          Subsoil           Target 
#>               24               10               10               38 
#> Undecontaminated 
#>               24
```

We join the metadata (general information) and the data (analyses) so
that all the information is on a single dataframe. Both dataframes are
joined by common variables, here IGSN and Sample_name. In addition, only
the analyses performed on the sample fraction below 63 microns are kept.

``` r
library(dplyr)

# Create a single dataframe with metadata and data information
database <- dplyr::left_join(db.metadata, db.data, by = join_by(IGSN, Sample_name)) %>% # Joining metadata and data data frame
  dplyr::filter(Sample_size == "< 63 µm") %>% # select sample fraction on which analyses were performed
  dplyr::filter(Class_decontamination != "Remediated") # to simplify the example remediated cropland are removed
```

``` r
table(database$Class_decontamination)
#> 
#>           Forest          Subsoil           Target Undecontaminated 
#>               24               10               38               24
```

Among the analysed properties, 31 properties from organic matter and
elemental geochemistry analyses were selected as potential tracers.
Together with the properties, their measurement uncertainties are
selected.

``` r
# colnames(database)

# Select the names/colnames of the properties
prop.values <- database %>% dplyr::select(TOC_PrC, TN_PrC,# organic matter properties
                                          EDXRF_Al_mg.kg.1:EDXRF_Zr_mg.kg.1) %>% names # elemental geochemistry


# Select the names/colnames of the property measurement uncertainties/errors
prop.uncertainties <- database %>% dplyr::select(TOC_SD, TN_SD, # organic matter
                                                 EDXRF_Al_RMSE:EDXRF_Zr_RMSE) %>% names # elemental geochemistry

names(prop.uncertainties) <- prop.values # Add property names to property uncertainty for easier selection
```

``` r
prop.values
#>  [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1" "EDXRF_Ca_mg.kg.1"
#>  [5] "EDXRF_Co_mg.kg.1" "EDXRF_Cr_mg.kg.1" "EDXRF_Cu_mg.kg.1" "EDXRF_Fe_mg.kg.1"
#>  [9] "EDXRF_K_mg.kg.1"  "EDXRF_Mg_mg.kg.1" "EDXRF_Mn_mg.kg.1" "EDXRF_Ni_mg.kg.1"
#> [13] "EDXRF_Pb_mg.kg.1" "EDXRF_Rb_mg.kg.1" "EDXRF_Si_mg.kg.1" "EDXRF_Sr_mg.kg.1"
#> [17] "EDXRF_Ti_mg.kg.1" "EDXRF_Zn_mg.kg.1" "EDXRF_Zr_mg.kg.1"
```

``` r

unname(prop.uncertainties)
#>  [1] "TOC_SD"        "TN_SD"         "EDXRF_Al_RMSE" "EDXRF_Ca_RMSE"
#>  [5] "EDXRF_Co_RMSE" "EDXRF_Cr_RMSE" "EDXRF_Cu_RMSE" "EDXRF_Fe_RMSE"
#>  [9] "EDXRF_K_RMSE"  "EDXRF_Mg_RMSE" "EDXRF_Mn_RMSE" "EDXRF_Ni_RMSE"
#> [13] "EDXRF_Pb_RMSE" "EDXRF_Rb_RMSE" "EDXRF_Si_RMSE" "EDXRF_Sr_RMSE"
#> [17] "EDXRF_Ti_RMSE" "EDXRF_Zn_RMSE" "EDXRF_Zr_RMSE"
```

First, we use `data.watcher` to check that the selected properties meet
the quality criteria, particularly in terms of their measurement
uncertainty. Several criteria are evaluated (e.g. presence of some
negative values or high uncertainty) and presented as indicators to
consider the use of a property.

``` r
library(fingR)

fingR::data.watcher(data = database, properties = prop.values, prop.uncer = prop.uncertainties)
#> 
#> Following column(s) contain(s) some negative values: EDXRF_Cr_mg.kg.1.
#> Following column(s) have a measurement uncertainty that makes some values to be virtually impossible: EDXRF_Co_mg.kg.1, EDXRF_Cr_mg.kg.1, EDXRF_Cu_mg.kg.1, EDXRF_Ni_mg.kg.1.
#> Following column(s) have a relative measurement uncertainty above 5% (up to - number): EDXRF_Co_mg.kg.1 (max:753% - n:26), EDXRF_Cr_mg.kg.1 (max:211% - n:38), EDXRF_Ni_mg.kg.1 (max:105% - n:96), EDXRF_Cu_mg.kg.1 (max:103% - n:52), EDXRF_Rb_mg.kg.1 (max:89% - n:93), TN_PrC (max:45% - n:91), EDXRF_Pb_mg.kg.1 (max:38% - n:91), EDXRF_Zn_mg.kg.1 (max:34% - n:96), EDXRF_Sr_mg.kg.1 (max:15% - n:46), TOC_PrC (max:14% - n:95), EDXRF_Zr_mg.kg.1 (max:7% - n:2).
```

According to `data.watcher` results: Co, Cr, Cu, Ni, and Rb have too
high measurement uncertainty and in addition Cr has some negative values
among the samples. These properties will be removed from following
study.

``` r
# Remove Co, Cr, Cu, Ni and Rb from the vector of properties
prop.values <- prop.values[!prop.values %in% c("EDXRF_Co_mg.kg.1", "EDXRF_Cr_mg.kg.1", "EDXRF_Cu_mg.kg.1", "EDXRF_Ni_mg.kg.1", "EDXRF_Rb_mg.kg.1")]

# Keep uncertainties associated to the new vector of properties
prop.uncertainties <- prop.uncertainties[prop.values]
```

``` r
prop.values
#>  [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1" "EDXRF_Ca_mg.kg.1"
#>  [5] "EDXRF_Fe_mg.kg.1" "EDXRF_K_mg.kg.1"  "EDXRF_Mg_mg.kg.1" "EDXRF_Mn_mg.kg.1"
#>  [9] "EDXRF_Pb_mg.kg.1" "EDXRF_Si_mg.kg.1" "EDXRF_Sr_mg.kg.1" "EDXRF_Ti_mg.kg.1"
#> [13] "EDXRF_Zn_mg.kg.1" "EDXRF_Zr_mg.kg.1"
```

### Tracer selection

### 1. Assessment of conservative behaviour

In the three-step method, the conservative behaviour is assessed by
range tests (RT), also known as bracket tests. To be considered to have
a conservative behaviour, all target samples values should lye within
the range of the potential source classes. The range of the potential
source classes is defined as the highest and lowest source class value
of a certain criterion.

Various criteria for range tests are documented in the literature,
including minimum-maximum (**MM**), minimum-maximum plus/minus 10%
(**MMe**) -to account for measurement error- , boxplot **whiskers** -as
threshold to identify extreme values-, boxplot **hinge** -50% of the
population-, **mean**, mean plus/minus one standard deviation
(**mean.sd**) and median. The **mean** and **mean.sd** criteria are
performed on log-transformed values, assuming a Normal distribution of
the samples.

By default, the function applies all these criteria, though their
effectiveness in identifying conservative characteristics may vary.
Among these, the **mean.sd** criterion is mathematically the most
robust.

The `range.test` function returns a list containing two data frames -
*results.df*: A summary overview of the range test results. -
*results.RT*: Detailed results for each target sample’s range test for
each property. The result of the range test in detailled as: `TRUE` for
samples within the range, `low` for sample values lower than the range,
and `high` for sample values higher than the range.

``` r
rt.results <- fingR::range.tests(data = database,                 # Dataset containing source and mixture information
                                 class = "Class_decontamination", # Column containing the classification or grouping of sources and mixtures
                                 mixture = "Target",              # Identifier for mixtures within the class variable
                                 properties = prop.values,        # Properties to be tested for conservativeness
                                 sample.id = "Sample_name",       # Identifier for individual samples
                                 criteria = c("mean.sd")          # Criteria for conducting range tests (options: "MM", "MMe", "whiskers", "hinge", "mean", "mean.sd", "median", or "all")
                                 # MM.error = c(0.1),             # Optional: Set the minimum-maximum plus/minus error as 10%
                                 # save.dir = dir.example,        # Optional: Directory path for saving the results
                                 # note = "example"               # Optional: Additional note to append to the file name
                                 )
```

``` r
rt.results$results.RT$EDXRF_Pb_mg.kg.1[1:5,]
#>         Sample_name         Property n_source RT_mean.sd
#> 1 ManoDd_2106_00-01 EDXRF_Pb_mg.kg.1       58       TRUE
#> 2 ManoDd_2106_01-02 EDXRF_Pb_mg.kg.1       58       TRUE
#> 3 ManoDd_2106_02-03 EDXRF_Pb_mg.kg.1       58       high
#> 4 ManoDd_2106_03-04 EDXRF_Pb_mg.kg.1       58       high
#> 5 ManoDd_2106_04-05 EDXRF_Pb_mg.kg.1       58       TRUE
```

``` r
rt.results$results.df[1:5]
#>            Property n_source n_mixture NAs RT_mean.sd_single
#> 1           TOC_PrC       58        38   0              TRUE
#> 2            TN_PrC       58        38   0              TRUE
#> 3  EDXRF_Al_mg.kg.1       58        38   0              TRUE
#> 4  EDXRF_Ca_mg.kg.1       58        38   0             FALSE
#> 5  EDXRF_Fe_mg.kg.1       58        38   0             FALSE
#> 6   EDXRF_K_mg.kg.1       58        38   0             FALSE
#> 7  EDXRF_Mg_mg.kg.1       58        38   0             FALSE
#> 8  EDXRF_Mn_mg.kg.1       58        38   0             FALSE
#> 9  EDXRF_Pb_mg.kg.1       58        38   0             FALSE
#> 10 EDXRF_Si_mg.kg.1       58        38   0             FALSE
#> 11 EDXRF_Sr_mg.kg.1       58        38   0             FALSE
#> 12 EDXRF_Ti_mg.kg.1       58        38   0              TRUE
#> 13 EDXRF_Zn_mg.kg.1       58        38   0             FALSE
#> 14 EDXRF_Zr_mg.kg.1       58        38   0             FALSE
```

The `is.conservative` function returns a list of vector of conservative
properties based on the results of range tests. If multiple criteria are
used, a vector is generated for each criterion.

``` r
prop.cons <- fingR::is.conservative(data = rt.results$results.df,  # Data frame containing the results of range tests, typically generated by fingR::range.tests
                                    # property = "Property",       # Optional: Column containing the names of properties being tested for conservativeness
                                    # test.format = "RT",          # Optional: Indicates the common pattern in column test names (default: "RT")
                                    # position = 2,                # Optional: Position of the test name in the column name (default: 2)
                                    # separator = "_",             # Optional: Character used to split test names in the column (default: "_")
                                    # note = "example"             # Optional: Additional note to append to the file name
                                    )
```

``` r
prop.cons
#> $mean.sd
#> [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1" "EDXRF_Ti_mg.kg.1"
```

### 2. Discriminant power

Inthe three-step method, the capacity of a property to discriminate
among source groups is commonly assessed using a Kruskal-Wallis H-test.
The *discriminant.test* function arguments are very similar to
*range.tests*. As an alternative Kolmogov-Smirnov two-samples tests can
be used. It provides more detailled results as source groups are
compared to each other.

``` r
KS.results <- fingR::discriminant.test(data = database,                 # Dataset containing source and mixture information
                                       class = "Class_decontamination", # Column containing the classification or grouping of sources and mixtures
                                       mixture = "Target",              # Identifier for mixtures within the class variable
                                       test = "KS",                     # Type of test performed, Kruskal-Wallis (KW) or Kolmogorov-smirnov (KS)
                                       properties = prop.values,        # Properties to be tested for conservativeness
                                       p.level = .01,                   # Optional: p-value significance level (default = 0.05)
                                       # save.discrim.tests = T,        # Optional: If two-samples tests should be saved
                                       # save.dir = dir.example,        # Optional: Directory path for saving the results
                                       # note = "example"               # Optional: Additional note to append to the file name
                                       )
```

``` r
KS.results[1:5,]
#>           Property n.diff.groups Kolmogorov.Smirnov_discriminant
#> 1          TOC_PrC             3                            TRUE
#> 2           TN_PrC             3                            TRUE
#> 3 EDXRF_Al_mg.kg.1             3                            TRUE
#> 4 EDXRF_Ca_mg.kg.1             1                            TRUE
#> 5 EDXRF_Fe_mg.kg.1             0                           FALSE
```

Properties that get a Kruskal-Wallis p-value bellow 0.05 (**p.value =
0.05**), are selected as discriminant properties. The function
*is.discriminant* list them. The function automatically recognise
data.frame produced by *discriminant.test* but it is possible to set it
for other data.frame format.

``` r
prop.discrim <- fingR::is.discriminant(KS.results,                                # data.frame from discriminant.test or any df with the same organisation.
                                       # property = "Property",                   # Optional: Column containing the names of properties being tested for conservativeness
                                       # test.format = "Kruskal.Wallis_p.value",  # Optional: Indicates the common pattern in column test names (default: "RT")
                                       # position = 1,                            # Optional: Position of the test name in the column name (default: 1)
                                       # separator = "_",                         # Optional: Character used to split test names in the column (default: "_")
                                       # p.level = 0.05,                          # Optional: p-value significance level (default = 0.05)
                                       # note = "example"                         # Optional: Additional note to append to the file name
                                       )
```

``` r
prop.discrim
#> $Kolmogorov.Smirnov
#>  [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1" "EDXRF_Ca_mg.kg.1"
#>  [5] "EDXRF_K_mg.kg.1"  "EDXRF_Pb_mg.kg.1" "EDXRF_Si_mg.kg.1" "EDXRF_Sr_mg.kg.1"
#>  [9] "EDXRF_Zn_mg.kg.1" "EDXRF_Zr_mg.kg.1"
```

#### Selected tracers

Tracers are conservative and discriminant properties.

``` r
tracers <- fingR::selected.tracers(cons = prop.cons,        # character vector of conservative properties
                                   discrim = prop.discrim)  # character Vector of discriminant properties
```

``` r
tracers 
#> $mean.sd_Kolmogorov.Smirnov
#> [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1"
```

Tracer selection are labelled by `selected.tracers` accordingly to the
range test criteria (e.g. mean.sd, hinge…) and discriminant test
(i.e. Kruskal.Wallis or Kolmogorov.Smirnov). However, sometimes this
label is to long for file labelling therefore, you may replace it
accondingly.

``` r
names(tracers) <- "msd_KS" # replace tracers names with the new name
```

``` r
tracers
#> $msd_KS
#> [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1"
```

### 3. Discriminant Function Analysis (DFA) stepwise selection

The conventional three-step method apply a DFA forward stepwise
selection on the selected tracers. This DFA stepwise selection aims to
retain tracers that maximize source discrimination. However, this step
has faced criticism. Observing the results for a large selection of
tracers can be insightful. However, it is not useful for small selection
of tracers, as it is the case here.

``` r
tracers.SW <- fingR::stepwise.selection(data = database,                 # Dataset containing source and mixture information
                                        class = "Class_decontamination", # Column containing the classification or grouping of source and mixtures
                                        tracers = tracers$msd_KS,        # Character vector containing tracers to consider
                                        target = "Target"                # Identifier for target samples within the "class" column
                                        # save.dir = dir.example,        # Optional: Directory path for saving the results
                                        # note = "example"               # Optional: Additional note to append to the file name
                                        )
```

``` r
tracers.SW
#> [1] "EDXRF_Al_mg.kg.1" "TOC_PrC"          "TN_PrC"
```

The DFA stepwise selection did not removed any of the selected tracers.
If the DFA selects different tracers, examining the modelling results
for both sets can provide useful insights.

Both tracer selections could joint like following:

``` r
# Joining two tracers vector in a list
all.tracers <- list("msd_KS" = tracers$msd_KS, "msd_KS_DFA" = tracers.SW)
```

## Source contribution modelling

#### Virtual mixtures

To evaluate the accuracy of un-mixing models, virtual mixtures are used.
These virtual mixtures, serving as target samples with known
contributions, allow for the calculation of modelling accuracy metrics.
The `VM.contrib.generator` generate virtual mixture contributions from
the `min` to the `max` contribution set with a specified `step`.
Contribution could be set as percentage (`min = 0, max = 100`) or as a
ratios (`min = 0, max = 1`). Smaller `step` result in a higher number of
virtual mixtures, such as 231 virtual mixtures for a 5% step and 5151
virtual mixtures for a 1% step. Alternatively, virtual mixtures can be
generated within `VM.builder`.

``` r
# Generate virtual mixture source contributions
VM.contrib <- fingR::VM.contrib.generator(n.sources = 3,                                              # Number of source levels
                                          min = 0,                                                    # Minimum contribution (here percentage)
                                          max = 100,                                                  # Maximum contribution (here percentage)
                                          step = 5,                                                   # Step between two contribution levels (here percentage)
                                          sources.class = c("Forest", "Subsoil", "Undecontaminated"), # Optional: Classification of sources
                                          save.dir = dir.example,                                     # Optional: Directory path for saving the results
                                          # note = "example"                                          # Optional: Additional note to append to the file name
                                          # VM.name = "Sample_name",                                  # Optional: Name of the column containing virtual mixture labels
                                          # fileEncoding = "latin1",                                  # Optional: File encoding, important if special character are used in source levels
                                          # return = TRUE,                                            # Optional: Whether the function should return the result
                                          # save = TRUE                                               # Optional: Whether the function should save the result
                                          )
```

``` r
VM.contrib[1:5,]
#>   Sample_name Forest Subsoil Undecontaminated
#> 1      VM-001      0       0              100
#> 2      VM-002      0       5               95
#> 3      VM-003      0      10               90
#> 4      VM-004      0      15               85
#> 5      VM-005      0      20               80
```

Next, virtual mixture properties are calculated as simple proportional
mixture of source signature (i.e. mean values). This approach is a
simple mass balance approach. The `VM.builder` function saves and
returns a list containing three *data.frame* objects: one with the
`$property` values, the other with the `$uncertainty` values (with
corresponding labels when given in `$uncertainty` if not simply “\_SD”
is added at the end of the tracer label), and the last one `$full` where
property and uncertainty were join.

To run un-mixing models, source and target information should be within
the same data frame. Source informations are added at the end of all the
*data.frame* created.

``` r
VM <- VM.builder(data = database,                                          # Dataset containing source samples
                 material = "Material",                                    # Column indicating the difference between source and target
                 source.name = "Source",                                   # Identifier for source samples within the material column
                 class = "Class_decontamination",                          # Column containing the classification or grouping of sources and mixtures
                 tracers = tracers$msd_KS,                                 # Character vector containing tracers to consider
                 uncertainty = unname(prop.uncertainties[tracers$msd_KS]), # Character vector containing tracers uncertainty labels
                 contributions = VM.contrib,                               # Virtual mixture contributions
                 VM.name = "Sample_name",                                  # Column with virtual mixture labels in the 'contribution' (i.e. VM.contribution)
                 add.sources = TRUE,                                       # Add source information at the end of the VM data frames
                 save.dir = dir.example,                                   # Optional: Directory path for saving the results
                 # note = "example"                                        # Optional: Additional note to append to the file name
                 )
```

``` r
VM$full[1:5,]
#>   Sample_name Class_decontamination TOC_PrC TN_PrC EDXRF_Al_mg.kg.1 TOC_SD
#> 1      VM-001       Virtual Mixture    5.16   0.42         84858.53   4.75
#> 2      VM-002       Virtual Mixture    4.97   0.40         86004.71   4.75
#> 3      VM-003       Virtual Mixture    4.78   0.39         87150.90   4.75
#> 4      VM-004       Virtual Mixture    4.60   0.38         88297.08   4.75
#> 5      VM-005       Virtual Mixture    4.41   0.36         89443.26   4.75
#>   TN_SD EDXRF_Al_RMSE
#> 1  0.28      17840.72
#> 2  0.28      17840.72
#> 3  0.28      17840.72
#> 4  0.28      17840.72
#> 5  0.28      17840.72
```

Here an example of sets to generate virtual mixture with the
`VM.builder` function without previously running the
`VM.contrib.generator` function.

``` r
VM <- VM.builder(data = database,                                          # Dataset containing source samples
                 material = "Material",                                    # Column indicating the difference between source and target
                 source.name = "Source",                                   # Identifier for source samples within the material column
                 class = "Class_decontamination",                          # Column containing the classification or grouping of sources and mixtures
                 tracers = tracers$msd_KS,                                 # Character vector containing tracers to consider
                 uncertainty = unname(prop.uncertainties[tracers$msd_KS]), # Character vector containing tracers uncertainty labels
                 VM.range = c(0, 100),                                     # Minimum and maximum contribution (here percentage)
                 VM.step = 5,                                              # Step between two contribution levels (here percentage)
                 VM.name = "Sample_name",                                  # Column with virtual mixture labels in the 'contribution' (i.e. VM.contribution)
                 add.sources = TRUE,                                       # Add source information at the end of the VM data frames
                 save.dir = dir.example,                                   # Optional: Directory path for saving the results
                 # note = "example"                                        # Optional: Additional note to append to the file name
                 )
```

### Un-mixing models

Create a folder where all modelling results will be saved

``` r
# Create new folder to save tracer modelling results
dir.create(file.path(dir.example, "Modelling/"), showWarnings = FALSE)
dir.modelling <- paste0(dir.example, "Modelling/")
```

### Bayesian Mean Model (BMM)

Create a folder specific from BMM modelling results.

``` r
# Create new folder to save BMM modelling results
dir.create(file.path(dir.modelling, "BMM/"), showWarnings = FALSE)
dir.mod.BMM <- paste0(dir.modelling, "BMM/")
```

#### Run BMM model with or without isotopic ratio

Run BMM models for actual sediment samples (*mix*) and virtual mixtures
(*VM*). The BMM model performs a Bayesian un-mixing with a Monte-Carlo
chain, the prediction is corrected using the sum of squared relative
error of each tracer. Without isotopic ratio within the tracers, there
is no need to take any precautions when setting up the model.

``` r
# Run BMM model for sediment samples
BMM.mix <- fingR::run.BMM(data = database,                                           # Dataset containing source and target samples
                          class = "Class_decontamination",                           # Column containing the classification or grouping of sources and mixtures
                          mixture = "Target",                                        # Column name identifying the target samples
                          sample.id = "Sample_name",                                 # Column name for sample identifiers
                          tracers = tracers$msd_KS,                                  # Character vector containing tracers to consider
                          uncertainty = unname(prop.uncertainties[tracers$msd_KS]),  # Optional: Character vector containing uncertainty of the tracers
                          n.iter = 30,                                               # Number of iterations for the model (30 for test version - 2500 or 5000 iterations are recommended) 'prop.uncertainties'
                          save.dir = dir.mod.BMM,                                    # Optional: Directory path for saving the results - 'BMM_previsions.CSV'
                          #note = "example"                                          # Optional: Additional note to append to the file name
                          )
```

``` r
# Run BMM model for virtual mixtures
BMM.VM <- fingR::run.BMM(data = VM$full,                                            # Dataset containing source and target samples
                         class = "Class_decontamination",                           # Column containing the classification or grouping of sources and mixtures
                         mixture = "Virtual Mixture",                               # Column name identifying the target samples
                         sample.id = "Sample_name",                                 # Column name for sample identifiers
                         tracers = tracers$msd_KS,                                  # Character vector containing tracers to consider
                         uncertainty = unname(prop.uncertainties[tracers$msd_KS]),  # Optional: Character vector containing uncertainty of the tracers
                         n.iter = 30,                                               # Number of iterations for the model (30 for test version - 2500 or 5000 iterations are recommended)
                         save.dir = dir.mod.BMM,                                    # Optional: Directory path for saving the results - 'BMM_previsions_VM.CSV'
                         note = "VM"                                                # Optional: Additional note to append to the file name
                         )
```

When dealing with isotopic ratios, which are non-linear properties,
errors should be calculated considering relative property content (see
[Laceby et al. (2015)](https://doi.org/10.1002/hyp.10311) for further
details). For example, the delta 13C ratio indicates the isotopic ratio
of 12C to 13C in organic matter, the `run.BMM` function should be
configured in this way:

``` r
# Run BMM model for sediment samples
BMM.iso <- fingR::run.BMM(data = database,                                           # Dataset containing source and target samples
                             class = "Class_decontamination",                           # Column containing the classification or grouping of sources and mixtures
                             mixture = "Target",                                        # Column name identifying the target samples
                             sample.id = "Sample_name",                                 # Column name for sample identifiers
                             tracers = tracers$msd_KS,                                  # Character vector containing tracers to consider
                             uncertainty = unname(prop.uncertainties[tracers$msd_KS]),  # Optional: Character vector containing uncertainty of the tracers
                             isotope.ratio = c("d13C_PrM"),                             # Optional: Character vector containing isotopic ratios
                             isotope.prop = c("TOC_PrC"),                               # Optional: Character vector containing isotopic ratios respective properties
                             isotopes.unc = c("d13C_SD"),                               # Optional: Character vecotr containing uncertainty of the isotopic ratios
                             n.iter = 30,                                               # Number of iterations for the model (30 for test version - 2500 or 5000 iterations are recommended) 'prop.uncertainties'
                             save.dir = dir.mod.BMM,                                    # Optional: Directory path for saving the results - 'BMM_previsions.CSV'
                             #note = "example"                                          # Optional: Additional note to append to the file name
                             )
```

After running the models, we extract the prediction information from the
iteration previsions. The `BMM.summary` function provides a summary of
the predictions, including the mean, standard deviation, and various
quantiles (2.5, 5, 25, 50, 75, 95, 97.5%) for each mixture (sediment
sample or virtual mixture). From this summary, the `BMM.pred` function
extracts the ‘Median’ and/or ‘Mean’ for each mixture. Finally, the
`ensure.total` function ensures that the total predicted contribution
from all sources sums to 1 or 100%.

``` r
# For sediment samples
## Summarise BMM model previsions
BMM.summary.mix <- fingR::BMM.summary(pred = BMM.mix,            # Predicted contributions from BMM
                                      #sample.id = "mix.names",  # Column name for sample identifier
                                      #source = "source",        # Column name for source identifier
                                      #value = "value",          # Column name for prediction value identifier
                                      save.dir = dir.mod.BMM,    # Optional: Directory path for saving the results
                                      #note = "example"          # Optional: Additional note to append to the file name
                                      )

## Extracts the median value of the previsions
BMM.preds.mix <- fingR::BMM.pred(data = BMM.summary.mix,         # Summary statistics of the predicted contribution by BMM, data from fingR::BMM.summary.mix
                                 stats = "Median",               # The summary statistics for source contribution, Could be Mean or Median
                                 #sample.id = "mix.names",       # Column name for sample identifier
                                 #source = "source",             # Column name for prediction value identifier
                                 save.dir = dir.mod.BMM,         # Optional: Directory path for saving the results
                                 #note = "example"               # Optional: Additional note to append to the file name
                                 )

## Ensure that the total predicted contribution sums to 1 or 100%
BMM.preds.mixE <- fingR::ensure.total(data = BMM.preds.mix,      # Predicted source contribution for each sample, data from fingR::BMM.pre
                                      sample.name = "mix.names", # Column name for sample identifier
                                      path = dir.mod.BMM,        # Optional: Directory path for saving the results
                                      #note = "example"          # Optional: Additional note to append to the file name
                                      )
```

``` r
BMM.preds.mixE[1:5,]
#>           mix.names Median_Forest Median_Subsoil Median_Undecontaminated total
#> 1 ManoDd_2106_00-01         0.001          0.711                   0.288     1
#> 2 ManoDd_2106_01-02         0.001          0.939                   0.060     1
#> 3 ManoDd_2106_02-03         0.001          0.687                   0.312     1
#> 4 ManoDd_2106_03-04         0.097          0.902                   0.001     1
#> 5 ManoDd_2106_04-05         0.001          0.614                   0.385     1
```

Same code for virtual mixtures:

``` r
# For virtual mixtures
## Summarise BMM model previsions
BMM.summary.VM <- fingR::BMM.summary(pred = BMM.VM,              # Predicted contributions from BMM
                                     #sample.id = "mix.names",   # Column name for sample identifier
                                     #source = "source",         # Column name for source identifier
                                     #value = "value",           # Column name for prediction value identifier
                                     save.dir = dir.mod.BMM,     # Optional: Directory path for saving the results
                                     note = "VM"                 # Optional: Additional note to append to the file name
                                     )

## Extracts the median value of the previsions
BMM.preds.VM <- fingR::BMM.pred(data = BMM.summary.VM,           # Summary statistics of the predicted contribution by BMM, data from fingR::BMM.summary.mix
                                stats = "Median",                # The summary statistics for source contribution, Could be Mean or Median
                                #sample.id = "mix.names",        # Column name for sample identifier
                                #source = "source",              # Column name for prediction value identifier
                                save.dir = dir.mod.BMM,          # Optional: Directory path for saving the results
                                note = "VM"                      # Optional: Additional note to append to the file name
                                 )

## Ensure that the total predicted contribution sums to 1 or 100%
BMM.preds.VME <- fingR::ensure.total(data = BMM.preds.VM,        # Predicted source contribution for each sample, data from fingR::BMM.pre
                                      sample.name = "mix.names", # Column name for sample identifier
                                      path = dir.mod.BMM,        # Optional: Directory path for saving the results
                                      note = "VM"                # Optional: Additional note to append to the file name
                                      )
```

``` r
BMM.preds.VME[1:5,]
#>   mix.names Median_Forest Median_Subsoil Median_Undecontaminated total
#> 1    VM-001         0.120          0.879                   0.001     1
#> 2    VM-002         0.001          0.943                   0.056     1
#> 3    VM-003         0.026          0.973                   0.001     1
#> 4    VM-004         0.012          0.987                   0.001     1
#> 5    VM-005         0.001          0.998                   0.001     1
```

#### Modelling accuracy statistics

The modelling accuracy of BMM model is evaluate with the virtual
mixtures. These virtual mixtures, serving as target samples with known
contributions (*VM.contrib*), allow for the calculation of modelling
accuracy metrics based on their prediction.

The `eval.groups` function calculates several common modelling accuracy
metrics: ME, RMSE, squared Pearson’s correlation coefficient (r2), and
Nash-Sutcliffe Modelling Efficiency Coefficient (NSE).

``` r
BMM.stats <- fingR::eval.groups(df.obs = VM.contrib,                               # Theoretical contribution
                                df.pred = BMM.preds.VME %>% dplyr::select(-total), # Predicted contribution (remove the $total column from ensured data.frame)
                                by = c("Sample_name" = "mix.names"),               # Column where mixtures labels are specified (for `dplyr::left_join` function)
                                path = dir.mod.BMM,                                # Optional: Directory path for saving the results
                                #note = "example"                                  # Optional: Additional note to append to the file name
                                )
```

``` r
BMM.stats
#>     Type           Source    ME RMSE   r2   NSE
#> 1 Median           Forest -0.16 0.24 0.58  0.13
#> 2 Median          Subsoil  0.42 0.49 0.33 -2.77
#> 3 Median Undecontaminated -0.26 0.39 0.00 -1.34
```

The `CRPS` functions calculate the continuous ranking probability score
and returns a list contraining two *data.frame* objects; one with the
`$samples` CRPS values per source class group (saved as *CRPS.csv*), the
other is `$mean` with the mean of the CRPS per source class groups
(saved as *CRPS_mean.csv*).

``` r
# Calculate prediction CRPS values
BMM.CRPS <- fingR::CRPS(obs = VM.contrib,                                             # Observed contributions
                        prev = read.csv(paste0(dir.mod.BMM, "BMM_prevision_VM.csv")), # Predicted prevision from BMM saved by `rum.BMM()`
                        source.groups = c("Forest", "Subsoil", "Undecontaminated"),   # Source class groups
                        mean.cal = TRUE,                                              # Calculate mean CRPS per source class group
                        save.dir = dir.mod.BMM,                                       # Optional: Directory path for saving the results
                        #note = "example"                                             # Optional: Additional note to append to the file name
                        )
#> Le chargement a nécessité le package : scoringRules
```

``` r
BMM.CRPS$samples[1:6,]
#>   Sample_name Forest Subsoil Undecontaminated
#> 1      VM-001 0.0680  0.4323           0.6693
#> 2      VM-002 0.0546  0.3434           0.5180
#> 3      VM-003 0.0464  0.3368           0.5116
#> 4      VM-004 0.0663  0.3151           0.5692
#> 5      VM-005 0.0508  0.3873           0.6046
#> 6      VM-006 0.0613  0.2569           0.4796
```

``` r

BMM.CRPS$mean
#>             Source CRPS.mean
#> 1           Forest    0.1371
#> 2          Subsoil    0.1682
#> 3 Undecontaminated    0.1863
```

The `interval.width` functions calculate two prediction interval width:
The *W50* contains 50% of the prevision (Q75-Q25) and the *W95* contains
95% of the prevision (Q97.5-Q2.5). It returns a list contraining two
*data.frame* objects; one with the `$samples` prediction interval width
values per source class group (saved as *Interval_width.csv*), the other
is `$mean` with the mean of the prediction interval width per source
class groups (saved as *Interval_width_mean.csv*).

``` r
# Calculate prediction interval width (W95, W50)
BMM.predWidth <- fingR::interval.width(path.to.prev = paste0(dir.mod.BMM, "BMM_prevision_VM.csv"), # Path to prediction file
                                       mean.cal = TRUE,                                            # Calculate mean of interval width per source group
                                       save = TRUE,                                                # Save the results at the same location of the path.to.prev
                                       #note = "exemple"                                           # Optional: Additional note to append to the file name
                                       )
```

``` r
BMM.predWidth$samples[1:6,]
#>   mix.names           source   W50   W95
#> 1    VM-001           Forest 0.242 0.794
#> 2    VM-001          Subsoil 0.584 0.997
#> 3    VM-001 Undecontaminated 0.332 0.845
#> 4    VM-002           Forest 0.322 0.595
#> 5    VM-002          Subsoil 0.613 0.997
#> 6    VM-002 Undecontaminated 0.498 0.897
```

``` r

BMM.predWidth$mean
#> # A tibble: 3 × 3
#>   Source           W50.mean W95.mean
#>   <chr>               <dbl>    <dbl>
#> 1 Forest              0.352    0.873
#> 2 Subsoil             0.586    0.973
#> 3 Undecontaminated    0.41     0.927
```

The `ESP` function calculates the Encompassed Sample Prediction (ESP).
The ESP is a newly introduced statistics in [Chalaux-Clergue et al
(under review)]() and was created to assess the transferability of the
statistics calculated on virtual mixtures to actual sediment samples.
The ESP was calculated as the percentage of actual samples for which the
predicted contributions remained within the lowest and the highest
predicted contributions obtained for the virtual mixtures. When
expressed as a percentage, ESP ranges from 0 to 100%, the latter
providing an optimal value. Values close to 100% indicate a higher
transferability of modelling evaluation statistics calculated on virtual
mixture to actual sediment samples.

``` r
sources.lvl <- c("Forest", "Subsoil", "Undecontaminated")

# Calculate encompassed sample predictions (ESP)
BMM.ESP <- fingR::ESP(obs = BMM.preds.VM,                       # Virtual mixtures predicted contributions
                      pred = BMM.preds.mixE,                    # Actual sediment samples predicted contributions
                      sources = paste0("Median_", sources.lvl), # Sources labels in prediction objects
                      count = "Both"                            # Count 'Number' and 'Percentage'
                      )
```

``` r
BMM.ESP
#>                                   Source ESP.Number ESP.Percentage
#> Median_Forest                     Forest         37             97
#> Median_Subsoil                   Subsoil         38            100
#> Median_Undecontaminated Undecontaminated         18             47
```

Modelling accuracy statistics could be interpreted the following way:
“Higher values of W50 indicate a wider distribution, which is related to
a higher uncertainty. The sign of the ME indicates the direction of the
bias, i.e. an overestimation or underestimation (positive or negative
value, respectively). As ME is affected by cancellation, a ME of zero
can also reflect a balanced distribution of predictions around the 1 : 1
line. Although this is not a bias, it does not mean that the model
outputs are devoid of errors. The RMSE is a measure of the accuracy and
allows us to calculate prediction errors of different models for a
particular dataset. RMSE is always positive, and its ideal value is
zero, which indicates a perfect fit to the data. As RMSE depends on the
squared error, it is sensitive to outliers. The r2 describes how linear
the prediction is. The NSE indicates the magnitude of variance explained
by the model, i.e. how well the predictions match with the observations.
A negative RMSE indicates that the mean of the measured values provides
a better predictor than the model. The joint use of r2 and NSE allows
for a better appreciation of the distribution shape of predictions and
thus facilitates the understanding of the nature of model prediction
errors. The CRPS evaluates both the accuracy and sharpness
(i.e. precision) of a distribution of predicted continuous values from a
probabilistic model for each sample (Matheson and Winkler, 1976). The
CRPS is minimised when the observed value corresponds to a high
probability value in the distribution of model outputs.”
[(Chalaux-Clergue et al, 2024)](10.5194/soil-10-109-2024).

### MixSIAR

The `MixSIAR` is an R package designed to create and run Bayesian mixing
models. This package is widely used in the sediment source
fingerprinting community to predict source contribution. To explore more
about `MixSIAR`, including detailed tutorials, examples, and technical
documentation, please visit the official [MixSIAR
website](http://brianstock.github.io/MixSIAR/index.html). Additionally,
the source code and further resources can be found on the [MixSIAR
GitHub page](https://github.com/brianstock/MixSIAR).

According to [MixSIAR
guide](http://brianstock.github.io/MixSIAR/index.html#installation),
installation should follow these steps:

1.  Download and install/updata [R](https://cran.r-project.org/).
2.  Download and install [JAGS](http://mcmc-jags.sourceforge.net/).
3.  Open R and run:

``` r
install.packages("MixSIAR", dependencies=TRUE)
```

You can install the GitHub version

``` r
#install.packages(remotes)

remotes::install_github("brianstock/MixSIAR", dependencies=T)
```

Create a folder specific from BMM modelling results.

``` r
# Create new folder to save BMM modelling results
dir.create(file.path(dir.modelling, "MixSIAR/"), showWarnings = FALSE)
dir.mod.MixSIAR <- paste0(dir.modelling, "MixSIAR/")
```

#### Generate data for MixSIAR

To MixSIAR models require data in a specific format to load the
information of mixtures and sources samples. The `data.for.MixSIAR`
function generates *csv* files that conform to the format required by
MixSIAR loading functions. The function generates three files:
*MixSIAR_mix.csv* containing mixtures information, *MixSIAR_sources*
containing the mean and standard deviation (sd) of the source classes,
and *MixSIAR_discrimination* which is a matrix of zero as there is no
throphic information in sediment source fingerprinting studies.

Of note, if several selection of tracers were obtained from the tracer
selection different files should be created. Use the `note` argument to
differentiate them.

``` r
fingR::data.for.MixSIAR(data = database,                 # Dataset containing source samples
                        class = "Class_decontamination", # Column containing the classification or grouping of sources and mixtures
                        target = "Target",               # Identifier for mixture samples within the class column
                        tracers = tracers$msd_KS,        # Character vector containing tracers to consider
                        sample.name = "Sample_name",     # Column containing sample names in data
                        save.dir = dir.mod.MixSIAR,      # Directory path for saving the files
                        # note = "exemple",              # Optional: Additional note to append to the file name
                        # fileEncoding = "latin1",       # Optional: File encoding, important if special character are used in source levels
                        # show.data = FALSE,             # Optional: Return generated files in R
                        )
```

``` r
fingR::data.for.MixSIAR(data = VM$full,                  # Dataset containing source samples
                        class = "Class_decontamination", # Column containing the classification or grouping of sources and mixtures
                        target = "Virtual Mixture",      # Identifier for mixture samples within the class column
                        tracers = tracers$msd_KS,        # Character vector containing tracers to consider
                        sample.name = "Sample_name",     # Column containing sample names in data
                        save.dir = dir.mod.MixSIAR,      # Directory path for saving the files
                        note = "VM",                     # Optional: Additional note to append to the file name
                        # fileEncoding = "latin1",       # Optional: File encoding, important if special character are used in source levels
                        # show.data = FALSE,             # Optional: Return generated files in R
                        )
```

#### Load mixture, source and discrimination data

Load mixture, source and discrimination data for sediment samples.

``` r
library(MixSIAR)

# Load sediment samples data
MSIAR.mix  <- MixSIAR::load_mix_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_mix.csv"),               # File containing real samples data
                                     iso_names = tracers$msd_KS,                                          # Names of tracers
                                     factors = c("Sample_name"),                                          # Columns used to differentiate samples
                                     fac_random = FALSE,                                                  # Indicates if the factor is a random effect
                                     cont_effects = NULL                                                  # Continuous effect column not specified
                                     )

# Load source data
MSIAR.source <- MixSIAR::load_source_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_sources.csv"),      # File containing source data
                                          source_factors = NULL,                                          # No source factors specified
                                          conc_dep = FALSE,                                               # Concentration dependence not considered
                                          data_type = "means",                                            # Type of data provided is means
                                          mix = MSIAR.mix                                                 # Actual samples mixtures
                                          )

# Load discrimination data
MSIAR.discr <- MixSIAR::load_discr_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_discrimination.csv"), # File containing discrimination data
                                        mix = MSIAR.mix)                                                  # Actual samples mixtures
```

Load mixture, source and discrimination data for virtual mixtures.

``` r
library(MixSIAR)


# Load virtual mixtures data
MSIAR.VM <- MixSIAR::load_mix_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_mix_VM.csv"),                    # File containing virtual mixtures data
                                          iso_names = tracers$msd_KS,                                           # Names of tracers
                                          factors = c("Sample_name"),                                           # Columns used to differentiate samples
                                          fac_random = FALSE,                                                   # Indicates if the factor is a random effect
                                          cont_effects = NULL)                                                  # Continuous effect column not specified


# Load source data
MSIAR.source.VM <- MixSIAR::load_source_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_sources_VM.csv"),      # File containing source data
                                          source_factors = NULL,                                                # No source factors specified
                                          conc_dep = FALSE,                                                     # Concentration dependence not considered
                                          data_type = "means",                                                  # Type of data provided is means
                                          mix = MSIAR.VM                                                        # Actual samples mixtures
                                          )

# Load discrimination data
MSIAR.discr.VM <- MixSIAR::load_discr_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_discrimination_VM.csv"), # File containing discrimination data
                                        mix = MSIAR.VM)                                                         # Actual samples mixtures
```

#### Write JAGS model file

Write the JAGS file, which define model structure. The model will be
saved as `model_file` (“MixSIAR_model.txt” is default).

``` r
# Write JAGS model file for actual samples
MixSIAR::write_JAGS_model(filename = paste0(dir.mod.MixSIAR, "MixSIAR_model.txt"),    # File path and name to write the JAGS model
                          resid_err = FALSE,                                          # Whether to include residual error in the model
                          process_err = TRUE,                                         # Whether to include process error in the model
                          mix = MSIAR.mix,                                            # Actual samples mixtures dataset
                          source = MSIAR.source                                       # Source dataset
                          )
```

``` r
# Write JAGS model file for virtual mixtures
MixSIAR::write_JAGS_model(filename = paste0(dir.mod.MixSIAR, "MixSIAR_model_VM.txt"), # File path and name to write the JAGS model
                          resid_err = FALSE,                                          # Whether to include residual error in the model
                          process_err = TRUE,                                         # Whether to include process error in the model
                          mix = MSIAR.VM,                                             # Virtual mixtures dataset
                          source = MSIAR.source.VM                                    # Source dataset loaded with virtual mixture mix
                          )
```

#### Run MixSIAR model

When running MixSIAR model you should choose one of the [MCMC run
option](http://brianstock.github.io/MixSIAR/articles/wolves_ex.html#run-model).
Here `run` is set to “test” as it is an example.

``` r
# note if "Error: .onload ... 'rgags' -> it's because R version is too old need at least R.2.2

# Run MixSIAR model for sediment samples
jags.mix <- MixSIAR::run_model(run = "test",                                                 # Type of run (e.g. "test", "long"...)
                               mix = MSIAR.mix,                                              # Sediment samples dataset
                               source = MSIAR.source,                                        # Source dataset
                               discr = MSIAR.discr,                                          # Discrimination dataset
                               model_filename = paste0(dir.mod.MixSIAR, "MixSIAR_model.txt") # File path to the JAGS model
                               )
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 114
#>    Unobserved stochastic nodes: 93
#>    Total graph size: 2838
#> 
#> Initializing model
```

``` r
# note if "Error: .onload ... 'rgags' -> it's because R version is too old need at least R.2.2

# Run MixSIAR model for Virtual mixtures
jags.VM <- MixSIAR::run_model(run = "test",                                                    # Type of run (e.g. "test", "long", "very long"...)
                              mix = MSIAR.VM,                                                  # Virtual mixtures dataset
                              source = MSIAR.source.VM,                                        # Source dataset loaded with virtual mixture mix
                              discr = MSIAR.discr.VM,                                          # Discrimination dataset
                              model_filename = paste0(dir.mod.MixSIAR, "MixSIAR_model_VM.txt") # File path to the JAGS model
                              )
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 693
#>    Unobserved stochastic nodes: 479
#>    Total graph size: 16541
#> 
#> Initializing model
```

After running the models, we extract the prediction information from the
MixSIAR model predictions. The `MixSIAR.summary` function provides a
summary of the predictions, including the mean, standard deviation, and
various quantiles (2.5, 5, 25, 50, 75, 95, 97.5%) for each mixture
(sediment sample or virtual mixture). From this summary, the
`MixSIAR.pred` function extracts the ‘Median’ and/or ‘Mean’ for each
mixture. Finally, the `ensure.total` function ensures that the total
predicted contribution from all sources sums to 1 or 100%.

``` r
## Summarise MixSIAR model previsions
MixSIAR.summary.mix <- fingR::JAGS.summary(jags.1 = jags.mix,                        # Data from the MixSIAR model `MixSIAR::run_model()`
                                          mix = MSIAR.mix,                           # Sediment dataset 
                                          sources = MSIAR.source,                    # Source dataset
                                          path = dir.mod.MixSIAR,                    # Directory path for saving the files
                                          #note = "example",                         # Optional: Additional note to append to the file name
                                          save_pred = TRUE                           # Optional: Save the MixSIAR modelling predictions (heavy files)
                                          )

## Extracts the median value of the previsions
MixSIAR.preds.mix <- fingR::JAGS.pred(path = paste0(dir.mod.MixSIAR, "contrib.csv"), # location of files generated by `JAGS.summary`
                                     stats = "Median",                               # Summary statistics to calculate (Median or Mean)
                                     save = TRUE,                                    # If the result should be saved
                                     #note = "example"                               # Optional: Additional note to append to the file name 
                                     )

## Ensure that the total predicted contribution sums to 1 or 100%
MixSIAR.preds.mixE <- fingR::ensure.total(data = MixSIAR.preds.mix,                  # Predicted source contribution for each sample, data from fingR::BMM.pre
                                          sample.name = "sample",                    # Column name for sample identifier
                                          path = dir.mod.MixSIAR,                    # Optional: Directory path for saving the results
                                          #note = "example"                          # Optional: Additional note to append to the file name
                                          )
```

``` r
MixSIAR.preds.mixE[1:5,]
#>              sample Median_Forest Median_Subsoil Median_Undecontaminated total
#> 1 ManoDd_2106_00-01         0.191          0.031                   0.778 0.999
#> 2 ManoDd_2106_01-02         0.070          0.032                   0.898 0.999
#> 3 ManoDd_2106_02-03         0.072          0.044                   0.884 0.999
#> 4 ManoDd_2106_03-04         0.073          0.063                   0.864 0.998
#> 5 ManoDd_2106_04-05         0.068          0.027                   0.905 1.000
```

Same code for virtual mixtures:

``` r
## Summarise MixSIAR model previsions
MixSIAR.summary.VM <- fingR::JAGS.summary(jags.1 = jags.VM,                            # Data from the MixSIAR model `MixSIAR::run_model()`
                                          mix = MSIAR.VM,                              # Virtual mixtures dataset 
                                          sources = MSIAR.source.VM,                   # Source dataset loaded with virtual mixture mix
                                          path = dir.mod.MixSIAR,                      # Directory path for saving the files
                                          note = "VM",                                 # Optional: Additional note to append to the file name
                                          save_pred = TRUE                             # Optional: Save the MixSIAR modelling predictions (heavy files)
                                          )

## Extracts the median value of the previsions      
MixSIAR.preds.VM <- fingR::JAGS.pred(path = paste0(dir.mod.MixSIAR, "contrib_VM.csv"), # location of files generated by `JAGS.summary`
                                     stats = "Median",                                 # Summary statistics to calculate (Median or Mean)
                                     save = TRUE,                                      # If the result should be saved
                                     note = "VM"                                       # Optional: Additional note to append to the file name 
                                     )

## Ensure that the total predicted contribution sums to 1 or 100%
MixSIAR.preds.VME <- fingR::ensure.total(data = MixSIAR.preds.VM,                      # Predicted source contribution for each sample, data from fingR::BMM.pre
                                          sample.name = "sample",                      # Column name for sample identifier
                                          path = dir.mod.MixSIAR,                      # Optional: Directory path for saving the results
                                          note = "VM"                                  # Optional: Additional note to append to the file name
                                          )
```

``` r
MixSIAR.preds.VME[1:5,]
#>   sample Median_Forest Median_Subsoil Median_Undecontaminated total
#> 1 VM-001         0.150          0.398                   0.451 0.999
#> 2 VM-002         0.069          0.418                   0.513 1.000
#> 3 VM-003         0.084          0.452                   0.464 1.000
#> 4 VM-004         0.081          0.480                   0.439 1.000
#> 5 VM-005         0.076          0.510                   0.414 1.000
```

#### Modelling accuracy statistics

The modelling accuracy of MixSIAR model is evaluate with the virtual
mixtures. These virtual mixtures, serving as target samples with known
contributions (*VM.contrib*), allow for the calculation of modelling
accuracy metrics based on their prediction.

The `eval.groups` function calculates several common modelling accuracy
metrics: ME, RMSE, squared Pearson’s correlation coefficient (r2), and
Nash-Sutcliff Modelling Efficiency Coefficient (NSE).

``` r
MixSIAR.stats <- fingR::eval.groups(df.obs = VM.contrib,                                   # Theoretical contribution
                                    df.pred = MixSIAR.preds.VME %>% dplyr::select(-total), # Predicted contribution (remove the $total column from ensured data.frame)
                                    by = c("Sample_name" = "sample"),                      # Column where mixtures labels are specified (for `dplyr::left_join` function)
                                    path = dir.mod.MixSIAR,                                # Optional: Directory path for saving the results
                                    #note = "example"                                      # Optional: Additional note to append to the file name
                                    )
```

``` r
MixSIAR.stats
#>     Type           Source    ME RMSE   r2   NSE
#> 1 Median           Forest -0.20 0.25 0.73 -0.01
#> 2 Median          Subsoil  0.08 0.14 0.79  0.69
#> 3 Median Undecontaminated  0.12 0.28 0.08 -0.27
```

The `CRPS` functions calculate the continuous ranking probability score
and returns a list contraining two *data.frame* objects; one with the
`$samples` CRPS values per source class group (saved as *CRPS.csv*), the
other is `$mean` with the mean of the CRPS per source class groups
(saved as *CRPS_mean.csv*).

``` r
# Calculate prediction CRPS values
MixSIAR.CRPS <- fingR::CRPS(obs = VM.contrib,                                                     # Observed contributions
                            prev = read.csv(paste0(dir.mod.MixSIAR, "MixSIAR_prevision_VM.csv")), # Predicted prevision from MixSIAR saved by `JAGS.summary()`
                            source.groups = c("Forest", "Subsoil", "Undecontaminated"),           # Source class groups
                            mean.cal = TRUE,                                                      # Calculate mean CRPS per source class group
                            save.dir = dir.mod.MixSIAR,                                           # Optional: Directory path for saving the results
                            #note = "example"                                                     # Optional: Additional note to append to the file name
                            )
```

``` r
MixSIAR.CRPS$samples[1:6,]
#>   Sample_name Forest Subsoil Undecontaminated
#> 1      VM-001 0.0838  0.3950           0.4923
#> 2      VM-002 0.0500  0.2945           0.3661
#> 3      VM-003 0.0570  0.2867           0.3633
#> 4      VM-004 0.0572  0.2645           0.3391
#> 5      VM-005 0.0519  0.2555           0.3261
#> 6      VM-006 0.0455  0.2404           0.3042
```

``` r

MixSIAR.CRPS$mean
#>             Source CRPS.mean
#> 1           Forest    0.1520
#> 2          Subsoil    0.0801
#> 3 Undecontaminated    0.1715
```

The `interval.width` functions calculate two prediction interval width:
The *W50* contains 50% of the prevision (Q75-Q25) and the *W95* contains
95% of the prevision (Q97.5-Q2.5). It returns a list contraining two
*data.frame* objects; one with the `$samples` prediction interval width
values per source class group (saved as *Interval_width.csv*), the other
is `$mean` with the mean of the prediction interval width per source
class groups (saved as *Interval_width_mean.csv*).

``` r
# Calculate prediction interval width (W95, W50)
MixSIAR.predWidth <- fingR::interval.width(path.to.prev = paste0(dir.mod.MixSIAR, "MixSIAR_prevision_VM.csv"), # Predicted prevision from MixSIAR saved by `JAGS.summary()`
                                           mean.cal = TRUE,                                                    # Calculate mean of interval width per source group
                                           save = TRUE,                                                        # Save the results at the same location of the path.to.prev
                                           #note = "exemple"                                                   # Optional: Additional note to append to the file name
                                           )
```

``` r
BMM.predWidth$samples[1:6,]
#>   mix.names           source   W50   W95
#> 1    VM-001           Forest 0.242 0.794
#> 2    VM-001          Subsoil 0.584 0.997
#> 3    VM-001 Undecontaminated 0.332 0.845
#> 4    VM-002           Forest 0.322 0.595
#> 5    VM-002          Subsoil 0.613 0.997
#> 6    VM-002 Undecontaminated 0.498 0.897
```

``` r

BMM.predWidth$mean
#> # A tibble: 3 × 3
#>   Source           W50.mean W95.mean
#>   <chr>               <dbl>    <dbl>
#> 1 Forest              0.352    0.873
#> 2 Subsoil             0.586    0.973
#> 3 Undecontaminated    0.41     0.927
```

The `ESP` function calculates the Encompassed Sample Prediction (ESP).
The ESP is a newly introduced statistics in [Chalaux-Clergue et al
(under review)]() and was created to assess the transferability of the
statistics calculated on virtual mixtures to actual sediment samples.
The ESP was calculated as the percentage of actual samples for which the
predicted contributions remained within the lowest and the highest
predicted contributions obtained for the virtual mixtures. When
expressed as a percentage, ESP ranges from 0 to 100%, the latter
providing an optimal value. Values close to 100% indicate a higher
transferability of modelling evaluation statistics calculated on virtual
mixture to actual sediment samples.

``` r
sources.lvl <- c("Forest", "Subsoil", "Undecontaminated")

# Calculate encompassed sample predictions (ESP)
MixSIAR.ESP <- fingR::ESP(obs = MixSIAR.preds.VM,                   # Virtual mixtures predicted contributions
                          pred = MixSIAR.preds.mixE,                # Actual sediment samples predicted contributions
                          sources = paste0("Median_", sources.lvl), # Sources labels in prediction objects
                          count = "Both"                            # Count 'Number' and 'Percentage'
                          )
```

``` r
MixSIAR.ESP
#>                                   Source ESP.Number ESP.Percentage
#> Median_Forest                     Forest         35             92
#> Median_Subsoil                   Subsoil          0              0
#> Median_Undecontaminated Undecontaminated          9             24
```

Modelling accuracy statistics could be interpreted the following way:
“Higher values of W50 indicate a wider distribution, which is related to
a higher uncertainty. The sign of the ME indicates the direction of the
bias, i.e. an overestimation or underestimation (positive or negative
value, respectively). As ME is affected by cancellation, a ME of zero
can also reflect a balanced distribution of predictions around the 1 : 1
line. Although this is not a bias, it does not mean that the model
outputs are devoid of errors. The RMSE is a measure of the accuracy and
allows us to calculate prediction errors of different models for a
particular dataset. RMSE is always positive, and its ideal value is
zero, which indicates a perfect fit to the data. As RMSE depends on the
squared error, it is sensitive to outliers. The r2 describes how linear
the prediction is. The NSE indicates the magnitude of variance explained
by the model, i.e. how well the predictions match with the observations.
A negative RMSE indicates that the mean of the measured values provides
a better predictor than the model. The joint use of r2 and NSE allows
for a better appreciation of the distribution shape of predictions and
thus facilitates the understanding of the nature of model prediction
errors. The CRPS evaluates both the accuracy and sharpness
(i.e. precision) of a distribution of predicted continuous values from a
probabilistic model for each sample (Matheson and Winkler, 1976). The
CRPS is minimised when the observed value corresponds to a high
probability value in the distribution of model outputs.”
[(Chalaux-Clergue et al, 2024)](10.5194/soil-10-109-2024).

## Future updates

Upcoming updates will introduce graphical support functions such as
*Bayesian prediction density plots*, *prediction vs. observation plots*,
and *ternary diagrams*.

## Getting help

If you encounter a clear bug, please file and issue or send an email to
[Thomas Chalaux-Clergue and Rémi
Bizeul](mailto:thomaschalaux@icloud.com,%20rbizeul59@gmail.com).

## Citation

To cite this packages:

``` r
utils::citation(package = "fingR")
#> To cite the 'fingR' package in publications please use:
#> 
#>   Chalaux-Clergue, T. and Bizeul, R (2024). fingR: A package to support
#>   sediment source fingerprinting studies, Zenodo [Package]:
#>   https://doi.org/10.5281/zenodo.8293595, Github [Package]:
#>   https://github.com/tchalauxclergue/fingR, Version = 2.0.0.
#> 
#> Une entrée BibTeX pour les utilisateurs LaTeX est
#> 
#>   @Manual{,
#>     title = {fingR: A package to support sediment source fingerprinting studies},
#>     author = {{Chalaux-Clergue} and {Thomas} and {Bizeul} and {Rémi}},
#>     year = {2024},
#>     month = {6},
#>     note = {R package version 2.0.0},
#>     doi = {https://doi.org/10.5281/zenodo.8293595},
#>     url = {https://github.com/tchalauxclergue/fingR},
#>   }
```

## References

- Chalaux-Clergue, T., Bizeul, R., Foucher, A., & Evrard, O. (2024a). An
  unified template for sediment source fingerprinting databases
  (24.03.01) \[Data set\]. Zenodo.
  <https://doi.org/10.5281/zenodo.10725787>.

- Chalaux-Clergue, T., Evrard, O., Durand, R., Caumon, A., Hayashi, S.,
  Tsuji, H., Huon, S., Vaury, V., Wakiyama, Y., Nakao, A., Laceby, J.
  P., & Onda, Y. (2024b). Organic matter, geochemical, visible
  spectrocolorimetric properties, radiocesium properties, and grain size
  of potential source material, target sediment core layers and
  laboratory mixtures for conducting sediment fingerprinting approaches
  in the Mano Dam Reservoir (Hayama Lake) catchment, Fukushima
  Prefecture, Japan (Version 2) \[Data set\]. Zenodo.
  <https://doi.org/10.5281/zenodo.10836974>.

- Chalaux-Clergue, T., Bizeul, R., Batista, P. V. G., Martinez-Carreras,
  N., Laceby, J. P., Evrard, P. (2024c). Sensitivity of source sediment
  fingerprinting to tracer selection. SOIL, 10(1), 109-138.
  <https://doi.org/10.5194/soil-10-109-2024>.

- Chalaux-Clergue, T., & Bizeul, R. (2024d). fingR: A support for
  sediment source fingerprinting studies (All version). Zenodo.
  <https://doi.org/10.5281/zenodo.8293595>. Github.
  <https://github.com/tchalauxclergue/fingR>.

- Laceby JP, Olley J, Pietsch TJ, Sheldon F, Bunn SE. Identifying
  subsoil sediment sources with carbon and nitrogen stable isotope
  ratios. Hydrological Processes. 15 avr 2015;29(8):1956‑71.
  <https://doi.org/10.1002/hyp.10311>

- Stock, B. C., Jackson, A. L., Ward, E. J., Parnell, A. C.,
  Phillips, D. L., & Semmens, B. X. (2018). Analyzing mixing systems
  using a new generation of Bayesian tracer mixing models. PeerJ, 6,
  e5096. <https://doi.org/10.7717/peerj.5096>.

- Stock, B. C., Jackson, A. L., Ward, E. J., Parnell, A. C.,
  Phillips, D. L. (2020). MixSIAR: Bayesian Mixing Models in R (Version
  3.1.12). Zenodo. <https://doi.org/10.5281/zenodo.594910>. Github.
  <https://github.com/brianstock/MixSIAR/tree/3.1.11>
