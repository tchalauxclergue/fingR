
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fingR <a href="10.5281/zenodo.8293595"><img src="man/figures/fingR_logo_ver2_small_300dpi.png" align="right" height="138" /></a>

<!-- badges: start -->

![GitHub
version](https://img.shields.io/github/r-package/v/tchalauxclergue/fingR?logo=github)
![GitHub Release
Date](https://img.shields.io/github/release-date/tchalauxclergue/fingR?color=blue)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1402028.svg)](https://doi.org/10.5281/zenodo.10044404)
[![GitHub
Downloads](https://img.shields.io/github/downloads/tchalauxclergue/fingR/total?label=GitHub%20downloads&style=flat)](https://github.com/tchalauxclergue/fingR/releases)
[![Zenodo
Downloads](https://img.shields.io/badge/Zenodo%20downloads-107-blue)](https://zenodo.org/records/15412071)
![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)
<!-- badges: end -->

# Overview

The `fingR` is a comprehensive package designed to support Sediment
Source Fingerprinting studies.It provides essential tools including:
dataset characterisation, tracer selection from analysed properties
using the three-step method, modelling of source contributions using the
Bayesian Mixing Model (BMM), and evaluation of model predictions using
virtual mixtures, and it supports BMM and
[MixSIAR](http://brianstock.github.io/MixSIAR/index.html) models.

The `fingR` package is available in this
[Github](https://github.com/tchalauxclergue/fingR) repository and
archived on [Zenodo](https://zenodo.org/records/10796375).

# Table of content

<!-- toc -->

- [Installation](#installation)
- [Usage](#usage)
  - [1 - Data preparation](#1---data-preparation)
    - [1.1 - Data construction](#11---data-construction)
    - [1.2 - Selecting properties
      vectors](#12---selecting-properties-vectors)
    - [1.3 - Evaluation of measurement
      quality](#13---evaluation-of-measurement-quality)
  - [2 - Tracer Selection](#2---tracer-selection)
    - [2.1 - Conservative behaviour](#21---conservative-behaviour)
    - [2.2 - Discriminant power](#22---discriminant-power)
    - [2.3 - Identified tracers: conservative and discriminant
      properties](#23---identified-tracers-conservative-and-discriminant-properties)
    - [2.4 - Discriminat Function Analysis stepwise
      selection](24---discriminat-function-analysis-stepwise-selection)
  - [3 - Source Contribution
    Modelling](#3---source-contribution-modelling)
    - [3.1 - Virtual Mixtures
      Generation](#31---virtual-mixtures-generation)
    - [3.2 - Un-mixing models](#32---un-mixing-models)
      - [3.2.1 - Bayesian Mean Model
        (BMM)](#321---bayesian-mean-model-bmm)
        - [3.2.1.a - Run BMM](#321a---run-bmm)
        - [3.2.1.b - Collect and organise BMM
          contributions](#321b---collect-and-organise-bmm-contributions)
      - [3.2.2 - MixSIAR model](#322---mixsiar-model)
        - [3.2.2.a - Generate data for
          MixSIAR](#322a---generate-data-for-mixsiar)
        - [3.2.2.b - Load mixture, source and discrimination
          data](#322b---load-mixture-source-and-discrimination-data)
        - [3.2.2.c - Write JAGS model
          file](#322c---write-jags-model-file)
        - [3.2.2.d - Run MixSIAR](#322d---run-mixsiar)
        - [3.2.2.e - Collect and organise MixSIAR
          contributions](#322e---collect-and-organise-mixsiar-contributions)
    - [3.3 - Modelling accuracy statistics
      calculation](#331---modelling-accuracy-statistics-calculation)
      - [3.3.1 - General accuracy
        metrics](#332---continuous-ranked-probability-score)
      - [3.3.2 - Continuous ranked probability
        score](#332---continuous-ranked-probability-score)
      - [3.3.3 - Prediction interval
        width](#333---prediction-interval-width)
      - [3.3.4 - Encompassed sample
        predictions](#334---encompassed-sample-predictions)
- [Future updates](#future-updates)
- [Getting help](#getting-help)
- [Citation](#citation)
- [References](#references)

<!-- tocstop -->

# Installation

``` r
# install.packages(devtools) # if not installed yet
library(devtools)

# Install the most recent version from GitHub - check github page for updates
devtools::install_github("https://github.com/tchalauxclergue/fingR/releases/tag/2.1.3", ref = "master", force = T)

# Alternatively, install from a downloaded '.tar.gz' file
devtools::install_local("path_to_file/fingR_2.1.3.tar.gz", repos = NULL)
# 'path_to_file' should be modified accordingly to your working environment
```

# Usage

To illustrate the usage of the fingR package, a database containing
layers from a sediment core and potential sources samples was used. The
38 layers of a sediment core collected in the Mano Dam reservoir
(Fukushima, Japan) in June 2021 were used as mixture targets. The
potential source samples include four classes: undecontaminated cropland
(*n* = 24), remediated cropland (*n* = 22), forest (*n* = 24), and
subsoil (mainly granite saprolite; *n* = 24). All sediment and soil
samples were sieved to 63 μm and analysed, the organic matter (total
organic carbon (TOC), and total nitrogen (TN)) and elemental
geochemistry by ED-XRF (Al, Ca, Co, Cr, Cu, Fe, K, Mg, Mn, Ni, Pb, Rb,
Si, Sr, Ti, Zn, Zr) were used as potential tracer properties.

This dataset, along with detailed measurement protocols, is available
for download on Zenodo at [Chalaux-Clergue et
al. (2024c)](https://zenodo.org/doi/10.5281/zenodo.7081093).

For more details about functions, please read function vignettes
(RStudio ‘Help’ section - press F1 for a quick access).

## 1 - Data preparation

The first step is to load the database, in this guide it consists of a
data file and a metadata file. To support database sharing and
compatibility, a database template has been proposed and made available
on Zenodo at [Chalaux-Clergue et al., (2024b) (ver.
24.03.01)](https://doi.org/10.5281/zenodo.10725788).

``` r
# Load the fingR package
library(fingR)

# Get the directory to data and metadata files within the fingR package
data.dr <- system.file("extdata", "TCC_MDD_20210608_data_ChalauxClergue_et_al_v240319.csv", package = "fingR")
metadata.dr <- system.file("extdata", "TCC_MDD_20210608_metadata_ChalauxClergue_et_al_v240319.csv", package = "fingR")

# Load the csv files of data and metadata - replace the dir with your file direction
db.data <- read.csv(data.dr, sep = ";", fileEncoding = "latin1", na = "")
db.metadata <- read.csv(metadata.dr, sep = ";", fileEncoding = "latin1", na = "")
```

The different classes of potential sources and sediment samples
information is in the `Class_decontamination column`. In which, the
*Target* class refers to sediment core layers, and other classes to
potential sediment sources.

``` r
table(db.metadata$Class_decontamination)
#> 
#>           Forest       Remediated          Subsoil           Target 
#>               24               10               10               38 
#> Undecontaminated 
#>               24
```

### 1.1 - Data construction

A single dataset is built by joining the metadata (general information)
and the data (analyses) sub-datasets. Both metadata and data
`data.frames` are joined by common variables, here using the columns
`IGSN` and `Sample_name`. In addition, two filters were applied. A
filter to keep the analysed performed on the fraction ≤ 63 μm, and a
filter to remove remediated cropland from the dataset, and therefore
using three potential sources.

``` r
library(dplyr)

# Create a single dataset with metadata and data information
database <- dplyr::left_join(x = db.metadata,
                             y = db.data,
                             by = join_by(IGSN, Sample_name)) %>%
  dplyr::filter(Sample_size == "< 63 µm") %>% # Filter sample fraction
  dplyr::filter(Class_decontamination != "Remediated") # Remove remediated cropland to keep 3 potential sources
```

Therefore, the final dataset contains three potential sources
(i.e. forest, subsoil, and undecontaminated cropland).

``` r
table(database$Class_decontamination)
#> 
#>           Forest          Subsoil           Target Undecontaminated 
#>               24               10               38               24
```

### 1.2 - Selecting properties vectors

Among the analysed properties, a vector is created listing the column
names of the values of the potential tracer properties,
i.e. `prop.values`.

``` r
# Display the database columns
# colnames(database)

# Select the column names of the properties values
prop.values <- database %>%
  dplyr::select(TOC_PrC, TN_PrC, # organic matter
                EDXRF_Al_mg.kg.1:EDXRF_Zr_mg.kg.1) %>% # elemental geochemistry
  base::names() # Extract column names
```

``` r
prop.values
#>  [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1" "EDXRF_Ca_mg.kg.1"
#>  [5] "EDXRF_Co_mg.kg.1" "EDXRF_Cr_mg.kg.1" "EDXRF_Cu_mg.kg.1" "EDXRF_Fe_mg.kg.1"
#>  [9] "EDXRF_K_mg.kg.1"  "EDXRF_Mg_mg.kg.1" "EDXRF_Mn_mg.kg.1" "EDXRF_Ni_mg.kg.1"
#> [13] "EDXRF_Pb_mg.kg.1" "EDXRF_Rb_mg.kg.1" "EDXRF_Si_mg.kg.1" "EDXRF_Sr_mg.kg.1"
#> [17] "EDXRF_Ti_mg.kg.1" "EDXRF_Zn_mg.kg.1" "EDXRF_Zr_mg.kg.1"
```

A vector of property measurement uncertainty column names is also
created, `prop.uncertainties`. In order to maintain the relationship
between the column names of the properties and the uncertainty, the
column names of the properties are set as `names` of the vector of
property names. This also simplifies the selection of the uncertainty
column names, as they can be selected from the property column names.

``` r
# Select the column names of the property measurement uncertainties values
prop.uncertainties <- database %>%
  dplyr::select(TOC_SD, TN_SD,
                EDXRF_Al_RMSE:EDXRF_Zr_RMSE) %>%
  base::names()

# Set property names to property uncertainty names for easier selection
base::names(prop.uncertainties) <- prop.values
```

``` r
unname(prop.uncertainties)
#>  [1] "TOC_SD"        "TN_SD"         "EDXRF_Al_RMSE" "EDXRF_Ca_RMSE"
#>  [5] "EDXRF_Co_RMSE" "EDXRF_Cr_RMSE" "EDXRF_Cu_RMSE" "EDXRF_Fe_RMSE"
#>  [9] "EDXRF_K_RMSE"  "EDXRF_Mg_RMSE" "EDXRF_Mn_RMSE" "EDXRF_Ni_RMSE"
#> [13] "EDXRF_Pb_RMSE" "EDXRF_Rb_RMSE" "EDXRF_Si_RMSE" "EDXRF_Sr_RMSE"
#> [17] "EDXRF_Ti_RMSE" "EDXRF_Zn_RMSE" "EDXRF_Zr_RMSE"
```

### 1.3 - Evaluation of measurement quality

The `data.watcher` function allows to check the format of the selected
properties. The function verifies if the values of the property are all
negative (e.g. δ<sup>13</sup>C, δ<sup>15</sup>N ), which will require an
inversion of the value prior to Bayesian modelling where the data is
systematically log-transformed, and if some samples have negative
values, which will require manual verification or correction of the
data. If the measurement uncertainties are provided to the `prop.uncer`
argument, the function will indicate if the measurement uncertainty
makes some values virtually impossible, and will give the maximum
relative uncertainty if it is greater than 5 %. The result of this
function should encourage the user to consider the quality of his data.

``` r
fingR::data.watcher(data = database,                 # database data.frame
                    properties = prop.values,        # vector of property labels
                    prop.uncer = prop.uncertainties) # vector of measurement uncertainty labels
#> 
#> Following column(s) contain(s) some negative values: EDXRF_Cr_mg.kg.1.
#> Following column(s) have a measurement uncertainty that makes some values to be virtually impossible: EDXRF_Co_mg.kg.1, EDXRF_Cr_mg.kg.1, EDXRF_Cu_mg.kg.1, EDXRF_Ni_mg.kg.1.
#> Following column(s) have a relative measurement uncertainty above 5% (up to - number): EDXRF_Co_mg.kg.1 (max:753% - n:26), EDXRF_Cr_mg.kg.1 (max:211% - n:38), EDXRF_Ni_mg.kg.1 (max:105% - n:96), EDXRF_Cu_mg.kg.1 (max:103% - n:52), EDXRF_Rb_mg.kg.1 (max:89% - n:93), TN_PrC (max:45% - n:91), EDXRF_Pb_mg.kg.1 (max:38% - n:91), EDXRF_Zn_mg.kg.1 (max:34% - n:96), EDXRF_Sr_mg.kg.1 (max:15% - n:46), TOC_PrC (max:14% - n:95), EDXRF_Zr_mg.kg.1 (max:7% - n:2).
```

According to the `data.watcher` function, some samples have negative
ED-XRF Cr values and the Co, Cr, Cu, Ni and Rb appear to be high. These
properties are removed from the properties used in the following guide.

``` r
# Remove the properties listed from the vector of properties
prop.values <- prop.values[!prop.values %in% c("EDXRF_Co_mg.kg.1", "EDXRF_Cr_mg.kg.1", "EDXRF_Cu_mg.kg.1", "EDXRF_Ni_mg.kg.1", "EDXRF_Rb_mg.kg.1")]
```

``` r
prop.values
#>  [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1" "EDXRF_Ca_mg.kg.1"
#>  [5] "EDXRF_Fe_mg.kg.1" "EDXRF_K_mg.kg.1"  "EDXRF_Mg_mg.kg.1" "EDXRF_Mn_mg.kg.1"
#>  [9] "EDXRF_Pb_mg.kg.1" "EDXRF_Si_mg.kg.1" "EDXRF_Sr_mg.kg.1" "EDXRF_Ti_mg.kg.1"
#> [13] "EDXRF_Zn_mg.kg.1" "EDXRF_Zr_mg.kg.1"
```

As well the measurement uncertainties of these properties are removed
from the selection.

``` r
# Keep uncertainties associated to the new vector of properties
prop.uncertainties <- prop.uncertainties[prop.values]
```

``` r
prop.uncertainties
#>          TOC_PrC           TN_PrC EDXRF_Al_mg.kg.1 EDXRF_Ca_mg.kg.1 
#>         "TOC_SD"          "TN_SD"  "EDXRF_Al_RMSE"  "EDXRF_Ca_RMSE" 
#> EDXRF_Fe_mg.kg.1  EDXRF_K_mg.kg.1 EDXRF_Mg_mg.kg.1 EDXRF_Mn_mg.kg.1 
#>  "EDXRF_Fe_RMSE"   "EDXRF_K_RMSE"  "EDXRF_Mg_RMSE"  "EDXRF_Mn_RMSE" 
#> EDXRF_Pb_mg.kg.1 EDXRF_Si_mg.kg.1 EDXRF_Sr_mg.kg.1 EDXRF_Ti_mg.kg.1 
#>  "EDXRF_Pb_RMSE"  "EDXRF_Si_RMSE"  "EDXRF_Sr_RMSE"  "EDXRF_Ti_RMSE" 
#> EDXRF_Zn_mg.kg.1 EDXRF_Zr_mg.kg.1 
#>  "EDXRF_Zn_RMSE"  "EDXRF_Zr_RMSE"
```

## 2 - Tracer Selection

### 2.1 - Conservative behaviour

The three-step method assesses conservative behaviour using range tests
(RT), also known as bracket tests. For a property to be considered
conservative, the values of each target sample must fall within the
range of potential source classes. This range is defined by the highest
and lowest values in a given source class for a given criterion. The
`range.test` function allows the calculation of the range test on each
property according to multiple range test criteria.

Several range test criteria have been documented, including
minimum-maximum (`MM`), minimum-maximum with a 10 % margin of error
(`MMe`), boxplot whiskers (outlier exclusion threshold), boxplot hinges
(representing the middle 50 %), mean, mean plus/minus one standard
deviation (`mean.sd`), and median. For the mean and `mean.sd` criteria,
log-transformed values are used, assuming a normal distribution. The
function applies all criteria by default (`criteria = c("all")`), or the
user can freely select several tests from the following list: `MM`,
`MMe`, `whiskers`, `hinge`, `mean`, `mean.sd` and/or `median`. Their
effectiveness in detecting conservative properties varies. Among them,
the <ins>`mean.sd` criterion</ins> is mathematically the most robust.

The `range.test` function outputs a list with two data frames:

- `$results.df` : A summary overview of all properties range test
  outcomes.

- `$results.RT` : Detailed range test results for each target sample
  across properties, where `TRUE` indicates that the sample value is
  within the range, while `high` and `low` indicate values outside the
  range.

Within the `range.test` function, the `class` argument specifies the
column name containing the classifications for potential source and
target mixture samples. The label for the target mixtures used in
`class` should be provided in the `mixture` argument. The `sample.id`
argument designates the column containing the unique identifiers or
labels for each sample. When a saving directory path the results is
provided to `save.dir`, the function write two CSV files:
“RT_samples\[\_note\].csv” corresponding to `results.RT`, and
“Range_test\[\_note\].csv” corresponding to `results.df`.

``` r
rt.results <- fingR::range.tests(data = database,
                                 class = "Class_decontamination",
                                 mixture = "Target",
                                 properties = prop.values,
                                 sample.id = "Sample_name",
                                 criteria = c("mean.sd")
                                 # MM.error = c(0.1),      # Optional
                                 # save.dir = dir.example, # Optional
                                 # note = "example"        # Optional
                                 )
```

``` r
# Results of the mean.sd range test for all properties
rt.results$results.df
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

The result of the range test for some target samples and a property that
did not pass the range test.

``` r
rt.results$results.RT$EDXRF_Pb_mg.kg.1[1:5,]
#>         Sample_name         Property n_source RT_mean.sd
#> 1 ManoDd_2106_00-01 EDXRF_Pb_mg.kg.1       58       TRUE
#> 2 ManoDd_2106_01-02 EDXRF_Pb_mg.kg.1       58       TRUE
#> 3 ManoDd_2106_02-03 EDXRF_Pb_mg.kg.1       58       high
#> 4 ManoDd_2106_03-04 EDXRF_Pb_mg.kg.1       58       high
#> 5 ManoDd_2106_04-05 EDXRF_Pb_mg.kg.1       58       TRUE
```

Based on the `$results.df` data frame from `range.tests`, the
`is.conservative` function filter the properties that passed the range
tests for each criterion, and returns a list of vectors. Each vector
contains the names of the properties that were identified as
conservative according to each criterion.

``` r
prop.cons <- fingR::is.conservative(data = rt.results$results.df,
                                    # property = "Property",
                                    # test.format = "RT",
                                    # position = 2,
                                    # separator = "_",
                                    # note = "example"
                                    )
```

``` r
prop.cons
#> $mean.sd
#> [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1" "EDXRF_Ti_mg.kg.1"
```

### 2.2 - Discriminant power

The three-step method evaluates the ability of the traits to
discriminate between source groups using a statistical test to test
whether or not samples are from the same distribution. Conventionally,
the non-parametric Kruskal-Wallis H test (KW) is used. The KW test is an
extension of the two-sample Mann-Whitney U test. As an alternative to
the KW test, the two-sample Kolmogorov-Smirnov (KS) test could be used.
The KS test allows for a more detailed understanding of the
discrimination between pairs of source groups. For instance, the KS
tests for for three source groups will be structured as: source A
vs. source B, source A vs. source C, and source B vs source C. The
`discriminant.test` function allows the calculation of the discriminant
power of each property. By setting the using the test argument to `KW`
or `KS` , the Kruskal-Wallis H-test or Kolmogorov-Smirnov test is
carried out.

When using the KS test (`test = "KW"`), the significance of the test *p*
value is commonly set to 5 % (`p.level = 0.05`). Whilst, when using the
Kolmogorov-Smirnov test ( test = “KS” ), which is highly sensitive to
distribution differences, the significance of the test *p* value could
be set to lower value, for instance, 1 % (`p.level = 0.01`).

Two CSV files will be written if the path to the save directory is given
to the `save.dir` argument:

- ’Discriminant_pairs\[\_note\].csv’, generated if
  `save.discrim.tests = TRUE` (default), contains the *p* value of each
  two-samples KS tests for each property. Set `return.tests = TRUE` to
  return this table as `$result.KS` (default is `FALSE`). Of note, when
  using the KW test, the KS test results can also help verify the KW
  assumption of similar distribution shape across groups.

- ’Discriminant_tests\[\_note\].csv’ summarises the results from KS or
  KW tests.

  - For `test = "KW"`, the number of groups that follows a similar
    distribution shape(`n.similar.groups`) is reported. If this value
    matches the total number of source groups, the the assumption of
    similar distribution shapes is satisfied
    (`respect.KW.shape.assumption`). The table also includes the *p*
    value (`KW_p.value`) and its significance (`KW_signif`) for each
    properties.

  - For `test = "KS"`, the number of statistically different groups
    (`n.diff.groups`) is reported, and if it is at least equal to the
    threshold (`min.discriminant = 1`) required to identify a property
    as discriminant, the property is marked as discriminant in
    `KS_discriminant`.

Example for KW test:

``` r
KW.results <- fingR::discriminant.test(data = database,
                                       class = "Class_decontamination",
                                       mixture = "Target",
                                       test = "KW",
                                       properties = prop.values,
                                       p.level = .05,
                                       # min.discriminant = 1,
                                       # save.discrim.tests = TRUE,
                                       return.tests = TRUE,
                                       # save.dir = dir.example,
                                       # note = "example"
                                       )
```

``` r
KW.results$result.KS[1:3,]
#>   Property Source_A         Source_B n_source_A n_source_B KS_p.value KS_signif
#> 1  TOC_PrC   Forest          Subsoil         24         10   1.01e-06       ***
#> 2  TOC_PrC   Forest Undecontaminated         24         24   4.57e-06       ***
#> 3  TOC_PrC  Subsoil Undecontaminated         10         24   1.34e-06       ***
```

``` r
KW.results$results.df[1:5,]
#>           Property n.similar.groups respect.KW.shape.assumption KW_p.value
#> 1          TOC_PrC                0                       FALSE    0.00000
#> 2           TN_PrC                0                       FALSE    0.00000
#> 3 EDXRF_Al_mg.kg.1                0                       FALSE    0.00000
#> 4 EDXRF_Ca_mg.kg.1                1                       FALSE    0.00301
#> 5 EDXRF_Fe_mg.kg.1                3                        TRUE    0.79411
#>   KW_signif
#> 1       ***
#> 2       ***
#> 3       ***
#> 4        **
#> 5
```

Example for KS test:

``` r
KS.results <- fingR::discriminant.test(data = database,
                                       class = "Class_decontamination",
                                       mixture = "Target",
                                       test = "KS",
                                       properties = prop.values,
                                       p.level = .01,
                                       # min.discriminant = 1,
                                       # save.discrim.tests = TRUE,
                                       return.tests = TRUE,
                                       # save.dir = dir.example,
                                       # note = "example"
                                       )
```

``` r
KS.results$result.KS[1:3,]
#>   Property Source_A         Source_B n_source_A n_source_B KS_p.value KS_signif
#> 1  TOC_PrC   Forest          Subsoil         24         10   1.01e-06       ***
#> 2  TOC_PrC   Forest Undecontaminated         24         24   4.57e-06       ***
#> 3  TOC_PrC  Subsoil Undecontaminated         10         24   1.34e-06       ***
```

``` r
KS.results$results.df[1:5,]
#>           Property n.diff.groups KS_discriminant
#> 1          TOC_PrC             3            TRUE
#> 2           TN_PrC             3            TRUE
#> 3 EDXRF_Al_mg.kg.1             3            TRUE
#> 4 EDXRF_Ca_mg.kg.1             1            TRUE
#> 5 EDXRF_Fe_mg.kg.1             0           FALSE
```

The results from the KS test are used for the following guide.

Based on the `$results.df` data frame from `discriminant.test`, the
`is.discriminant` function filter out the properties that passed the
discriminant power tests according to the set p value (`p.level`) as
list them in a vector. The function recognise the structure of the data
frame produced by the `discriminant.test` function, however, it is
possible to set it for other data frame format by setting its arguments.

``` r
prop.discrim <- fingR::is.discriminant(KS.results$results.df,
                                       # property = "Property",
                                       # test.format = "Kruskal.Wallis_p.value",
                                       # test.pos = 1,
                                       # sep.format = "_",
                                       # p.level = 0.01,
                                       # note = "example"
                                       )
```

``` r
prop.discrim
#> $KS
#>  [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1" "EDXRF_Ca_mg.kg.1"
#>  [5] "EDXRF_K_mg.kg.1"  "EDXRF_Pb_mg.kg.1" "EDXRF_Si_mg.kg.1" "EDXRF_Sr_mg.kg.1"
#>  [9] "EDXRF_Zn_mg.kg.1" "EDXRF_Zr_mg.kg.1"
```

### 2.3 - Identified tracers: conservative and discriminant properties

The properties that pass both the evaluation of conservative behaviour
(`prop.cons` from `is.conservative`) and discriminant power
(`prop.discrim` from `is.discriminant`) are compared by the
`selected.tracers` function, which list them in a vector.

``` r
tracers <- fingR::is.tracers(cons = prop.cons,
                             discrim = prop.discrim)
```

``` r
tracers 
#> $mean.sd_KS
#> [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1"
```

### 2.4 - Discriminat Function Analysis stepwise selection

Frequently, a Discriminant Function Analysis (DFA) stepwise selection is
applyed on the selected tracers. The DFA stepwise selection aims to
retain the tracers that maximise source discrimination. Of note, the DFA
stepwise selection has been criticised since its use is associated to a
reduction of the number of tracers, especially since some studies showed
that the use of a higher number of tracers decreases the sensitivity of
sediment source fingerprinting modelling to non-conservative tracers
([Martínez-Carreras et al.,
2008](https://iahs.info/uploads/dms/14500.16-94-105-11-325-Martinez.pdf);
[Sherriff et al.,
2015](http://link.springer.com/10.1007/s11368-015-1123-5)). In addition,
it is not useful for small selection of tracers, as it is the case here.

The `stepwise.selection` function allows to apply a forward stepwise
selection based on the tracer selected, and returns a vector containing
the retained tracers.

``` r
tracers.SW <- fingR::stepwise.selection(data = database,
                                        class = "Class_decontamination",
                                        tracers = tracers$mean.sd_KS,
                                        target = "Target",
                                        # save.dir = dir.example,
                                        # note = "example"
                                        )
```

``` r
tracers.SW
#> [1] "EDXRF_Al_mg.kg.1" "TOC_PrC"          "TN_PrC"
```

Here, the stepwise selection did not remove any of the selected tracers.
However, if the stepwise selection resulted in different tracers,
examining the modelling results for both sets may provide useful
insights. Both tracer selection vectors could be joint in a list as
follows:

``` r
all.tracers <- list("mean.sd_KS" = tracers$mean.sd_KS,
                    "mean.sd_KS_DFA" = tracers.SW)
```

``` r
all.tracers
#> $mean.sd_KS
#> [1] "TOC_PrC"          "TN_PrC"           "EDXRF_Al_mg.kg.1"
#> 
#> $mean.sd_KS_DFA
#> [1] "EDXRF_Al_mg.kg.1" "TOC_PrC"          "TN_PrC"
```

## 3 - Source Contribution Modelling

### 3.1 - Virtual Mixtures Generation

To evaluate the accuracy of unmixing models, virtual mixtures could be
used as a reliable alternative to laboratory mixtures ([Batista et al.,
2022](https://link.springer.com/10.1007/s11368-022-03157-4)). These
artificial mixtures (virtual or laboratory made) act as target samples
with known contributions, enabling for the calculation of modelling
accuracy metrics. In addition, the ease of virtual mixture generation
allows to explore a wide range of source contribution combinations.

The `VM.contrib.generator` function allows a combination of source
contributions to be generated. The contributions are designed to range
from a minimum value (`min`) to a maximum value (`max`) by a specified
step (`step`). The function first creates a contribution sequence for
the first group of sources, from `min` to `max` according to the `step`
(e.g. `min = 0`, `max = 100`, `step = 5`: 0, 5, 10, … 100). The
following sources are then defined as the remaining maximum after the
specified step. The contributions can be generated as a percentage
(e.g. `min = 0` and `max= 100`) or as a ratio (e.g. `min = 0` and
`max= 1`). A small increment results in a higher number of virtual
mixtures. For example, three sources with 5 % increment (`min = 0`,
`max = 100`, `step = 5`) generated 231 contribution combinations, and
5151 contribution combinations with 1 % increment (`step = 1`).

The contribution combination data frame is saved
(’VM_contributions\[\_note\]\[number of virtual mixtures\].csv’) in the
save directory specified by the `save.dir` argument.

``` r
# Generate virtual mixture source contributions
VM.contrib <- fingR::VM.contrib.generator(n.sources = 3,
                                          min = 0,   # Optional
                                          max = 100, # Optional
                                          step = 5,  # Optional
                                          sources.class = c("Forest", "Subsoil", "Undecontaminated"), # Optional
                                          save.dir = dir.example,                                     # Optional
                                          # note = "example"
                                          # VM.name = "Sample_name",
                                          # fileEncoding = "latin1",
                                          # return = TRUE,
                                          # save = TRUE
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

The property values for each virtual mixture are generated based on the
source group property signatures (i.e. mean values). The property values
are calculated as a simple mass balance, the mean value of each source
group is multiplied by the theoretical contribution of each source group
and the result is summed, as detailed in the following equation:

<p align="center">

$p = \sum_{i=1}^{n} P_{Si}~*~C_{Si}$
</p>

Where $p$ is the property value of a virtual mixture, $P_{Si}$ the mean
value of the property for the source group $i$, $C_{Si}$ the theoretical
contribution of the source group $i$ of the virtual mixture, and $n$ the
number of source groups.

The `VM.builder` function allows the calculation of the tracers values
(`tracers`) of the virtual mixtures generated with
`VM.contrib.generator` according to their respective theoretical source
contribution combinations (`contributions`). The `VM.builder` function
returns and, if `save.dir` is set, saves three data frames:

- `$property` - ’VM_properties\[\_note\].csv’: contains the values of
  the tracers for each virtual mixtures.

- `$uncertainty` - ’VM_properties_SD\[\_note\].csv’: contains the
  uncertainty of the tracers. If the actual measurement uncertainty are
  provided (`uncertainty`) they will be used, otherwise 5 % of the
  tracer values will be considered as measurement uncertainty.

- `$full` - ’VM_properties_full\[\_note\].csv’: contains the values of
  the tracers and their measurement uncertainty.

It is possible to add the property values data frame of the source
samples to the one of the virtual mixture by setting
`add.sources = TRUE`, which could be required when running unmixing
models.

``` r
VM <- fingR::VM.builder(data = database,
                        material = "Material",
                        source.name = "Source",
                        class = "Class_decontamination",
                        tracers = tracers$mean.sd_KS,
                        uncertainty = unname(prop.uncertainties[tracers$mean.sd_KS]),  # easy selection of uncertainty labels
                        contributions = VM.contrib,
                        VM.name = "Sample_name",
                        add.sources = TRUE,  # Add source information at the end of the VM data frames
                        save.dir = dir.example,
                        # note = "example"
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

It is also possible to generate virtual mixture contributions and tracer
values in one step in the `VM.builder` function, by setting the
contribution range as a vector (`VM.range`) and the step (`VM.step`),
such as below:

``` r
VM <- fingR::VM.builder(data = database,
                        material = "Material",
                        source.name = "Source",
                        class = "Class_decontamination",
                        tracers = tracers$mean.sd_KS,
                        uncertainty = unname(prop.uncertainties[tracers$mean.sd_KS]),
                        VM.range = c(0, 100), # specify the range
                        VM.step = 5, # specify the step
                        VM.name = "Sample_name",
                        add.sources = TRUE,
                        save.dir = dir.example,
                        # note = "example"
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

### 3.2 - Un-mixing models

Running the models will generate numerous files, therefore we
recommended to create a dedicated folder to organise them, and keep the
directory with `dir.modelling`.

``` r
dir.create(file.path(dir.example, "Modelling/"), showWarnings = FALSE)
dir.modelling <- paste0(dir.example, "Modelling/")
```

### 3.2.1 - Bayesian Mean Model (BMM)

Similarly, create a dedicated folder for Bayesian Mean modelling (BMM),
and keep the directory with `dir.mod.BMM.`

``` r
dir.create(file.path(dir.modelling, "BMM/"), showWarnings = FALSE)
dir.mod.BMM <- paste0(dir.modelling, "BMM/")
```

The Bayesian Mean Model (BMM) is a statistical approach that uses
Bayesian inference to estimate the mean of a population or dataset
([Laceby and Olley,
2015](https://onlinelibrary.wiley.com/doi/10.1002/hyp.10287);[Batista et
al., 2019](http://link.springer.com/10.1007/s11368-018-2199-5)). This
approach incorporates prior knowledge and observed data to calculate a
posterior distribution for the mean, providing a probabilistic framework
that accounts for uncertainty in the estimates. The BMM approach that
minimises the sum of square residuals of the mixing equation for each
iteration of a Monte Carlo simulation.

### 3.2.1.a - Run BMM

The `run.BMM` function has been developed to allow to simplify the use
of the BMM model. The number of iterations should be set in `n.iter`,
between 2500 and 7500, to ensure convergence of the Monte Carlo chain.

> Setting `n.iter` to 30 allows the structure to be tested before the
> actual modelling is carried out.

Of note, it is not compulsory to provide the actual measurement
uncertainty of tracers to `uncertainty` argument, although, it is
hilghly recommended.

The function returns a data frame containing the individual iteration
source contributions for each mixture and, if `save.dir` is set, saves
it as ’BMM_prevision\[\_note\].csv

``` r

BMM.mix <- fingR::run.BMM(data = database,
                          class = "Class_decontamination",
                          mixture = "Target",
                          sample.id = "Sample_name",
                          tracers = tracers$mean.sd_KS,
                          uncertainty = unname(prop.uncertainties[tracers$mean.sd_KS]),
                          n.iter = 30, # test version
                          save.dir = dir.mod.BMM,
                          #note = "example"
                          )
```

When dealing with isotopic ratios, which are non-linear properties, the
residuals of the mixing equation for each iteration should be calculated
taking into account the relative content of the related property (see
[Laceby and Olley
(2015)](https://onlinelibrary.wiley.com/doi/10.1002/hyp.10287) for
further explanation). Several isotopic ratios could be used, bearing in
mind that the order must be identical between `isotope.ratio`,
isotope.prop and `isotopes.unc`. For example, when using the
δ<sup>13\<\>C isotopic ratio in organic matter, the run.BMM function
should be set as follows:

``` r

BMM.mix.iso <- fingR::run.BMM(data = database,
                              class = "Class_decontamination",
                              mixture = "Target",
                              sample.id = "Sample_name",
                              tracers = tracers$mean.sd_KS,
                              uncertainty = unname(prop.uncertainties[tracers$mean.sd_KS]),
                              isotope.ratio = c("d13C_PrM"),                             # Optional: Character vector containing isotopic ratios
                              isotope.prop = c("TOC_PrC"),                               # Optional: Character vector containing isotopic ratios respective properties
                              isotopes.unc = c("d13C_SD"),                               # Optional: Character vecotr containing uncertainty of the isotopic ratios
                              n.iter = 30, # test version
                              save.dir = dir.mod.BMM,
                              #note = "example"
                              )
```

### 3.2.1.b - Collect and organise BMM contributions

Then, the predictions resulting from the BMM must be processed to
determine the contribution values for each target mixture.

The `BMM.summary` function provides a summary of the predictions,
including mean value, standard deviation, and various quantiles (2.5, 5,
25, 50, 75, 95, 97.5) for each target mixture (saves
’BMM_contrib\[\_note\].csv’). From this summary, the `BMM.pred` function
extracts the median and/or mean, `stats = "Median"`, `stats = "Mean"`,
or `stats = c("Median", "Mean")`, value of source contributions for each
target mixture (saves ’BMM_ordered_contrib\[\_note\].csv’).

Finally, the `ensure.total` function ensures that the total of the
predicted source contributions from all sources sum to 1 or 100 % (saves
’corrected_contrib\[\_note\].csv’).

``` r
# Summarise BMM model previsions
BMM.summary.mix <- fingR::BMM.summary(pred = BMM.mix,
                                      #sample.id = "mix.names",
                                      #source = "source",
                                      #value = "value",
                                      save.dir = dir.mod.BMM,
                                      #note = "example"
                                      )

# Extracts the median value of the previsions
BMM.preds.mix <- fingR::BMM.pred(data = BMM.summary.mix,
                                 stats = "Median",
                                 #sample.id = "mix.names",
                                 #source = "source",
                                 save.dir = dir.mod.BMM,
                                 #note = "example" 
                                 )

# Ensure that the total predicted contribution sums to 1 or to 100%
BMM.preds.mixE <- fingR::ensure.total(data = BMM.preds.mix,
                                      sample.name = "mix.names",
                                      path = dir.mod.BMM,
                                      #note = "example"
                                      )
```

``` r
BMM.preds.mixE[1:5,]
#>           mix.names Median_Forest Median_Subsoil Median_Undecontaminated total
#> 1 ManoDd_2106_00-01         0.001          0.751                   0.248     1
#> 2 ManoDd_2106_01-02         0.014          0.985                   0.001     1
#> 3 ManoDd_2106_02-03         0.038          0.743                   0.219     1
#> 4 ManoDd_2106_03-04         0.105          0.720                   0.175     1
#> 5 ManoDd_2106_04-05         0.001          0.646                   0.353     1
```

The same sequence of functions is applied for the virtual mixture
predictions, resulting in `BMM.summary.VM` from `BMM.summary`,
`BMM.preds.VM` from `BMM.pred`, and `BMM.pred.VME` from `ensure.total`.

``` r

BMM.VM <- fingR::run.BMM(data = VM$full,
                         class = "Class_decontamination",
                         mixture = "Virtual Mixture",
                         sample.id = "Sample_name",
                         tracers = tracers$mean.sd_KS,
                         uncertainty = unname(prop.uncertainties[tracers$mean.sd_KS]),
                         n.iter = 30, # test version
                         save.dir = dir.mod.BMM,
                         note = "VM"
                         )
```

``` r
# Summarise BMM model previsions
BMM.summary.VM <- fingR::BMM.summary(pred = BMM.VM,
                                      #sample.id = "mix.names",
                                      #source = "source",
                                      #value = "value",
                                      save.dir = dir.mod.BMM,
                                      #note = "example"
                                      )

# Extracts the median value of the previsions
BMM.preds.VM <- fingR::BMM.pred(data = BMM.summary.VM,
                                 stats = "Median",
                                 #sample.id = "mix.names",
                                 #source = "source",
                                 save.dir = dir.mod.BMM,
                                 #note = "example" 
                                 )

# Ensure that the total predicted contribution sums to 1 or to 100%
BMM.pred.VME <- fingR::ensure.total(data = BMM.preds.VM,
                                      sample.name = "mix.names",
                                      path = dir.mod.BMM,
                                      #note = "example"
                                      )
```

``` r
BMM.pred.VME[1:5,]
#>   mix.names Median_Forest Median_Subsoil Median_Undecontaminated total
#> 1    VM-001         0.007          0.664                   0.329     1
#> 2    VM-002         0.001          0.899                   0.100     1
#> 3    VM-003         0.001          0.864                   0.135     1
#> 4    VM-004         0.001          0.998                   0.001     1
#> 5    VM-005         0.063          0.936                   0.001     1
```

### 3.2.2 - MixSIAR model

Create a dedicated folder for MixSIAR modelling, and keep the directory
with `dir.mod.MixSIAR`.

``` r
dir.create(file.path(dir.modelling, "MixSIAR/"), showWarnings = FALSE)
dir.mod.MixSIAR <- paste0(dir.modelling, "MixSIAR/")
```

The MixSIAR package is designed to create and run Bayesian mixing models
using Just Another Gibbs Sampler (JAGS). This package is widely used in
the sediment source fingerprinting community to predict source
contribution. To explore more about MixSIAR, including detailed
tutorials, examples, and technical documentation, please visit the
official MixSIAR website. Additionally, the source code and further
resources can be found on the MixSIAR GitHub page
(<https://github.com/brianstock/MixSIAR/tree/3.1.9>).

According to MixSIAR guide, installation should follow these steps:

- Download and install/update R (<https://cran.r-project.org/bin/>).

- Download and install JAGS (<https://mcmc-jags.sourceforge.io/>).

- Open R and run:

``` r
devtools::install.packages("MixSIAR", dependencies = TRUE)
```

Or install the GitHub version:

``` r
# install.packages(remotes)
remotes::install_github("brianstock/MixSIAR", dependencies = TRUE)
```

- Load the `MixSIAR` package:

``` r
library(MixSIAR)
```

### 3.2.2.a - Generate data for MixSIAR

To MixSIAR model requires data to be formated in a specific format to
load the information of source and mixture samples. The
`data.for.MixSIAR` function generates CSV files that conform to the
format required by MixSIAR loading functions (i.e. `load_mix_data` ,
`load_source_data` , and `load_discr_data` ). The function generates
three files with the properties according to the specified tracer
selection (in `tracers` argument):

- ’MixSIAR_mix\[\_note\].csv’: containing mixtures information.

- ’MixSIAR_sources\[\_note\].csv’: containing the mean and standard
  deviation (sd) of the source classes.

- ’MixSIAR_discrimination\[\_note\].csv’: containing is a matrix of zero
  as there is no trophic information in sediment source fingerprinting
  studies.

``` r
# Actual sediment samples
fingR::data.for.MixSIAR(data = database,
                        class = "Class_decontamination",
                        target = "Target",
                        tracers = tracers$mean.sd_KS,
                        sample.name = "Sample_name",
                        save.dir = dir.mod.MixSIAR,
                        # note = "exemple",
                        # fileEncoding = "latin1",
                        # show.data = FALSE,
                        )

# Virtual Mixtures
fingR::data.for.MixSIAR(data = VM$full,
                        class = "Class_decontamination",
                        target = "Virtual Mixture",
                        tracers = tracers$mean.sd_KS,
                        sample.name = "Sample_name",
                        save.dir = dir.mod.MixSIAR,
                        note = "VM",
                        # fileEncoding = "latin1",
                        # show.data = FALSE,
                        )
```

### 3.2.2.b - Load mixture, source and discrimination data

The MixSIAR functions `load_mix_data`, `load_source_data`, and
`load_discr_data` are used to load the corresponding CSV files for the
actual mixtures: ’MixSIAR_mix.csv’ (actual mixtures),
’MixSIAR_sources.csv’ (sources), and ’MixSIAR_discrimination.csv’
(discrimination matrix) that were generated previously.

``` r
# Load sediment samples data
MixSIAR.mix <- MixSIAR::load_mix_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_mix.csv"),
                                      iso_names = tracers$mean.sd_KS,
                                      factors = c("Sample_name"),
                                      fac_random = FALSE,
                                      cont_effects = NULL)

# Load source data
MixSIAR.source <- MixSIAR::load_source_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_sources.csv"),
                                            source_factors = NULL,
                                            conc_dep = FALSE,
                                            data_type = "means",
                                            mix = MixSIAR.mix)

# Load discrimination data
MixSIAR.discr <- MixSIAR::load_discr_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_discrimination.csv"),
                                          mix = MixSIAR.mix)
```

The same functions are used to load the virtual mixtures information.
The virtual mixtures properties (’MixSIAR_mix_VM.csv’) are loaded using
`load_mix_data` and the same source and discrimination matrix files as
for the actual mixtures.

``` r
# Load virtual mixtures data
MixSIAR.VM <- MixSIAR::load_mix_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_mix_VM.csv"),
                                     iso_names = tracers$mean.sd_KS,
                                     factors = c("Sample_name"),
                                     fac_random = FALSE,
                                     cont_effects = NULL)


# Load source data
MixSIAR.source.VM <- MixSIAR::load_source_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_sources_VM.csv"),
                                               source_factors = NULL,
                                               conc_dep = FALSE,
                                               data_type = "means",
                                               mix = MixSIAR.VM)

# Load discrimination data
MixSIAR.discr.VM <- MixSIAR::load_discr_data(filename = paste0(dir.mod.MixSIAR, "MixSIAR_discrimination_VM.csv"),
                                             mix = MixSIAR.mix)
```

### 3.2.2.c - Write JAGS model file

The “JAGS is Just Another Gibbs Sampler. It is a program for analysis of
Bayesian hierarchical models using Markov Chain Monte Carlo (MCMC)
simulation not wholly” used in MixSIAR package ([Stock et al.,
2022](https://doi.org/10.5281/zenodo.1209993)). The `write_JAGS_model`
writes the JAGS file to define the structure of the Bayesian model. The
JAGS model is saved at the path set in `filename` and using the
specified txt file name (e.g. ’MixSIAR_model.txt’).

``` r
# Write JAGS model file for actual samples
MixSIAR::write_JAGS_model(filename = paste0(dir.mod.MixSIAR, "MixSIAR_model.txt"),
                          resid_err = FALSE,
                          process_err = TRUE,
                          mix = MixSIAR.mix,
                          source = MixSIAR.source)
```

Another JAGS model is written for virtual mixtures since the the mixture
samples and sources are set in `mix` and `source`.

``` r
# Write JAGS model file for virtual mixtures
MixSIAR::write_JAGS_model(filename = paste0(dir.mod.MixSIAR, "MixSIAR_model_VM.txt"),
                          resid_err = FALSE,
                          process_err = TRUE,
                          mix = MixSIAR.VM,
                          source = MixSIAR.source.VM)
```

### 3.2.2.d - Run MixSIAR

When running MixSIAR model you must choose one of the [MCMC run
option](https://brianstock.github.io/MixSIAR/articles/wolves_ex.html#run-model)
[(Stock et al., 2020)](https://doi.org/10.5281/zenodo.1209993)

<p align="center">

| run ==       | Chain Length | Burn-in   | Thin | \# Chains |
|--------------|--------------|-----------|------|-----------|
| “test”       | 1,000        | 500       | 1    | 3         |
| “very short” | 10,000       | 5,000     | 5    | 3         |
| “short”      | 50,000       | 25,000    | 25   | 3         |
| “normal”     | 100,000      | 50,000    | 50   | 3         |
| “long”       | 300,000      | 200,000   | 100  | 3         |
| “very long”  | 1,000,000    | 500,000   | 500  | 3         |
| “extreme”    | 3,000,000    | 1,500,000 | 500  | 3         |

</p>

In this example MCMC is set to “test”, which allows to quickly ensures
that the code implemented is functional.

> if “Error: .onload … ‘rgags’ -\> it’s because R version is too old
> need at least R.2.2

``` r
# Run MixSIAR model for sediment samples
jags.mix <- MixSIAR::run_model(run = "test",
                               mix = MixSIAR.mix,
                               source = MixSIAR.source,
                               discr = MixSIAR.discr,
                               model_filename = paste0(dir.mod.MixSIAR, "MixSIAR_model.txt")
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
# Run MixSIAR model for sediment samples
jags.VM <- MixSIAR::run_model(run = "test",
                              mix = MixSIAR.VM,
                              source = MixSIAR.source.VM,
                              discr = MixSIAR.discr.VM,
                              model_filename = paste0(dir.mod.MixSIAR, "MixSIAR_model_VM.txt")
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

### 3.2.2.e - Collect and organise MixSIAR contributions

Similar to the BMM modelling, the predictions from the MixSIAR must be
processed to determine the contribution values for each target mixture.

Conversely to `fingR::run.BMM` function, the `MixSIAR::run_model`
function does not save the Bayesian previsions directly. Therefore the
`JAGS.summary` function saves the previsions
(’JAGS_prevision\[\_note\].csv’) and provides a summary of the
predictions, including mean value, standard deviation, and various
quantiles (2.5, 5, 25, 50, 75, 95, 97.5) for each target mixture (saves
’JAGS_contrib\[\_note\].csv’). From this summary, the `JAGS.pred`
function extracts the median and/or mean, `stats = "Median"`,
`stats = "Mean"`, or `stats = c("Median", "Mean")`, value of source
contributions for each target mixture (saves
’JAGS_ordered_contrib\[\_note\].csv’). Finally, the ensure.total
function ensures that the total of the predicted source contributions
from all sources sum to 1 or 100 % (saves
’corrected_contrib\[\_note\].csv’).

``` r
## Summarise MixSIAR model previsions
MixSIAR.summary.mix <- fingR::JAGS.summary(jags.1 = jags.mix,
                                           mix = MixSIAR.mix,
                                           sources = MixSIAR.source,
                                           path = dir.mod.MixSIAR,
                                           #note = "example",
                                           save_pred = TRUE
                                           )

## Extracts the median value of the previsions
MixSIAR.preds.mix <- fingR::JAGS.pred(path = paste0(dir.mod.MixSIAR, "JAGS_contrib.csv"),
                                      stats = "Median",
                                      save = TRUE,
                                      #note = "example"
                                      )

## Ensure that the total predicted contribution sums to 1 or 100%
MixSIAR.preds.mixE <- fingR::ensure.total(data = MixSIAR.preds.mix,
                                          sample.name = "sample",
                                          path = dir.mod.MixSIAR,
                                          #note = "example"
                                          )
```

``` r
MixSIAR.preds.mixE[1:5,]
#>              sample Median_Forest Median_Subsoil Median_Undecontaminated total
#> 1 ManoDd_2106_00-01         0.179          0.020                   0.801 1.000
#> 2 ManoDd_2106_01-02         0.058          0.013                   0.929 0.999
#> 3 ManoDd_2106_02-03         0.059          0.017                   0.924 1.000
#> 4 ManoDd_2106_03-04         0.067          0.021                   0.912 0.999
#> 5 ManoDd_2106_04-05         0.065          0.016                   0.919 0.999
```

The same sequence of functions is used for the virtual mixture
predictions, resulting in `MixSIAR.summary.VM` from `JAGS.summary`,
`MixSIAR.preds.VM` from `JAGS.pred`, and `MixSIAR.pred.VME` from
`ensure.total`.

### 3.3 - Modelling accuracy statistics calculation

The accuracy of unmixing models is evaluated by comparing the
theoretical source contribution values (`VM.contrib`), representing the
known contributions of the virtual mixtures, with the predicted
contributions of the virtual mixtures (`BMM.preds.VM`) generated by the
unmixing model.

Several criteria can be used to evaluate the accuracy of the
predictions:

- **Uncertainty**: Assessed using prediction interval widths (e.g., W50,
  W95; for Bayesian models).

- **Residual Error** or **Bias**: Evaluated using metrics such as mean
  error (ME).

- **Performance**: Measured using metrics such as the squared Pearson
  correlation coefficient (r<sup>2</sup>), root-mean-square error
  (RMSE), Nash-Sutcliffe modeling efficiency coefficient (NSE), and
  continuous ranked probability score (CRPS; for Bayesian models).

Modelling accuracy statistics could be interpreted the following way
([Chalaux-Clergue et al.,
2024a](https://soil.copernicus.org/articles/10/109/2024/)): “Higher
values of W50 indicate a wider distribution, which is related to a
higher uncertainty. The sign of the ME indicates the direction of the
bias, i.e. an overestimation or underestimation (positive or negative
value, respectively). As ME is affected by cancellation, a ME of zero
can also reflect a balanced distribution of predictions around the 1:1
line. Although this is not a bias, it does not mean that the model
outputs are devoid of errors. The RMSE is a measure of the accuracy and
allows us to calculate prediction errors of different models for a
particular dataset. RMSE is always positive, and its ideal value is
zero, which indicates a perfect fit to the data. As RMSE depends on the
squared error, it is sensitive to outliers. The r<sup>2</sup> describes
how linear the prediction is. The NSE indicates the magnitude of
variance explained by the model, i.e. how well the predictions match
with the observations. A negative RMSE indicates that the mean of the
measured values provides a better predictor than the model. The joint
use of r<sup>2</sup> and NSE allows for a better appreciation of the
distribution shape of predictions and thus facilitates the understanding
of the nature of model prediction errors. The CRPS evaluates both the
accuracy and sharpness (i.e. precision) of a distribution of predicted
continuous values from a probabilistic model for each sample ([Matheson
and Winkler,
1976](http://pubsonline.informs.org/doi/10.1287/mnsc.22.10.1087)). The
CRPS is minimised when the observed value corresponds to a high
probability value in the distribution of model outputs.”

To illustrate, only predictions made using BMM are used, however, it
also applies to predictions made using MixSIAR.

### 3.3.1 - General accuracy metrics

The `eval.groups` function calculates general accuracy metrics (ME,
RMSE, r<sup>2</sup>, NSE) by comparing the theoretical source
contribution values (provided in `df.obs`) with the predicted values
(provided in `df.pred`) (saves ’stats\[\_note\].csv’). In df.obs , the
virtual mixtures are identified by the column Sample_name , while in
`df.pred`, they are identified by the column `mix.names`, which is
specified in `by` (The resulting data frame is saved as
’ObsPred\[\_note\].csv’).

``` r
BMM.stats <- fingR::eval.groups(df.obs = VM.contrib, # Theoretical contribution
                                df.pred = BMM.pred.VME %>%
                                  dplyr::select(-total), # remove the $total column from ensured data.frame
                                by = c("Sample_name" = "mix.names"),
                                path = dir.mod.BMM,
                                #note = "example" 
                                )
```

``` r
BMM.stats
#>     Type           Source    ME RMSE   r2   NSE
#> 1 Median           Forest -0.16 0.24 0.57  0.11
#> 2 Median          Subsoil  0.40 0.47 0.37 -2.52
#> 3 Median Undecontaminated -0.24 0.38 0.00 -1.25
```

### 3.3.2 - Continuous ranked probability score

The `CRPS` function calculates the continuous ranked probability score
(CRPS). The predicted values of the virtual mixture (`prev`) are the
previsions for each sample from the Bayesian models. Since a large
number of previsions are made when running the Bayesian models, it is
possible to specify the path to the file containing the previsions
rather than loading them into the workspace in `path.to.prev`
(previously saved by `BMM.summary`)

The function returns and, if `save.path` is set, saves two data frames
at the `save.dir` direction or if `save = TRUE` is set, saves them at
the direction indicated by `path.to.prev`:

- `$samples` - ’CRPS\[\_note\].csv’: contains the CRPS values for each
  source group for each virtual mixtures.

- `$mean` - ’CRPS\[\_note\]\_mean.csv’: contains the values of the mean
  CRPS value per contribution source class group for each virtual
  mixtures.

``` r
# Calculate prediction CRPS values
BMM.CRPS <- fingR::CRPS(obs = VM.contrib,
                        prev = read.csv(paste0(dir.mod.BMM, "BMM_prevision_VM.csv")),
                        source.groups = c("Forest", "Subsoil", "Undecontaminated"),
                        mean.cal = TRUE,
                        # save = TRUE,
                        save.dir = dir.mod.BMM,
                        #note = "example"
                        )
#> Lade nötiges Paket: scoringRules
```

``` r
BMM.CRPS$samples[1:5,]
#>   Sample_name Forest Subsoil Undecontaminated
#> 1      VM-001 0.0641  0.2921           0.5433
#> 2      VM-002 0.0543  0.3606           0.5966
#> 3      VM-003 0.0263  0.3004           0.4377
#> 4      VM-004 0.0377  0.4220           0.6046
#> 5      VM-005 0.0517  0.2912           0.4933
```

``` r
BMM.CRPS$mean
#>             Source CRPS.mean
#> 1           Forest    0.1377
#> 2          Subsoil    0.1672
#> 3 Undecontaminated    0.1889
```

### 3.3.3 - Prediction interval width

The `interval.width` function calculates two prediction interval widths:
W50 and W95. The W50 contains 50 % of the previsions (Q75-Q25) and the
W95 contains 95 % of the previsions (Q97.5-Q2.5). Since the interval
width is only calculated on the Bayesian model previsions, it is
possible to calculate them on actual and virtual mixtures. Similarly to
`CRPS`, it is possible to specify the path to the file containing the
Bayesian model previsions, rather than loading them into the workspace
with `path.to.prev` (previously saved by `BMM.summary`).

The function returns and, if `save.path` is set, saves two data frames
at the `save.dir` direction or if `save = TRUE` is set, saves them at
the direction indicated by `path.to.prev`:

- `$samples` - ’Interval_width\[\_note\].csv’: contains both interval
  width values for each source group for each virtual mixtures.

- `$mean` - ’Interval_width_mean\[\_note\].csv’: contains the mean of
  both interval width values per contribution source class group for
  each virtual mixtures.

``` r
# Calculate prediction interval width (W95, W50)
BMM.predWidth <- fingR::interval.width(path.to.prev = paste0(dir.mod.BMM, "BMM_prevision_VM.csv"),
                                       mean.cal = TRUE,
                                       save = TRUE,
                                       #note = "exemple"
                                       )
```

``` r
BMM.predWidth$samples[1:6,]
#>   mix.names           source   W50   W95
#> 1    VM-001           Forest 0.282 0.997
#> 2    VM-001          Subsoil 0.573 0.997
#> 3    VM-001 Undecontaminated 0.493 0.854
#> 4    VM-002           Forest 0.339 0.826
#> 5    VM-002          Subsoil 0.664 0.997
#> 6    VM-002 Undecontaminated 0.249 0.988
```

``` r
BMM.predWidth$mean
#> # A tibble: 3 × 3
#>   Source           W50.mean W95.mean
#>   <chr>               <dbl>    <dbl>
#> 1 Forest              0.35     0.885
#> 2 Subsoil             0.584    0.966
#> 3 Undecontaminated    0.397    0.93
```

### 3.3.4 - Encompassed sample predictions

The `ESP` function calculates the Encompassed Sample Prediction (ESP).
The ESP is a newly introduced statistics in [Chalaux-Clergue et
al. (2024d)](https://linkinghub.elsevier.com/retrieve/pii/S0048969724046941)
and was created to assess the transferability of the statistics
calculated on virtual mixtures to actual sediment samples. The ESP was
calculated as the percentage of actual samples for which the predicted
contributions remained within the lowest and the highest predicted
contributions obtained for the virtual mixtures. When expressed as a
percentage, ESP ranges from 0 to 100 %, the latter providing an optimal
value. Values close to 100 % indicate a higher transferability of
modelling evaluation statistics calculated on virtual mixture to actual
sediment samples.

``` r
sources.lvl <- c("Forest", "Subsoil", "Undecontaminated")

# Calculate encompassed sample predictions (ESP)
BMM.ESP <- fingR::ESP(obs = BMM.preds.VM,                           # Virtual mixtures predicted contributions
                          pred = BMM.preds.mixE,                    # Actual sediment samples predicted contributions
                          sources = paste0("Median_", sources.lvl), # Sources labels in prediction objects
                          count = "Both"                            # Count 'Number' and 'Percentage'
                          )
```

``` r
BMM.ESP
#>                                   Source ESP.Number ESP.Percentage
#> Median_Forest                     Forest         34             89
#> Median_Subsoil                   Subsoil         38            100
#> Median_Undecontaminated Undecontaminated         19             50
```

# Future updates

The upcoming version 2.3.0 update will introduce the Python version of
fingR, making the sediment source fingerprinting tools available to a
broader community.

*Graphical support functions such as Bayesian prediction density plots,
prediction vs. observation plots, and ternary diagrams are under
development.*

# Getting help

If you encounter a clear bug, please open an issue or send an email to
[Thomas Chalaux-Clergue](mailto:thomaschalaux@icloud.com).

# Citation

To cite this packages:

``` r
utils::citation(package = "fingR")
#> To cite the 'fingR' package in publications please use:
#> 
#>   Chalaux-Clergue et al. (2025). fingR: A package to support sediment
#>   source fingerprinting studies, Zenodo [Package]:
#>   https://doi.org/10.5281/zenodo.8293595, Github [Package]:
#>   https://github.com/tchalauxclergue/fingR, Version = 2.1.3.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {fingR: A support to sediment source fingerprinting studies},
#>     author = {{Chalaux-Clergue} and {Thomas} and {Bizeul} and {Rémi} and {Amaury} and {Bardelle}},
#>     year = {2025},
#>     month = {7},
#>     note = {R package version 2.1.3},
#>     doi = {https://doi.org/10.5281/zenodo.8293595},
#>     url = {https://github.com/tchalauxclergue/fingR},
#>   }
```

# References

- Batista, P. V. G., Laceby, J. P., & Evrard, O. (2022). How to evaluate
  sediment fingerprinting source apportionments. Journal of Soils and
  Sediments, 22(4), 1315–1328.
  <https://doi.org/10.1007/s11368-022-03157-4>

- Batista, P. V. G., Laceby, J. P., Silva, M. L. N., Tassinari, D.,
  Bispo, D. F. A., Curi, N., Davies, J., & Quinton, J. N. (2019). Using
  pedological knowledge to improve sediment source apportionment in
  tropical environments. Journal of Soils and Sediments, 19(9),
  3274–3289. <https://doi.org/10.1007/s11368-018-2199-5>

- Chalaux-Clergue, T., Bizeul, R., Batista, P. V. G., Martínez-Carreras,
  N., Laceby, J. P., & Evrard, O. (2024a). Sensitivity of source
  sediment fingerprinting to tracer selection methods. SOIL, 10(1),
  109–138. <https://doi.org/10.5194/soil-10-109-2024>

- Chalaux-Clergue, T., Bizeul, R., Foucher, A., & Evrard, O. (2024b). A
  unified template for sediment source fingerprinting databases (Version
  24.03.01) \[Data set\]. \[object Object\].
  <https://doi.org/10.5281/ZENODO.10725788>

- Chalaux-Clergue, T., Evrard, O., Durand, R., Caumon, A., Hayashi, S.,
  Tsuji, H., Huon, S., Vaury, V., Wakiyama, Y., Nakao, A., Laceby, J.
  P., & Onda, Y. (2022c). Organic matter, geochemical and colorimetric
  properties of potential source material, target sediment and
  laboratory mixtures for conducting sediment fingerprinting approaches
  in the Mano Dam Reservoir (Hayama Lake) catchment, Fukushima
  Prefecture, Japan. (Version 1) \[Data set\]. Zenodo.
  <https://doi.org/10.5281/ZENODO.7081094>

- Chalaux-Clergue, T., Foucher, A., Chaboche, P.-A., Hayashi, S., Tsuji,
  H., Wakiyama, Y., Huon, S., Vandromme, R., Cerdan, O., Nakao, A., &
  Evrard, O. (2024d). Impacts of farmland decontamination on 137Cs
  transfers in rivers after Fukushima nuclear accident: Evidence from a
  retrospective sediment core study. Science of The Total Environment,
  947, 174546. <https://doi.org/10.1016/j.scitotenv.2024.174546>

- Evrard, O., Batista, P. V. G., Company, J., Dabrin, A., Foucher, A.,
  Frankl, A., García-Comendador, J., Huguet, A., Lake, N., Lizaga, I.,
  Martínez‑Carreras, N., Navratil, O., Pignol, C., & Sellier, V. (2022).
  Improving the design and implementation of sediment fingerprinting
  studies: Summary and outcomes of the TRACING 2021 Scientific School.
  Journal of Soils and Sediments, 22(6), 1648–1661.
  <https://doi.org/10.1007/s11368-022-03203-1>

- Laceby, J. P., & Olley, J. (2015). An examination of geochemical
  modelling approaches to tracing sediment sources incorporating
  distribution mixing and elemental correlations. Hydrological
  Processes, 29(6), 1669–1685. <https://doi.org/10.1002/hyp.10287>

- Martínez-Carreras, N., Gallart, F., Iffly, J. F., Pfister, L.,
  Walling, D. E., & Krein, A. (2008). Uncertainty assessment in
  suspended sediment fingerprinting based on tracer mixing models: A
  case study from Luxembourg. IAHS Publication, 325, 94.

- Matheson, J. E., & Winkler, R. L. (1976). Scoring Rules for Continuous
  Probability Distributions. Management Science, 22(10), 1087–1096.
  <https://doi.org/10.1287/mnsc.22.10.1087>

- Sherriff, S. C., Franks, S. W., Rowan, J. S., Fenton, O., &
  Ó’hUallacháin, D. (2015). Uncertainty-based assessment of tracer
  selection, tracer non-conservativeness and multiple solutions in
  sediment fingerprinting using synthetic and field data. Journal of
  Soils and Sediments, 15(10), 2101–2116.
  <https://doi.org/10.1007/s11368-015-1123-5>

- Stock, B. C., Semmens, B. X., Ward, E. J., Parnell, A. C., &
  Phillips, D. L. (2020). MixSIAR: Bayesian Mixing Models in R (Version
  3.1.12) \[Computer software\].
  <https://doi.org/10.5281/zenodo.1209993>

- Stock, B. C., Semmens, B. X., Ward, E. J., Parnell, A. C., &
  Phillips, D. L. (2022). JAGS: Bayesian Mixing Models in R (Version
  4.3.1) \[Computer software\]. <https://doi.org/10.5281/zenodo.1209993>
