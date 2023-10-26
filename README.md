fingR: A support for sediment source fingerprinting studies
================

The fingR package is a package to support sediment source fingerprinting
studies: characterising your dataset, selecting tracers (three-step
method), modelling source contribution (BMM) and assessing the quality
of modelling predictions using virtual mixtures (support BMM and
MixSIAR).

## Table of contents

- [Installation](#installation)
- [Data preparation](#data-preparation)
- [Tracers selection: Three-step method](#tracers-selection-TSM)
  - [Conservativity](#conservativity)
  - [Discriminant power](#discriminant-power)
  - [Selected tracers](#selected-tracers)
  - [Discriminant Function Analysis (DFA) stepwise
    selection](#Discriminant-Function-Analysis-DFA-stepwise-selection)
- [Source contribution modelling](#source-contribution-modelling)
  - [Artificial mixtures](#artificial-mixtures)
  - [Un-mixing model: MixSIAR](#mixsiar)
  - [Un-mixing model: BMM](#bmm)

<!-- tocstop -->
</details>

------------------------------------------------------------------------

# Installation

The `fingR` package is available in this GitLab repository only. The
package is likely to be updated in the future. We recommand to
re-install the package if you didn’t use it for a long time. If any
issue, send an email to [Thomas Chalaux-Clergue and Rémi
Bizeul](mailto:thomaschalaux@icloud.com,%20rbizeul59@gmail.com).

Install the package from a local directory after downloading the tar.gz
file:

``` r
# install the fingR package from a local directory
devtools::install_local("path_to_file/fingR_1.1.0.tar.gz", repos = NULL)
```

or from GitHub (version 1.1.0)

``` r
devtools::install_github("https://github.com/tchalauxclergue/fingR", ref = "master", force = T)
```

``` r
library(fingR)
```

# Data preparation

A example dataset containing 32 layers of a sediment core from Hayama
lake, Fukushima prefecture, Japan, 56 source soil samples, and 27
laboratory mixtures. Three types of source are represented: 24 cropland,
22 forest and 10 subsoil. Various properties were measured: organic
matter, geochemistry and diffuse reflectance. The full dataset and
measurements protocols are available ([Chalaux-Clergue et al.,
2022](https://doi.org/10.5281/zenodo.7081094)).

``` r
load(file = "data/Hayama.rda")

head(Hayama)[, c(1, 3:4, 11:12, 16:18)]
```

    ##         Sample_name   Nature  Class TOC_PrC TN_PrC Al_mg.kg Ca_mg.kg Co_mg.kg
    ## 1 ManoDd_2106_06-07 Sediment Target    4.24   0.34    73133    16796       47
    ## 2 ManoDd_2106_07-08 Sediment Target    5.88   0.45    75033    16789       51
    ## 3 ManoDd_2106_08-09 Sediment Target    6.42   0.49    78200    15931       53
    ## 4 ManoDd_2106_09-10 Sediment Target    7.75   0.53    69378    17918       53
    ## 5 ManoDd_2106_10-11 Sediment Target    7.75   0.55    72169    16251       53
    ## 6 ManoDd_2106_11-12 Sediment Target    7.19   0.53    69818    16247       49

The dataset is composed of source (cropland, forest and subsoil) and
sediment samples (target)

``` r
table(Hayama$Class)
```

    ## 
    ##    Cropland      Forest Lab Mixture     Subsoil      Target 
    ##          24          22          27          10          32

The dataset also contains laboratory mixtures which we won’t be using.

``` r
library(dplyr)

Hayama <- Hayama %>%
  dplyr::filter(Material != "Lab Mixture")

table(Hayama$Class)
```

    ## 
    ## Cropland   Forest  Subsoil   Target 
    ##       24       22       10       32

First, the dataset contains 31 analysed properties. We create a vectors
of these properties values and uncertainty.

``` r
library(dplyr)

# list of property values
prop.values <- Hayama %>% dplyr::select(TOC_PrC:TN_PrC, Al_mg.kg:h_D65, A1:Goethite_525nm_FstD) %>% colnames()

# list of property measurement uncertainty/error
prop.uncertainties <- Hayama %>% dplyr::select(TOC_SD:TN_SD, Al_RMSE:h_D65_SD, A1_uncertainty:Goethite_525nm_FstD_uncertainty) %>% colnames()

prop.values
```

    ##  [1] "TOC_PrC"             "TN_PrC"              "Al_mg.kg"           
    ##  [4] "Ca_mg.kg"            "Co_mg.kg"            "Cr_mg.kg"           
    ##  [7] "Cu_mg.kg"            "Fe_mg.kg"            "K_mg.kg"            
    ## [10] "Mg_mg.kg"            "Mn_mg.kg"            "Ni_mg.kg"           
    ## [13] "Pb_mg.kg"            "Rb_mg.kg"            "Si_mg.kg"           
    ## [16] "Sr_mg.kg"            "Ti_mg.kg"            "Zn_mg.kg"           
    ## [19] "Zr_mg.kg"            "L_star_D65"          "a_star_D65"         
    ## [22] "b_star_D65"          "C_star_D65"          "h_D65"              
    ## [25] "A1"                  "A2"                  "A3"                 
    ## [28] "Gt"                  "Q7.4"                "Goethite_445nm_FstD"
    ## [31] "Goethite_525nm_FstD"

``` r
prop.uncertainties
```

    ##  [1] "TOC_SD"                          "TN_SD"                          
    ##  [3] "Al_RMSE"                         "Ca_RMSE"                        
    ##  [5] "Co_RMSE"                         "Cr_RMSE"                        
    ##  [7] "Cu_RMSE"                         "Fe_RMSE"                        
    ##  [9] "K_RMSE"                          "Mg_RMSE"                        
    ## [11] "Mn_RMSE"                         "Ni_RMSE"                        
    ## [13] "Pb_RMSE"                         "Rb_RMSE"                        
    ## [15] "Si_RMSE"                         "Sr_RMSE"                        
    ## [17] "Ti_RMSE"                         "Zn_RMSE"                        
    ## [19] "Zr_RMSE"                         "L_star_D65_SD"                  
    ## [21] "a_star_D65_SD"                   "b_star_D65_SD"                  
    ## [23] "C_star_D65_SD"                   "h_D65_SD"                       
    ## [25] "A1_uncertainty"                  "A2_uncertainty"                 
    ## [27] "A3_uncertainty"                  "Gt_uncertainty"                 
    ## [29] "Q7.4_uncertainty"                "Goethite_445nm_FstD_uncertainty"
    ## [31] "Goethite_525nm_FstD_uncertainty"

To begin, we check that the relation between uncertainty and sample
values.

``` r
fingR::data.watcher(data = Hayama, properties = prop.values, prop.uncer = prop.uncertainties)
```

    ## 
    ## Following column(s) contain(s) some negative values: Cr_mg.kg, A3.
    ## Following column(s) have a measurement uncertainty that makes some values to be virtually impossible: Co_mg.kg, Cr_mg.kg, Cu_mg.kg, Ni_mg.kg, A3, Goethite_445nm_FstD.
    ## Following column(s) have a relative measurement uncertainty above 5% (up to - number): A3 (max:Inf% - n:20), Goethite_445nm_FstD (max:1086% - n:32), Co_mg.kg (max:775% - n:88), Cr_mg.kg (max:233% - n:50), Cu_mg.kg (max:110% - n:86), Ni_mg.kg (max:105% - n:83), Rb_mg.kg (max:90% - n:67), A2 (max:57% - n:88), Goethite_525nm_FstD (max:57% - n:41), b_star_D65 (max:47% - n:87), C_star_D65 (max:43% - n:2), Pb_mg.kg (max:39% - n:9), a_star_D65 (max:36% - n:16), Zn_mg.kg (max:33% - n:12), L_star_D65 (max:33% - n:12), A1 (max:30% - n:1), TN_PrC (max:29% - n:41), Q7.4 (max:16% - n:47), Gt (max:15% - n:50), Sr_mg.kg (max:14% - n:5), TOC_PrC (max:9% - n:8), h_D65 (max:8% - n:22), Zr_mg.kg (max:7% - n:9).

It can be seen that Co, Cr, Cu, Ni, Rb, A3 and Goethite at 445nm are not
well defined, so we remove from subsequent analysis.

``` r
prop.values <- prop.values[!prop.values %in% c("Co_mg.kg", "Cr_mg.kg", "Cu_mg.kg", "Ni_mg.kg", "Rb_mg.kg", "A3", "Goethite_445nm_FstD")]

prop.uncertainties <- prop.uncertainties[!prop.uncertainties %in% c("Co_RMSE", "Cr_RMSE", "Cu_RMSE", "Ni_RMSE", "Rb_RMSE", "A3_uncertainty", "Goethite_445nm_FstD_uncertainty")]
names(prop.uncertainties) <- prop.values
```

# Tracer selection: Three-step method

### Conservativity

In the three-step method conservativity is assessed using range tests
(RT). The range of sources is defined as the highest and lowest values
of the property among source groups. To be considered as conservative,
all the sample property values should lie within the source range.
Several range test criteria exists in the literature, the most common
are: minimum-maximum (**MM**), minimum-maximum plus/minus 10% (as
measurement error - **MMe**), boxplot **whiskers**, boxplot **hinge**,
**mean**, mean plus/minus one standard deviation (**mean.sd**) and
**median**. To perform all test set criteria to **all**. However, the
different criteria provide a more or less reliable selection of
conservative properties. We recommend to use **mean.sd** and/or
**hinge**. The *range.test* function returns a list of two data.frame: -
*results.df* contains overall result of the range test(s) - *results.RT*
is a data.frame of each target sample property range test results.

``` r
# Conventional methodroach
rt.results <- range.tests(Hayama, # dataset
                          class = "Class", # variable where difference between source class/levels/groups and mixture is made
                          mixture = "Target", # how to identify mixtures in class
                          properties = prop.values, # tested properties
                          sample.id = "Sample_name",
                          criteria = c("mean.sd"), # all range tests: min-max ("MM"), min-max +/- error ("MMe"), whiskers, hinge, mean, mean +/- sd ("mean.sd"), and median or to use all the criteria "all"
                          # MM.error = c(0.1), # to set the minimum-maximum plus/minus error 10% (0.1) as default
                          # save.dir = save.dir.fingR, # path to save the results
                          # note = "fingR" # to add a note at the end of the file name
                          )

rt.results$results.df
```

    ##               Property n_source n_mixture NAs RT_mean.sd_single
    ## 1              TOC_PrC       56        32   0              TRUE
    ## 2               TN_PrC       56        32   0              TRUE
    ## 3             Al_mg.kg       56        32   0              TRUE
    ## 4             Ca_mg.kg       56        32   0             FALSE
    ## 5             Fe_mg.kg       56        32   0             FALSE
    ## 6              K_mg.kg       56        32   0             FALSE
    ## 7             Mg_mg.kg       56        32   0             FALSE
    ## 8             Mn_mg.kg       56        32   0             FALSE
    ## 9             Pb_mg.kg       56        32   0             FALSE
    ## 10            Si_mg.kg       56        32   0             FALSE
    ## 11            Sr_mg.kg       56        32   0             FALSE
    ## 12            Ti_mg.kg       56        32   0              TRUE
    ## 13            Zn_mg.kg       56        32   0             FALSE
    ## 14            Zr_mg.kg       56        32   0             FALSE
    ## 15          L_star_D65       56        32   0              TRUE
    ## 16          a_star_D65       56        32   0             FALSE
    ## 17          b_star_D65       56        32   0              TRUE
    ## 18          C_star_D65       56        32   0              TRUE
    ## 19               h_D65       56        32   0             FALSE
    ## 20                  A1       56        32   0             FALSE
    ## 21                  A2       56        32   0              TRUE
    ## 22                  Gt       56        32   0             FALSE
    ## 23                Q7.4       56        32   0              TRUE
    ## 24 Goethite_525nm_FstD       56        32   0              TRUE

The function *is.conservative* returns a list of the conservative
properties according to each range test criteria.

``` r
prop.cons <- is.conservative(data = rt.results$results.df, # data.frame result.df from fingR::range.tests function or any df with the same organisation
                             property = "Property", # column that contains property names
                             # test.format = "RT", # (default) indicates the common pattern in column test names
                             # 2, # (default) the position of the test name in the column name
                             # "_" # the splitting character in column test names
                             # note = "fingR" # add a note to tests names in the list
                             )
prop.cons
```

    ## $mean.sd
    ##  [1] "TOC_PrC"             "TN_PrC"              "Al_mg.kg"           
    ##  [4] "Ti_mg.kg"            "L_star_D65"          "b_star_D65"         
    ##  [7] "C_star_D65"          "A2"                  "Q7.4"               
    ## [10] "Goethite_525nm_FstD"

### Discriminant power

In the three-step method, the capacity of a property to discriminate
among source groups is commonly assessed using a Kruskal-Wallis H-test.
The *discriminant.test* function arguments are very similar to
*range.tests*. As an alternative Kolmogov-Smirnov two-samples tests can
be used. It provides more detailled results as source groups are
compared to each other.

``` r
KW.results <- discriminant.test(Hayama,
                                class = "Class",
                                mixture = "Target",
                                test = "KW", # will perform Kruskal-Wallis (KW) or Kolmogorov-smirnov (KS)
                                properties = prop.values,
                                # p.level = .05,
                                # save.discrim.tests = T, # to save two-samples tests
                                # save.dir = save.dir.fingR,
                                # note = "fingR"
                                )

KW.results[,c(1,4:5)]
```

    ##               Property Kruskal.Wallis_p.value Kruskal.Wallis_signif
    ## 1              TOC_PrC                0.00000                   ***
    ## 2               TN_PrC                0.00000                   ***
    ## 3             Al_mg.kg                0.00000                   ***
    ## 4             Ca_mg.kg                0.00250                    **
    ## 5             Fe_mg.kg                0.84128                      
    ## 6              K_mg.kg                0.00003                   ***
    ## 7             Mg_mg.kg                0.11649                      
    ## 8             Mn_mg.kg                0.00357                    **
    ## 9             Pb_mg.kg                0.05469                     .
    ## 10            Si_mg.kg                0.00006                   ***
    ## 11            Sr_mg.kg                0.00218                    **
    ## 12            Ti_mg.kg                0.08341                     .
    ## 13            Zn_mg.kg                0.00030                   ***
    ## 14            Zr_mg.kg                0.01759                     *
    ## 15          L_star_D65                0.00000                   ***
    ## 16          a_star_D65                0.00015                   ***
    ## 17          b_star_D65                0.00004                   ***
    ## 18          C_star_D65                0.00005                   ***
    ## 19               h_D65                0.00000                   ***
    ## 20                  A1                0.00135                    **
    ## 21                  A2                0.00971                    **
    ## 22                  Gt                0.00006                   ***
    ## 23                Q7.4                0.00040                   ***
    ## 24 Goethite_525nm_FstD                0.00001                   ***

Properties that get a Kruskal-Wallis p-value bellow 0.05 (**p.value =
0.05**), are selected as discriminant properties. The function
*is.discriminant* list them. The function automatically recognise
data.frame produced by *discriminant.test* but it is possible to set it
for other data.frame format.

``` r
prop.discrim <- is.discriminant(KW.results, # data.frame from discriminant.test or any df with the same organisation.
                                # property = "Property", # the column name of which contains property names
                                # test.format = "Kruskal.Wallis_p.value", # the column name of which contains KW p-value
                                # test.pos = 1, #position of test name in column name
                                # sep = "_", # the splitting character in column test names
                                # p.level = 0.05, # KW p-value level to reject H0 (default)
                                # note = "fingR" # add a note to tests names in the list
                                ) 

prop.discrim
```

    ## $Kruskal.Wallis
    ##  [1] "TOC_PrC"             "TN_PrC"              "Al_mg.kg"           
    ##  [4] "Ca_mg.kg"            "K_mg.kg"             "Mn_mg.kg"           
    ##  [7] "Si_mg.kg"            "Sr_mg.kg"            "Zn_mg.kg"           
    ## [10] "Zr_mg.kg"            "L_star_D65"          "a_star_D65"         
    ## [13] "b_star_D65"          "C_star_D65"          "h_D65"              
    ## [16] "A1"                  "A2"                  "Gt"                 
    ## [19] "Q7.4"                "Goethite_525nm_FstD"

### Selected tracers

Properties that are conservative and discriminant are selected as
tracers. Conservative and discriminant *lists* can contains several
*vector* of properties.

``` r
tracers <- selected.tracers(cons = prop.cons, discrim = prop.discrim)

tracers
```

    ## $mean.sd_Kruskal.Wallis
    ## [1] "TOC_PrC"             "TN_PrC"              "Al_mg.kg"           
    ## [4] "L_star_D65"          "b_star_D65"          "C_star_D65"         
    ## [7] "A2"                  "Q7.4"                "Goethite_525nm_FstD"

### Discriminant Function Analysis (DFA) stepwise selection

methodly a DFA forward stepwise selection on the selected tracers
list(s) to keep tracers that maximise

``` r
tracers.DFA <- list()

for(method in names(tracers)){ # in case of different range tests criteria
  tracers.DFA[[method]] <- intersect(tracers[[method]], # intersect reorder DFA kept tracers according to the previous order
                                     
                                     fingR::stepwise.selection(Hayama, # the dataset
                                                               class = "Class", # variable where difference between source class/levels/groups and mixture is made
                                                               tracers = tracers[[method]], # a selection of tracers
                                                               target = "Target", # a character string corresponding to way "Target" are named
                                                               # save.dir = save.dir.fingR,
                                                               # note = paste0("fingR", test)
                                                               ))
  
}

tracers.DFA
```

    ## $mean.sd_Kruskal.Wallis
    ## [1] "Al_mg.kg"   "b_star_D65" "C_star_D65"

The DFA did not kept A2, this lead to two different list of tracers:
without DFA and with DFA.

``` r
all.tracers <- list(tracers, tracers.DFA)
names(all.tracers) <- c("no.DFA", "DFA")
all.tracers <- unlist(all.tracers, recursive = F)

selected.tracers <- unique(unname(unlist(all.tracers, recursive = T))) # get the list of all the tracer that were selected at least onces among the methods

all.tracers
```

    ## $no.DFA.mean.sd_Kruskal.Wallis
    ## [1] "TOC_PrC"             "TN_PrC"              "Al_mg.kg"           
    ## [4] "L_star_D65"          "b_star_D65"          "C_star_D65"         
    ## [7] "A2"                  "Q7.4"                "Goethite_525nm_FstD"
    ## 
    ## $DFA.mean.sd_Kruskal.Wallis
    ## [1] "Al_mg.kg"   "b_star_D65" "C_star_D65"

Sometimes, **all.tracers** names are too long for windows file names.
Therefore, we recommand to modify them.

``` r
names(all.tracers) <- c("no.DFA.mSD.KW", "DFA.mSD.KW")
```

# Source contribution modelling

### Artificial mixtures

To assess modelling accuracy we generate virtual mixtures with known
source contribution. First, we generate contribution with
*fingR::VM.contrib.generator*. Contributions are built as a scale
according the set step (between 0 and 1 excluded). Small steps generates
more virtual mixtures. However, it’s possible to directly generate
virtual mixture in *fingR::VM.builder*

``` r
# generate contribution
VM.contrib <- VM.contrib.generator(n.sources = 3, # the number of source groups
                                   groups = c("Cropland", "Forest", "Subsoil"), # the name of source groups
                                   step = .05, # the step between two contributions increment, between 0 and 1 excluded
                                   save.dir = save.dir.fingR)

VM.contrib[1:5,]
```

    ##     mix.names Cropland Forest Subsoil
    ## V11    VM-001     0.00 0.7500  0.2500
    ## V12    VM-002     0.01 0.7425  0.2475
    ## V13    VM-003     0.05 0.7125  0.2375
    ## V14    VM-004     0.10 0.6750  0.2250
    ## V15    VM-005     0.15 0.6375  0.2125

Then we calculate the virtual mixture properties as simple proportional
mixture of the source signatures (i.e. mean and standard deviation).
*fingR::VM.builder* saves and returns a list of two data.frame:
*property* values and *uncertainty*.

``` r
all.VM <- list()
for(method in names(all.tracers)){
  
 all.VM[[method]] <- VM.builder(Hayama, # dataset which contains source samples
                                material = "Material", # indicates the columns where the difference between source and target is made
                                source.name = "Source", # how to recognise source samples in material
                                class = "Class", # the source groups labels
                                tracers = all.tracers[[method]], # the tracers that will be generated
                                contributions = VM.contrib,
                                VM.name = "mix.names", # the column with virtual mixture labels in the VM df
                                save.dir = save.dir.fingR, # need to be saved for MixSIAR
                                note = method)
 
 # correcting uncertainty col names according to how they are named is the main dataset
 colnames(all.VM[[method]]$uncertainty)[2:ncol(all.VM[[method]]$uncertainty)] <- unname(prop.uncertainties[colnames(all.VM[[method]]$uncertainty)[2:ncol(all.VM[[method]]$uncertainty)]])
 
 all.VM[[method]][["full"]] <- dplyr::full_join(all.VM[[method]]$property, all.VM[[method]]$uncertainty, by = "mix.names") # generate dataframe with property and uncertainty values
    
}

all.VM$no.DFA.mSD.KW$property[1:5, 1:5]
```

    ##   mix.names TOC_PrC TN_PrC Al_mg.kg L_star_D65
    ## 1    VM-001    8.57   0.56    76890      51.24
    ## 2    VM-002    8.54   0.56    76970      51.29
    ## 3    VM-003    8.40   0.56    77289      51.47
    ## 4    VM-004    8.23   0.55    77687      51.69
    ## 5    VM-005    8.06   0.54    78085      51.92

Here an example of sets to directly generate virtual mixture within the
*fingR::VM.builder* function.

``` r
VM.builder(Hayama, # dataset which contains source samples
           material = "Material", # indicates the columns where the difference between source and target is made
           source.name = "Source", # how to recognise source samples in material
           class = "Class", # the source groups labels
           tracers = all.tracers[[method]], # the tracers that will be generated
           step = 0.05, # numerical value of the step between two contributions
           save.dir = save.dir.fingR, # need to be saved for MixSIAR
           note = method)
```

### Un-mixing model: MixSIAR

We will use MixSIAR as un-mixing model. You can find all the
informations about MixSIAR here:
<http://brianstock.github.io/MixSIAR/index.html>

MixSIAR requires to load csv respecting a special format. The function
*data.for.MixSIAR* creates these files (i.e. mix and source) according
to your dataset and tracer selection(s).

``` r
for(method in names(all.tracers)){
  data.for.MixSIAR(data = Hayama,
                   class = "Class", # a column where source groups and target are indicated
                   target = "Target", # how target samples are identify ind class column
                   sample.name = "Sample_name",
                   tracers = all.tracers[[method]], # a list of tracer
                   save.dir = save.dir.fingR,
                   note = method)
}
```

MixSIAR also requires to set trophic discrimination factor, which means
that the tracer values are modified (enriched/depleted), as tracers are
considered as strictly conservative trophic discrimination are set to
zero.

``` r
discrim <- matrix(data = 0, # tracers are considered conservative
                  nrow = 3, # number of sources
                  ncol = length(selected.tracers)*2, # maximum number of selected tracers
                  dimnames = list(c("Cropland", "Forest", "Subsoil"), # source groups
                                  paste0(c("Mean", "SD"), rep(selected.tracers, each = 2)))) # generate prop names according to MixSIAR format (i.e. MeanPropertyA, SDPropertyA...)

write.csv(discrim, file = paste(save.dir.fingR, "MixSIAR_discrimination.csv", sep=""), row.names = T)
```

First, we load the virtual mixtures, sediments and source data for each
methodroach as well as the trophic discrimination factor using MixSIAR
loading functions.

``` r
library(MixSIAR)

MSIAR.mix.VM <- list()
MSIAR.mix <- list()
MSIAR.source <- list()
MSIAR.discr <- list()

for(method in names(all.tracers)){
  # virtual mixtures data
  MSIAR.mix.VM[[method]] <- load_mix_data(filename = paste0(save.dir.fingR, paste0("VM_properties_", method,".csv")), # target files
                                          iso_names = all.tracers[[method]], # optimal tracers (conventionally isotopes)
                                          factors = c("mix.names"), # how to differentiate samples
                                          fac_random = FALSE, # if the factor is random effect
                                          cont_effects = NULL) # continuous effect column
  # real samples data
  MSIAR.mix[[method]] <- load_mix_data(filename = paste0(save.dir.fingR, paste0("MixSIAR_mix_", method,".csv")), # target files
                                       iso_names = all.tracers[[method]], # optimal tracers (conventionally isotopes)
                                       factors = c("Sample_name"), # how to differentiate samples
                                       fac_random = FALSE, # if the factor is random effect
                                       cont_effects = NULL) # continuous effect column
  
  # source data
  MSIAR.source[[method]] <- load_source_data(filename = paste0(save.dir.fingR, paste0("MixSIAR_sources_", method,".csv")),
                                             source_factors = NULL, 
                                             conc_dep = FALSE, 
                                             data_type = "means", 
                                             MSIAR.mix[[method]])
  
  # even if discrimination csv file is unique, each selection of tracer needs it's own
  MSIAR.discr[[method]] <- load_discr_data(filename = paste0(save.dir.fingR, "MixSIAR_discrimination.csv"),
                                              MSIAR.mix[[method]])
}
```

Writing JAGS models.

First generate folders to save modelling results.

``` r
# create folder to save mixSIAR_modelling
path.model <- "MixSIAR_modelling/"
if(!dir.exists(file.path(paste0(save.dir.fingR, path.model)))){
  dir.create(file.path(save.dir.fingR, "MixSIAR_modelling/"))
}
for(method in names(all.tracers)){
  if(!dir.exists( file.path( paste0(save.dir.fingR, path.model, paste0("MixSIAR_", method, "/")) ) )){ # create the folder if it doesn't already exist
    dir.create(file.path(dirname(paste0(save.dir.fingR, path.model, paste0("MixSIAR_", method, "/"))),  paste0("MixSIAR_", method, "/")))
  }
  if(!dir.exists( file.path( paste0(save.dir.fingR, path.model, paste0("MixSIAR_VM_", method, "/")) ) )){ # create the folder if it doesn't already exist
    dir.create(file.path(dirname(paste0(save.dir.fingR, path.model, paste0("MixSIAR_VM_", method, "/"))),  paste0("MixSIAR_VM_", method, "/")))
  }
}
```

``` r
for(method in names(all.tracers)){
  # for actual samples
  write_JAGS_model(paste0(paste0(save.dir.fingR, path.model, paste0("MixSIAR_", method, "/")), "MixSIAR_model_", method, ".txt"),
                   resid_err = FALSE, process_err = TRUE,
                   mix = MSIAR.mix[[method]],
                   source = MSIAR.source[[method]])
  
  
  # for virtual mixtures samples
  write_JAGS_model(paste0(paste0(save.dir.fingR, path.model, paste0("MixSIAR_VM_", method, "/")), "MixSIAR_model_VM_", method, ".txt"),
                   resid_err = FALSE, process_err = TRUE,
                   mix = MSIAR.mix.VM[[method]],
                   source = MSIAR.source[[method]])
}
```

Run models

``` r
#  note if "Error: .onload ... 'rgags' -> it's because R version is too old need at least R.2.2

jags.VM <- list()
jags.actual <- list()

run.type <- "test"
#run.type <- "long"

for(method in names(all.tracers)){
  # for actual samples
  jags.actual[[method]] <- run_model(run = run.type,
                                     mix = MSIAR.mix[[method]],
                                     source = MSIAR.source[[method]],
                                     discr = MSIAR.discr[[method]],
                                     model_filename = paste0(paste0(save.dir.fingR, path.model, paste0("MixSIAR_", method, "/")), "MixSIAR_model_", method, ".txt"))
  
  # for virtual mixtures
  jags.VM[[method]] <- run_model(run = run.type,
                                 mix = MSIAR.mix.VM[[method]],
                                 source =  MSIAR.source[[method]],
                                 discr = MSIAR.discr[[method]],
                                 model_filename = paste0(paste0(save.dir.fingR, path.model, paste0("MixSIAR_VM_", method, "/")), "MixSIAR_model_VM_", method, ".txt"))
}
```

To save MixSIAR predictions and calculate general statistics (mean,
median, etc) for each mixture. *JAGS.summary* generate a csv file with
the summary of the prediction (file “contrib…csv”) and save all MixSIAR
predictions (MixSIAR_prevision…csv). *JAGS.pred* collect the prediction
according to the selected statistics (e.g. *stats = “Median”* or
*“Mean”*).

``` r
library(fingR)

MixSIAR.summary <- list()
MixSIAR.summary.VM <- list()
MixSIAR.preds <- list()
MixSIAR.preds.VM <- list()

stats <- c("Median")

for(method in names(all.tracers)){
  # for actual sediments
  MixSIAR.summary[[method]] <- JAGS.summary(jags.actual[[method]],
                                            mix = MSIAR.mix[[method]],
                                            sources = MSIAR.source[[method]],
                                            path = paste0(save.dir.fingR, path.model, paste0("MixSIAR_", method, "/")),
                                            note = method,
                                            save_pred = T) # save the MixSIAR modelling predictions (heavy files)
  
  
  MixSIAR.preds[[method]] <- JAGS.pred(path = paste0(save.dir.fingR, path.model, paste0("MixSIAR_", method, "/"), # location of files generated by JAGS.summary
                                             paste0("contrib_", method, ".csv")),
                                       stats = c("Median"),
                                       save = T,
                                       note = paste0(method))
  
  # for virtual mixtures
  MixSIAR.summary.VM[[method]] <- JAGS.summary(jags.VM[[method]],
                                               mix = MSIAR.mix.VM[[method]],
                                               sources = MSIAR.source[[method]],
                                               path = paste0(save.dir.fingR, path.model, paste0("MixSIAR_VM_", method, "/")),
                                               note = paste0("VM_", method),
                                               save_pred = T)
  
  
  MixSIAR.preds.VM[[method]] <- JAGS.pred(path = paste0(save.dir.fingR, path.model, paste0("MixSIAR_VM_", method, "/"), # location of files generated by JAGS.summary
                                             paste0("contrib_VM_", method, ".csv")),
                                          stats = c("Median"),
                                          save = T,
                                          note = paste0("VM_", method))
}

MixSIAR.preds$no.DFA.mSD.KW[1:5,]
```

    ##              sample Median_Cropland Median_Forest Median_Subsoil
    ## 1 ManoDd_2106_06-07           0.335         0.553          0.111
    ## 2 ManoDd_2106_07-08           0.303         0.535          0.142
    ## 3 ManoDd_2106_08-09           0.312         0.545          0.117
    ## 4 ManoDd_2106_09-10           0.302         0.615          0.065
    ## 5 ManoDd_2106_10-11           0.151         0.723          0.088

``` r
MixSIAR.preds.VM$no.DFA.mSD.KW[1:5,]
```

    ##   sample Median_Cropland Median_Forest Median_Subsoil
    ## 1 VM-001           0.473         0.254          0.273
    ## 2 VM-002           0.380         0.423          0.193
    ## 3 VM-003           0.398         0.415          0.187
    ## 4 VM-004           0.404         0.405          0.195
    ## 5 VM-005           0.437         0.378          0.180

With the virtual mixtures we assess each tracer selection modelling
accuracy. First we calculate each sample Continuous Ranking Probability
Score (CRPS) and prediction interval width (W50) from each sample
MixSIAR prevision (generated with *fingR::JAGS.summary*).

``` r
MixSIAR.CRPS <- list()
MixSIAR.predW <- list()

for(method in names(all.tracers)){
  MixSIAR.CRPS[[method]] <- CRPS(obs = VM.contrib,
                                 prev = read.csv(paste0(save.dir.fingR, path.model, paste0("MixSIAR_VM_", method, "/"), paste0("MixSIAR_prevision_VM_", method, ".csv"))),
                                 source.groups = c("Cropland", "Forest", "Subsoil"),
                                 mean.cal = TRUE,
                                 save.dir = paste0(save.dir.fingR, path.model, paste0("MixSIAR_VM_", method, "/")),
                                 note = paste0("VM_", method))
  
  MixSIAR.predW[[method]] <- interval.width(path.to.prev = paste0(save.dir.fingR, path.model, paste0("MixSIAR_VM_", method, "/"), paste0("MixSIAR_prevision_VM_", method, ".csv")),
                                       mean.cal = TRUE,
                                       save = TRUE,
                                       note = paste0("VM_", method))
}
```

The function *fingR::eval.groups* calculate the ME, RMSE, r2 and NSE per
source group for a tracer selection

``` r
MixSIAR.accuracy <- list()

for(method in names(all.tracers)){
  MixSIAR.accuracy[[method]] <- eval.groups(df.obs = VM.contrib,
                                       df.pred = MixSIAR.preds.VM[[method]],
                                       by = c("mix.names" = "sample"),
                                       note = paste0("VM_", method),
                                       path = paste0(save.dir.fingR, path.model, paste0("MixSIAR_VM_", method, "/")))
  
  # add predW and CRPS to the accuracy statistic table
  MixSIAR.accuracy[[method]] <- left_join( left_join(MixSIAR.accuracy[[method]],
                                                     MixSIAR.predW[[method]]$mean, by = c("Source")),
                                      MixSIAR.CRPS[[method]]$mean, by = c("Source") )
}

MixSIAR.accuracy
```

    ## $no.DFA.mSD.KW
    ##     Type   Source   ME RMSE   r2  NSE W50.mean W95.mean CRPS.mean
    ## 1 Median Cropland  0.1 0.19 0.79 0.57    0.153    0.438    0.1128
    ## 2 Median   Forest -0.1 0.17 0.96 0.65    0.097    0.279    0.0982
    ## 3 Median  Subsoil  0.0 0.07 0.98 0.95    0.078    0.225    0.0393
    ## 
    ## $DFA.mSD.KW
    ##     Type   Source    ME RMSE   r2   NSE W50.mean W95.mean CRPS.mean
    ## 1 Median Cropland  0.24 0.35 0.21 -0.53    0.257    0.663    0.1854
    ## 2 Median   Forest -0.22 0.33 0.71 -0.36    0.208    0.495    0.1611
    ## 3 Median  Subsoil -0.03 0.08 0.97  0.92    0.101    0.289    0.0470

At the moment: ME, RMSE, W50, W95 and CRPS are adapted to contribution
expressed as rate (0 to 1). They should be multiplicated by 100 if
contribution are displayed as percentage. Many graphs can be realised
with virtual mixture predicted contributions: predicted versus
theoretical contributions, CRPS versus theoretical contributions, W50 or
W95 versus theoretical contributions.

### Un-mixing model: Bayesian Mean Model (BMM)

``` r
# create folder to save mixSIAR_modelling
path.model <- "BMM_modelling/"
if(!dir.exists(file.path(paste0(save.dir.fingR, path.model)))){
  dir.create(file.path(save.dir.fingR, "BMM_modelling/"))
}
for(method in names(all.tracers)){
  if(!dir.exists( file.path( paste0(save.dir.fingR, path.model, paste0("BMM_", method, "/")) ) )){ # create the folder if it doesn't already exist
    dir.create(file.path(dirname(paste0(save.dir.fingR, path.model, paste0("BMM_", method, "/"))),  paste0("BMM_", method, "/")))
  }
  if(!dir.exists( file.path( paste0(save.dir.fingR, path.model, paste0("BMM_VM_", method, "/")) ) )){ # create the folder if it doesn't already exist
    dir.create(file.path(dirname(paste0(save.dir.fingR, path.model, paste0("BMM_VM_", method, "/"))),  paste0("BMM_VM_", method, "/")))
  }
}
```

Run models

``` r
BMM.actual <- list()
BMM.VM <- list()

for(method in names(all.tracers)){
    
    #for actual samples
    BMM.actual[[method]] <- run.BMM(data = Hayama,
                                    class = "Class",
                                    mixture = "Target",
                                    sample.id = "Sample_name",
                                    tracers = all.tracers[[method]],
                                    n.iter = 10, # recommended 2500 or 5000
                                    #uncertainty = all.uncertainty[[method]], # missing uncertainty for visible indices
                                    save.dir = paste0(save.dir.fingR, path.model, paste0("BMM_", method, "/")),
                                    note = method)
    
    #for virtual samples
    BMM.VM[[method]] <- run.BMM(data = Hayama, 
                                data.target = all.VM[[method]]$full, # virtual mixture property value and error
                                class = "Class",
                                mixture = "Target",
                                sample.id = "Sample_name",
                                target.id = "mix.names", # if target
                                tracers = all.tracers[[method]],
                                n.iter = 10, # recommended 2500 or 5000
                                #uncertainty = all.uncertainty[[method]], # missing uncertainty for visible indices
                                save.dir = paste0(save.dir.fingR, path.model, paste0("BMM_VM_", method, "/")),
                                note = paste0("VM_", method))
}
```

To save MixSIAR predictions and calculate general statistics (mean,
median, etc) for each mixture. *BMM.summary* generate a csv file with
the summary of the prediction (file “contrib…csv”) and save all BMM
predictions (MixSIAR_prevision…csv). *BMM.pred* collect the prediction
according to the selected statistics (e.g. *stats = “Median”* or
*“Mean”*).

``` r
BMM.summary.actual <- list()
BMM.preds.actual <- list()

BMM.summary.VM <- list()
BMM.preds.VM <- list()

for(method in names(all.tracers)){
    
    # actual samples
    BMM.summary.actual[[method]] <- BMM.summary(pred = BMM.actual[[method]],
                                                #sample.id = "mix.names",
                                                #source = "source",
                                                #value = "value",
                                                save.dir = paste0(save.dir.fingR, path.model, paste0("BMM_", method, "/")),
                                                note = method)
    
    BMM.preds.actual[[method]] <- BMM.pred(data = BMM.summary.actual[[method]],
                                           stats = c("Median"),
                                           #sample.id = "mix.names",
                                           #source = "source",
                                           save.dir = paste0(save.dir.fingR, path.model, paste0("BMM_", method, "/")),
                                           note = method)
    
    # virtual mixtures
    BMM.summary.VM[[method]] <- BMM.summary(pred = BMM.VM[[method]],
                                            save.dir = paste0(save.dir.fingR, path.model, paste0("BMM_VM_", method, "/")),
                                            note = paste0("VM_", method))
    
    BMM.preds.VM[[method]] <- BMM.pred(data = BMM.summary.VM[[method]],
                                       stats = c("Median"),
                                            save.dir = paste0(save.dir.fingR, path.model, paste0("BMM_VM_", method, "/")),
                                            note = paste0("VM_", method))
}


BMM.preds.actual$no.DFA.mSD.KW[1:5,]
```

    ##           mix.names Median_Cropland Median_Forest Median_Subsoil
    ## 1 ManoDd_2106_06-07           0.663         0.182          0.137
    ## 2 ManoDd_2106_07-08           0.525         0.210          0.064
    ## 3 ManoDd_2106_08-09           0.524         0.345          0.005
    ## 4 ManoDd_2106_09-10           0.342         0.564          0.077
    ## 5 ManoDd_2106_10-11           0.075         0.622          0.001

``` r
BMM.preds.VM$no.DFA.mSD.KW[1:5,]
```

    ##   mix.names Median_Cropland Median_Forest Median_Subsoil
    ## 1    VM-001           0.217         0.491          0.158
    ## 2    VM-002           0.339         0.361          0.231
    ## 3    VM-003           0.565         0.369          0.107
    ## 4    VM-004           0.507         0.415          0.033
    ## 5    VM-005           0.522         0.314          0.044

With the virtual mixtures we assess each tracer selection modelling
accuracy. First we calculate each sample Continuous Ranking Probability
Score (CRPS) and prediction interval width (W50) from each sample BMM
prevision (output of *fingR::run.BMM*).

``` r
BMM.CRPS <- list()
BMM.predW <- list()

for(method in names(all.tracers)){
    
    BMM.CRPS[[method]] <- CRPS(obs = VM.contrib,
                               prev = BMM.VM[[method]],
                               #prev.source.name = "source",
                               #prev.values = "value",
                               #source.groups = colnames(VM.contrib)[2:ncol(VM.contrib)],
                               mean.cal = TRUE,
                               save.dir = paste0(save.dir.fingR, path.model, paste0("BMM_VM_", method, "/")),
                               note = paste0("VM_", method))
    
    BMM.predW[[method]] <- interval.width(prev = BMM.VM[[method]],
                                          mean.cal = TRUE,
                                          save.dir = paste0(save.dir.fingR, path.model, paste0("BMM_VM_", method, "/")),
                                          note = paste0("VM_", method))
}
```

The function *fingR::eval.groups* calculate the ME, RMSE, r2 and NSE per
source group for a tracer selection

``` r
BMM.accuracy <- list()

for(method in names(all.tracers)){
  BMM.accuracy[[method]] <- eval.groups(df.obs = VM.contrib,
                                        df.pred = BMM.preds.VM[[method]],
                                        by = "mix.names",
                                        note = paste0("VM_", method),
                                        path = paste0(save.dir.fingR, path.model, paste0("BMM_VM_", method, "/")))
  
  # add predW and CRPS to the accuracy statistic table
  BMM.accuracy[[method]] <- left_join( left_join(BMM.accuracy[[method]],
                                                 BMM.predW[[method]]$mean, by = c("Source")),
                                      BMM.CRPS[[method]]$mean, by = c("Source") )
}

BMM.accuracy
```

    ## $no.DFA.mSD.KW
    ##     Type   Source    ME RMSE   r2  NSE W50.mean W95.mean CRPS.mean
    ## 1 Median Cropland  0.03 0.21 0.45 0.44    0.366    0.728    0.1213
    ## 2 Median   Forest -0.07 0.18 0.71 0.60    0.234    0.537    0.0935
    ## 3 Median  Subsoil -0.02 0.10 0.89 0.89    0.231    0.517    0.0643
    ## 
    ## $DFA.mSD.KW
    ##     Type   Source    ME RMSE   r2  NSE W50.mean W95.mean CRPS.mean
    ## 1 Median Cropland -0.05 0.25 0.28 0.21    0.451    0.795    0.1440
    ## 2 Median   Forest -0.07 0.20 0.56 0.50    0.311    0.640    0.1126
    ## 3 Median  Subsoil  0.01 0.13 0.80 0.79    0.285    0.629    0.0811

# References

- Chalaux-Clergue, Thomas, Evrard, Olivier, Durand, Roxanne, Caumon,
  Alison, Hayashi, Seiji, Tsuji, Hideki, Huon, Sylvain, Vaury,
  Véronique, Wakiyama, Yoshifumi, Nakao, Atsushi, Laceby, J. Patrick, &
  Onda, Yuichi. (2022). Organic matter, geochemical and colorimetric
  properties of potential source material, target sediment and
  laboratory mixtures for conducting sediment fingerprinting approaches
  in the Mano Dam Reservoir (Hayama Lake) catchment, Fukushima
  Prefecture, Japan. (Version 1) \[Data set\]. Zenodo.
  \[<https://doi.org/10.5281/zenodo.7081094>\]
- Chalaux-Clergue, T., Bizeul, R., Batista, P. V. G., Martinez-
  Carreras,  N., Laceby, J. P., and Evrard, O.: Sensitivity of source 
  sediment fingerprinting modelling to tracer selection methods,
  EGUsphere [preprint], https://doi.org/10.5194/egusphere-2023-1970, 2023. 
