# Article Information

**Title**: Body size and local density explain movement patterns in
stream fish

# Analysis

## Movement Analysis

**Dispersal-Observation Model**: Script `run_model_move.R` runs the
Dispersal-Observation model. Only consecutive recaptures were used.
Model source codes are `model_move.R`.

Data are formatted in and sourced from:

- data_fmt/data_move.rds (from `format_cmr.R`; data frame with
  consecutive fish movement related data)
- data_fmt/data_water_hobo.rds (from `format_water_level.R`; data
  frame with water temperature)
- data_fmt/data_density.rds (from `format_cmr.R`; generates all
  density of all fish species per section area)
- data_fmt/data_habitat.rds (from `format_habitat.R`; data frame
  with all habitat data)
- data_fmt/data_detection.rds (from `run_model_scjs.R`; generated
  to account for seasonal detectability)
- data_fmt/data_season.rds (from `run_model_scjs.R`; generated to
  include seasonal differences)

For visualization, see:

- `figures_main.R`
- `figures_supp.R`

## Detection Analysis

**Detection Model**: Script `run_model_scjs.R` runs the modified CJS
model accounting for seasonal variation in detection probability. Model
source codes are `model_scjs.R`.

Data are formatted in and sourced from:

- data_fmt/data_cmr.rds (from `format_cmr.R`; base data frame with
  all fish related data (tagged and non-tagged fish))
- data_fmt/data_move.rds (from `format_cmr.R`; data frame with
  consecutive fish movement related data)

## Formatting Habitat Data (`format_habitat.R`)

Data was collected by section in which each section was broken into
three transects. Measurements were collected along three evenly spaced
points along each transect. These were then averaged to generate one
value for each section.

- occasion: range from 1-15
- section: range from 1-43
- width: wet width measured in meters
- section length: wet length measured in meters
- depth_mean: measured in millimeters
- velocity_mean: measured in meters per second
- substrate_mean: measured in millimeters
- area: surface area (m^2) of section based on width and total section
  length
- area_pool: surface area (m^2) of pool based on width and length of
  that
- area_riffle: surface area (m^2) of riffle based on width and length
- area_run: surface area (m^2) of run based on width and length
- area_ucb: surface area (m^2) of undercut bank based on width and
  length (renamed as habitat refuge area (HRA) in manuscript)

## Formatting Capture-Recapture Data

Fish were collected via single-pass backpack electroshocking
(`format_cmr.R`).

- species: tagged species ID
- tag_id: individual tag ID
- section0: section of capture
- section1: section of recapture (NA if not recaptured)
- occasion0: occasion of capture
- occasion1: occasion of recapture (NA if not recaptured)
- length0: total body length (mm) at capture
- length1: total body length (mm) at recapture (NA if not recaptured)
- datetime0: actual date at capture (year, month, day, hour, minute,
  second)
- datetime1: actual date at recapture (NA if not recaptured)
- weight0: total wet weight (g) at capture
- weight1: total wet weight (g) at recapture (NA if not recaptured)

## Formatting Density Data

Density of each fish species was calculated as the number of individuals
per section area for each section across each occasion

## Version and set-up

    ## R version 4.4.1 (2024-06-14)
    ## Platform: aarch64-apple-darwin20
    ## Running under: macOS 15.2
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: America/Chicago
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] reporter_1.4.4             common_1.1.3              
    ##  [3] xtable_1.8-4               ggfortify_0.4.17          
    ##  [5] kableExtra_1.4.0           scales_1.3.0              
    ##  [7] MCMCvis_0.16.3             googledrive_2.1.1         
    ##  [9] gtable_0.3.5               ggh4x_0.2.8               
    ## [11] PerformanceAnalytics_2.0.4 xts_0.14.0                
    ## [13] zoo_1.8-12                 usethis_2.2.3             
    ## [15] foreach_1.5.2              ggpubr_0.6.0              
    ## [17] lubridate_1.9.3            forcats_1.0.0             
    ## [19] stringr_1.5.1              dplyr_1.1.4               
    ## [21] purrr_1.0.2                readr_2.1.5               
    ## [23] tidyr_1.3.1                tibble_3.2.1              
    ## [25] ggplot2_3.5.1              tidyverse_2.0.0           
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] xfun_0.49         rstatix_0.7.2     gargle_1.5.2      lattice_0.22-6   
    ##  [5] tzdb_0.4.0        quadprog_1.5-8    vctrs_0.6.5       tools_4.4.1      
    ##  [9] generics_0.1.3    fansi_1.0.6       pacman_0.5.1      pkgconfig_2.0.3  
    ## [13] lifecycle_1.0.4   compiler_4.4.1    munsell_0.5.1     codetools_0.2-20 
    ## [17] carData_3.0-5     fmtr_1.6.5        htmltools_0.5.8.1 yaml_2.3.8       
    ## [21] crayon_1.5.2      pillar_1.9.0      car_3.1-2         iterators_1.0.14 
    ## [25] abind_1.4-5       zip_2.3.1         tidyselect_1.2.1  digest_0.6.35    
    ## [29] stringi_1.8.4     rprojroot_2.0.4   fastmap_1.2.0     here_1.0.1       
    ## [33] colorspace_2.1-0  cli_3.6.2         magrittr_2.0.3    utf8_1.2.4       
    ## [37] broom_1.0.6       withr_3.0.0       backports_1.5.0   timechange_0.3.0 
    ## [41] rmarkdown_2.27    jpeg_0.1-10       gridExtra_2.3     ggsignif_0.6.4   
    ## [45] hms_1.1.3         evaluate_0.23     knitr_1.49        viridisLite_0.4.2
    ## [49] rlang_1.1.3       Rcpp_1.0.12       glue_1.7.0        xml2_1.3.6       
    ## [53] svglite_2.1.3     rstudioapi_0.16.0 R6_2.5.1          systemfonts_1.1.0
    ## [57] fs_1.6.4
