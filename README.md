
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Temporal resolution of hydrogen peroxide (H2O2) dynamics during heat stress do not support a causative role in coral bleaching

The goal of this project is to reproduce all analyses and figures of the
paper

> Title: Temporal resolution of hydrogen peroxide (H2O2) dynamics during
> heat stress do not support a causative role in coral bleaching
>
> Authors: Schlotheuber M, Voolstra CR, de Beer D, Camp EF, Klatt JM,
> Ghilardi M, Neumüller K, Ousley S, Bejarano S

currently under revision at Coral Reefs.

## Content

This repository is structured as follow:

-   [:file_folder:data/](https://github.com/MarlenSchlotheuber/CBASS_Microsensor_ROS/tree/CBASS_Microsensor_ROS/data):
    contains all raw and processed data and images used for Visual
    Bleaching Assessment (VBA).
-   [:file_folder:R/](https://github.com/MarlenSchlotheuber/CBASS_Microsensor_ROS/tree/CBASS_Microsensor_ROS/R):
    contains all the R code used to reproduce analyses and figures.
-   [:file_folder:figures/](https://github.com/MarlenSchlotheuber/CBASS_Microsensor_ROS/tree/CBASS_Microsensor_ROS/figures):
    contains all figures created during the analyses.

## Instructions

-   Clone this repository (for those not familiar with GitHub, click on
    the green button `Code` on the project main page on GitHub and then
    on `Download ZIP` to download the entire repository, thus unzip it).

-   Open the `.Rproj` file in RStudio or open an R session with working
    directory set to the root of the project.

-   Reproduce the entire project by running:

``` r
# Install dependencies
devtools::install_deps()
# Run all scripts
scripts <- list.files("R/")
lapply(paste("R", scripts, sep = "/"), source)
```

For questions related to the data please contact Marlen Schlotheuber at
<marlen.schlotheuber@uni-konstanz.de>. For questions related to the code
please contact Mattia Ghilardi at <mattia.ghilardi91@gmail.com>.

## Working environment

    #> R version 4.1.3 (2022-03-10)
    #> Platform: x86_64-pc-linux-gnu (64-bit)
    #> Running under: Ubuntu 20.04.6 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
    #> LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    #>  [3] LC_TIME=de_DE.UTF-8        LC_COLLATE=en_US.UTF-8    
    #>  [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=en_US.UTF-8   
    #>  [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
    #>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    #> [11] LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] compiler_4.1.3  magrittr_2.0.1  fastmap_1.1.0   cli_3.4.1      
    #>  [5] tools_4.1.3     htmltools_0.5.5 rstudioapi_0.13 yaml_2.2.1     
    #>  [9] stringi_1.7.3   rmarkdown_2.14  knitr_1.33      stringr_1.4.0  
    #> [13] xfun_0.31       digest_0.6.27   rlang_1.0.6     evaluate_0.14
