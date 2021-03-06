
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

The goal of the Calibration Training App is to help the [University of
Bonn](https://www.uni-bonn.de/en/university/) administer *calibration*
assessments …

## Installation

1.  Clone this repository to your local machine

2.  Open the **calibration_app.Rproj** file from the directory on your
    local machine where you cloned this repository. This should install
    the {renv} package if you do not already have it installed, but if
    you don’t see that happen in the console, run
    `install.packages("renv")`.

3.  Run `renv::restore()` to install the package dependencies needed to
    run this app successfully

4.  Edit the [global.R](global.R) script as necessary (see
    [below](#app-parameters))

5.  Open the [app.R](app.R) file and execute the code in that script to
    launch the app

### App Parameters

Before a calibration workshop, the [global.R](global.R) script should be
edited to define the following parameters for the workshop:

1.  The language that questions should be shown in (current options
    include *English*, *German*, and *Vietnamese*)

2.  Google authentication credentials; such credentials are not
    necessary if the Google Sheet containing the questions is *public*,
    and require a simple call to `googlesheets4::gs4_deauth()`

    -   If the Google Sheet is not public, we recommend reading the
        [**{googlesheets4}** authentication
        documentation](https://googlesheets4.tidyverse.org/articles/auth.html)
        to determine how to configure the appropriate credentials

3.  The URL of the Google Sheet’s shareable link

4.  The specific questions to be used for each group & question type

5.  The [{pins}](https://pins.rstudio.com/) board on RStudio Connect
    that has been dedicated for this workshop

## Workflow

![](www/calibration_app_workflow.png)

------------------------------------------------------------------------

See below for session info for full reproducibility:

``` r
sessionInfo()
#> R version 4.1.3 (2022-03-10)
#> Platform: x86_64-apple-darwin17.0 (64-bit)
#> Running under: macOS Big Sur/Monterey 10.16
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRblas.0.dylib
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices datasets  utils     methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_4.1.3  magrittr_2.0.1  fastmap_1.1.0   htmltools_0.5.2
#>  [5] tools_4.1.3     yaml_2.2.1      stringi_1.7.5   rmarkdown_2.11 
#>  [9] knitr_1.36      stringr_1.4.0   xfun_0.27       digest_0.6.28  
#> [13] rlang_0.4.12    renv_0.14.0     evaluate_0.14
```
