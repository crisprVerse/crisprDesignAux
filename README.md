crisprDesignAux: auxiliary functions to the crisprDesign package
================

-   <a href="#overview" id="toc-overview">Overview</a>
-   <a href="#installation" id="toc-installation">Installation</a>
    -   <a href="#software-requirements" id="toc-software-requirements">Software
        requirements</a>
        -   <a href="#os-requirements" id="toc-os-requirements">OS Requirements</a>
    -   <a href="#installation-1" id="toc-installation-1">Installation</a>
-   <a href="#converting-a-guideset-object-to-a-bed-file"
    id="toc-converting-a-guideset-object-to-a-bed-file">Converting a
    GuideSet object to a BED file</a>
-   <a href="#license" id="toc-license">License</a>
-   <a href="#reproducibility" id="toc-reproducibility">Reproducibility</a>

Authors: Jean-Philippe Fortin

Date: Sept 12, 2022

# Overview

`crisprDesignAux`

# Installation

## Software requirements

### OS Requirements

This package is supported for macOS, Linux and Windows machines. It was
developed and tested on R version 4.2.1

## Installation

`crisprDesignAux` can be installed from the Bioconductor devel branch by
typing the following commands inside of an R session:

``` r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(version="devel")
BiocManager::install("crisprDesignAux")
```

The complete documentation for the package can be found
[here](https://bioconductor.org/packages/devel/bioc/manuals/crisprDesignAux/man/crisprDesignAux.pdf).

# Converting a GuideSet object to a BED file

We load `crisprDesignAux` in the usual way:

``` r
library(crisprDesignAux)
```

    ## Warning: multiple methods tables found for 'aperm'

    ## Warning: replacing previous import 'BiocGenerics::aperm' by
    ## 'DelayedArray::aperm' when loading 'SummarizedExperiment'

# License

The project as a whole is covered by the MIT license.

# Reproducibility

``` r
sessionInfo()
```

    ## R version 4.2.1 (2022-06-23)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.7
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] crisprDesignAux_1.3.15
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] bitops_1.0-7                  matrixStats_0.62.0           
    ##   [3] bit64_4.0.5                   filelock_1.0.2               
    ##   [5] progress_1.2.2                httr_1.4.4                   
    ##   [7] GenomeInfoDb_1.33.7           tools_4.2.1                  
    ##   [9] utf8_1.2.2                    R6_2.5.1                     
    ##  [11] DBI_1.1.3                     BiocGenerics_0.43.4          
    ##  [13] tidyselect_1.1.2              prettyunits_1.1.1            
    ##  [15] bit_4.0.4                     curl_4.3.2                   
    ##  [17] compiler_4.2.1                crisprBowtie_1.1.1           
    ##  [19] cli_3.4.0                     crisprDesign_0.99.150        
    ##  [21] Biobase_2.57.1                basilisk.utils_1.9.3         
    ##  [23] crisprScoreData_1.1.3         xml2_1.3.3                   
    ##  [25] DelayedArray_0.23.1           rtracklayer_1.57.0           
    ##  [27] randomForest_4.7-1.1          readr_2.1.2                  
    ##  [29] rappdirs_0.3.3                stringr_1.4.1                
    ##  [31] digest_0.6.29                 Rsamtools_2.13.4             
    ##  [33] rmarkdown_2.16                crisprScore_1.1.15           
    ##  [35] basilisk_1.9.6                XVector_0.37.1               
    ##  [37] pkgconfig_2.0.3               htmltools_0.5.3              
    ##  [39] MatrixGenerics_1.9.1          dbplyr_2.2.1                 
    ##  [41] fastmap_1.1.0                 BSgenome_1.65.2              
    ##  [43] rlang_1.0.5                   rstudioapi_0.14              
    ##  [45] RSQLite_2.2.16                shiny_1.7.2                  
    ##  [47] BiocIO_1.7.1                  generics_0.1.3               
    ##  [49] jsonlite_1.8.0                BiocParallel_1.31.12         
    ##  [51] dplyr_1.0.10                  VariantAnnotation_1.43.3     
    ##  [53] RCurl_1.98-1.8                magrittr_2.0.3               
    ##  [55] GenomeInfoDbData_1.2.8        Matrix_1.4-1                 
    ##  [57] Rcpp_1.0.9                    S4Vectors_0.35.3             
    ##  [59] fansi_1.0.3                   reticulate_1.26              
    ##  [61] Rbowtie_1.37.0                lifecycle_1.0.1              
    ##  [63] stringi_1.7.8                 yaml_2.3.5                   
    ##  [65] SummarizedExperiment_1.27.2   zlibbioc_1.43.0              
    ##  [67] BiocFileCache_2.5.0           AnnotationHub_3.5.1          
    ##  [69] grid_4.2.1                    blob_1.2.3                   
    ##  [71] promises_1.2.0.1              parallel_4.2.1               
    ##  [73] ExperimentHub_2.5.0           crayon_1.5.1                 
    ##  [75] dir.expiry_1.5.1              lattice_0.20-45              
    ##  [77] Biostrings_2.65.3             GenomicFeatures_1.49.6       
    ##  [79] hms_1.1.2                     KEGGREST_1.37.3              
    ##  [81] knitr_1.40                    pillar_1.8.1                 
    ##  [83] GenomicRanges_1.49.1          rjson_0.2.21                 
    ##  [85] codetools_0.2-18              biomaRt_2.53.2               
    ##  [87] stats4_4.2.1                  BiocVersion_3.16.0           
    ##  [89] XML_3.99-0.10                 glue_1.6.2                   
    ##  [91] evaluate_0.16                 BiocManager_1.30.18          
    ##  [93] httpuv_1.6.5                  png_0.1-7                    
    ##  [95] vctrs_0.4.1                   tzdb_0.3.0                   
    ##  [97] purrr_0.3.4                   assertthat_0.2.1             
    ##  [99] cachem_1.0.6                  xfun_0.32                    
    ## [101] mime_0.12                     xtable_1.8-4                 
    ## [103] restfulr_0.0.15               later_1.3.0                  
    ## [105] tibble_3.1.8                  GenomicAlignments_1.33.1     
    ## [107] AnnotationDbi_1.59.1          memoise_2.0.1                
    ## [109] IRanges_2.31.2                crisprBase_1.1.5             
    ## [111] interactiveDisplayBase_1.35.0 ellipsis_0.3.2
