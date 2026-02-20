# Check and install R packages

Check and install R packages

## Usage

``` r
check_r(
  packages,
  lib = .libPaths()[1],
  dependencies = TRUE,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- packages:

  Package to be installed. Package source can be *CRAN*, *Bioconductor*
  or *Github*. By default, the package name is extracted according to
  the `packages` parameter.

- lib:

  The location of the library directories where to install the packages.

- dependencies:

  Whether to install dependencies of the packages. Default is `TRUE`.

- force:

  Whether to force the installation of packages. Default is `FALSE`.

- verbose:

  Whether to print the message. Default is `TRUE`.

## Value

Package installation status.

## Examples

``` r
check_r(c("ggplot2", "dplyr"))
#> ◌ [2026-02-20 02:15:22] Installing: ggplot2...
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#>  
#> → Will install 116 packages.
#> → All 116 packages (0 B) are cached.
#> + DBI             1.2.3    
#> + Formula         1.2-5    
#> + Hmisc           5.2-5    
#> + MatrixModels    0.5-4    
#> + R6              2.6.1    
#> + RColorBrewer    1.1-3    
#> + Rcpp            1.1.1    
#> + S7              0.2.1    
#> + SparseM         1.84-2   
#> + TH.data         1.1-5    
#> + askpass         1.2.1    
#> + backports       1.5.0    
#> + base64enc       0.1-6    
#> + brew            1.0-10   
#> + brio            1.1.5    
#> + broom           1.0.12   
#> + bslib           0.10.0   
#> + cachem          1.1.0    
#> + callr           3.7.6    
#> + checkmate       2.3.4    
#> + classInt        0.4-11   
#> + cli             3.6.5    
#> + colorspace      2.1-2    
#> + commonmark      2.0.0    
#> + covr            3.6.5    
#> + crayon          1.5.3    
#> + curl            7.0.0     + ✔ libcurl4-openssl-dev, ✔ libssl-dev
#> + data.table      1.18.2.1 
#> + desc            1.4.3    
#> + diffobj         0.3.6    
#> + digest          0.6.39   
#> + dplyr           1.2.0    
#> + e1071           1.7-17   
#> + evaluate        1.0.5    
#> + farver          2.1.2    
#> + fastmap         1.2.0    
#> + fontawesome     0.5.3    
#> + fs              1.6.6     + ✔ make
#> + generics        0.1.4    
#> + ggplot2         4.0.2    
#> + ggplot2movies   0.0.1    
#> + glue            1.8.0    
#> + gridExtra       2.3      
#> + gtable          0.3.6    
#> + hexbin          1.28.5   
#> + highr           0.11     
#> + hms             1.1.4    
#> + htmlTable       2.4.3    
#> + htmltools       0.5.9    
#> + htmlwidgets     1.6.4    
#> + httr            1.4.8    
#> + isoband         0.3.0    
#> + jquerylib       0.1.4    
#> + jsonlite        2.0.0    
#> + knitr           1.51      + ✔ pandoc
#> + labeling        0.4.3    
#> + later           1.4.6    
#> + lazyeval        0.2.2    
#> + lifecycle       1.0.5    
#> + magrittr        2.0.4    
#> + mapproj         1.2.12   
#> + maps            3.4.3    
#> + memoise         2.0.1    
#> + mime            0.13     
#> + multcomp        1.4-29   
#> + munsell         0.5.1    
#> + mvtnorm         1.3-3    
#> + openssl         2.3.4     + ✔ libssl-dev
#> + pillar          1.11.1   
#> + pkgbuild        1.4.8    
#> + pkgconfig       2.0.3    
#> + pkgload         1.5.0    
#> + praise          1.0.0    
#> + processx        3.8.6    
#> + profvis         0.4.0    
#> + proxy           0.4-29   
#> + ps              1.9.1    
#> + purrr           1.2.1    
#> + quantreg        6.1      
#> + quarto          1.5.1    
#> + ragg            1.5.0     + ✔ libfreetype6-dev, ✔ libjpeg-dev, ✔ libpng-dev, ✔ libtiff-dev, ✔ libwebp-dev
#> + rappdirs        0.3.4    
#> + rex             1.2.1    
#> + rlang           1.1.7    
#> + rmarkdown       2.30      + ✔ pandoc
#> + roxygen2        7.3.3    
#> + rprojroot       2.1.1    
#> + rstudioapi      0.18.0   
#> + s2              1.1.9     + ✖ libabsl-dev, ✖ cmake, ✔ libssl-dev
#> + sandwich        3.1-1    
#> + sass            0.4.10    + ✔ make
#> + scales          1.4.0    
#> + sf              1.0-24    + ✖ libgdal-dev, ✖ gdal-bin, ✖ libgeos-dev, ✖ libproj-dev, ✔ libsqlite3-dev
#> + stringi         1.8.7     + ✔ libicu-dev
#> + stringr         1.6.0    
#> + svglite         2.2.2     + ✔ libpng-dev
#> + sys             3.4.3    
#> + systemfonts     1.3.1     + ✔ libfontconfig1-dev, ✔ libfreetype6-dev
#> + testthat        3.3.2    
#> + textshaping     1.0.4     + ✔ libfreetype6-dev, ✔ libfribidi-dev, ✔ libharfbuzz-dev
#> + tibble          3.3.1    
#> + tidyr           1.3.2    
#> + tidyselect      1.2.1    
#> + tinytex         0.58     
#> + units           1.0-0     + ✖ libudunits2-dev
#> + utf8            1.2.6    
#> + vctrs           0.7.1    
#> + vdiffr          1.0.9     + ✔ libpng-dev
#> + viridisLite     0.4.3    
#> + waldo           0.6.2    
#> + withr           3.0.2    
#> + wk              0.9.5    
#> + xfun            0.56     
#> + xml2            1.5.2     + ✔ libxml2-dev
#> + yaml            2.3.12   
#> + zoo             1.8-15   
#> → Will install 7 system packages:
#> + cmake            - s2   
#> + gdal-bin         - sf   
#> + libabsl-dev      - s2   
#> + libgdal-dev      - sf   
#> + libgeos-dev      - sf   
#> + libproj-dev      - sf   
#> + libudunits2-dev  - units
#> ℹ No downloads are needed, 116 pkgs are cached
#> ✔ Got askpass 1.2.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (21.89 kB)
#> ✔ Got base64enc 0.1-6 (x86_64-pc-linux-gnu-ubuntu-24.04) (28.70 kB)
#> ✔ Got brio 1.1.5 (x86_64-pc-linux-gnu-ubuntu-24.04) (35.30 kB)
#> ✔ Got brew 1.0-10 (x86_64-pc-linux-gnu-ubuntu-24.04) (76.35 kB)
#> ✔ Got backports 1.5.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (115.90 kB)
#> ✔ Got Formula 1.2-5 (x86_64-pc-linux-gnu-ubuntu-24.04) (159.13 kB)
#> ✔ Got digest 0.6.39 (x86_64-pc-linux-gnu-ubuntu-24.04) (230.38 kB)
#> ✔ Got pkgconfig 2.0.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (18.08 kB)
#> ✔ Got RColorBrewer 1.1-3 (x86_64-pc-linux-gnu-ubuntu-24.04) (51.81 kB)
#> ✔ Got pkgload 1.5.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (222.83 kB)
#> ✔ Got sys 3.4.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (40.73 kB)
#> ✔ Got e1071 1.7-17 (x86_64-pc-linux-gnu-ubuntu-24.04) (588.22 kB)
#> ✔ Got stringr 1.6.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (333.76 kB)
#> ✔ Got diffobj 0.3.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.00 MB)
#> ✔ Got gtable 0.3.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (222.55 kB)
#> ✔ Got lifecycle 1.0.5 (x86_64-pc-linux-gnu-ubuntu-24.04) (131.69 kB)
#> ✔ Got systemfonts 1.3.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (817.04 kB)
#> ✔ Got curl 7.0.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (788.30 kB)
#> ✔ Got dplyr 1.2.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.52 MB)
#> ✔ Got broom 1.0.12 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.78 MB)
#> ✔ Got ps 1.9.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (499.64 kB)
#> ✔ Got tidyselect 1.2.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (225.28 kB)
#> ✔ Got generics 0.1.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (80.38 kB)
#> ✔ Got tibble 3.3.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (647.68 kB)
#> ✔ Got mapproj 1.2.12 (x86_64-pc-linux-gnu-ubuntu-24.04) (51.48 kB)
#> ✔ Got httr 1.4.8 (x86_64-pc-linux-gnu-ubuntu-24.04) (480.15 kB)
#> ✔ Got munsell 0.5.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (243.50 kB)
#> ✔ Got pkgbuild 1.4.8 (x86_64-pc-linux-gnu-ubuntu-24.04) (208.62 kB)
#> ✔ Got DBI 1.2.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (916.93 kB)
#> ✔ Got praise 1.0.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (16.39 kB)
#> ✔ Got rprojroot 2.1.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (113.23 kB)
#> ✔ Got rex 1.2.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (126.59 kB)
#> ✔ Got mvtnorm 1.3-3 (x86_64-pc-linux-gnu-ubuntu-24.04) (952.05 kB)
#> ✔ Got testthat 3.3.2 (x86_64-pc-linux-gnu-ubuntu-24.04) (2.14 MB)
#> ✔ Got openssl 2.3.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.31 MB)
#> ✔ Got withr 3.0.2 (x86_64-pc-linux-gnu-ubuntu-24.04) (223.90 kB)
#> ✔ Got xfun 0.56 (x86_64-pc-linux-gnu-ubuntu-24.04) (596.48 kB)
#> ✔ Got rlang 1.1.7 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.60 MB)
#> ✔ Got crayon 1.5.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (163.30 kB)
#> ✔ Got commonmark 2.0.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (148.07 kB)
#> ✔ Got evaluate 1.0.5 (x86_64-pc-linux-gnu-ubuntu-24.04) (102.86 kB)
#> ✔ Got desc 1.4.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (334.52 kB)
#> ✔ Got s2 1.1.9 (x86_64-pc-linux-gnu-ubuntu-24.04) (2.21 MB)
#> ✔ Got wk 0.9.5 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.73 MB)
#> ✔ Got fs 1.6.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (310.07 kB)
#> ✔ Got classInt 0.4-11 (x86_64-pc-linux-gnu-ubuntu-24.04) (496.56 kB)
#> ✔ Got htmlTable 2.4.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (417.61 kB)
#> ✔ Got viridisLite 0.4.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.30 MB)
#> ✔ Got jquerylib 0.1.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (526.85 kB)
#> ✔ Got gridExtra 2.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.11 MB)
#> ✔ Got later 1.4.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (154.56 kB)
#> ✔ Got lazyeval 0.2.2 (x86_64-pc-linux-gnu-ubuntu-24.04) (157.44 kB)
#> ✔ Got hexbin 1.28.5 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.60 MB)
#> ✔ Got ggplot2movies 0.0.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.26 MB)
#> ✔ Got magrittr 2.0.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (221.44 kB)
#> ✔ Got bslib 0.10.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (6.06 MB)
#> ✔ Got R6 2.6.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (86.81 kB)
#> ✔ Got jsonlite 2.0.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.09 MB)
#> ✔ Got pillar 1.11.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (660.46 kB)
#> ✔ Got profvis 0.4.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (348.59 kB)
#> ✔ Got purrr 1.2.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (554.22 kB)
#> ✔ Got S7 0.2.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (325.15 kB)
#> ✔ Got quarto 1.5.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (544.37 kB)
#> ✔ Got roxygen2 7.3.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (731.24 kB)
#> ✔ Got SparseM 1.84-2 (x86_64-pc-linux-gnu-ubuntu-24.04) (887.98 kB)
#> ✔ Got scales 1.4.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (841.36 kB)
#> ✔ Got xml2 1.5.2 (x86_64-pc-linux-gnu-ubuntu-24.04) (281.53 kB)
#> ✔ Got maps 3.4.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (3.09 MB)
#> ✔ Got tinytex 0.58 (x86_64-pc-linux-gnu-ubuntu-24.04) (143.77 kB)
#> ✔ Got waldo 0.6.2 (x86_64-pc-linux-gnu-ubuntu-24.04) (135.16 kB)
#> ✔ Got sandwich 3.1-1 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.51 MB)
#> ✔ Got Rcpp 1.1.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (2.22 MB)
#> ✔ Got vctrs 0.7.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.45 MB)
#> ✔ Got covr 3.6.5 (x86_64-pc-linux-gnu-ubuntu-24.04) (344.29 kB)
#> ✔ Got fastmap 1.2.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (66.05 kB)
#> ✔ Got zoo 1.8-15 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.06 MB)
#> ✔ Got hms 1.1.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (103.38 kB)
#> ✔ Got multcomp 1.4-29 (x86_64-pc-linux-gnu-ubuntu-24.04) (643.70 kB)
#> ✔ Got cli 3.6.5 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.34 MB)
#> ✔ Got labeling 0.4.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (60.95 kB)
#> ✔ Got fontawesome 0.5.3 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.40 MB)
#> ✔ Got MatrixModels 0.5-4 (x86_64-pc-linux-gnu-ubuntu-24.04) (408.50 kB)
#> ✔ Got rappdirs 0.3.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (45.71 kB)
#> ✔ Got processx 3.8.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (337.39 kB)
#> ✔ Got proxy 0.4-29 (x86_64-pc-linux-gnu-ubuntu-24.04) (172.19 kB)
#> ✔ Got knitr 1.51 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.05 MB)
#> ✔ Got rstudioapi 0.18.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (346.01 kB)
#> ✔ Got utf8 1.2.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (151.81 kB)
#> ✔ Got svglite 2.2.2 (x86_64-pc-linux-gnu-ubuntu-24.04) (239.36 kB)
#> ✔ Got vdiffr 1.0.9 (x86_64-pc-linux-gnu-ubuntu-24.04) (213.10 kB)
#> ✔ Got data.table 1.18.2.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (2.97 MB)
#> ✔ Got tidyr 1.3.2 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.18 MB)
#> ✔ Got htmltools 0.5.9 (x86_64-pc-linux-gnu-ubuntu-24.04) (354.78 kB)
#> ✔ Got highr 0.11 (x86_64-pc-linux-gnu-ubuntu-24.04) (37.50 kB)
#> ✔ Got checkmate 2.3.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (767.77 kB)
#> ✔ Got htmlwidgets 1.6.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (815.05 kB)
#> ✔ Got colorspace 2.1-2 (x86_64-pc-linux-gnu-ubuntu-24.04) (2.64 MB)
#> ✔ Got stringi 1.8.7 (x86_64-pc-linux-gnu-ubuntu-24.04) (3.29 MB)
#> ✔ Got ragg 1.5.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (743.40 kB)
#> ✔ Got yaml 2.3.12 (x86_64-pc-linux-gnu-ubuntu-24.04) (121.13 kB)
#> ✔ Got isoband 0.3.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.68 MB)
#> ✔ Got units 1.0-0 (x86_64-pc-linux-gnu-ubuntu-24.04) (478.58 kB)
#> ✔ Got farver 2.1.2 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.47 MB)
#> ✔ Got mime 0.13 (x86_64-pc-linux-gnu-ubuntu-24.04) (44.52 kB)
#> ✔ Got sass 0.4.10 (x86_64-pc-linux-gnu-ubuntu-24.04) (2.46 MB)
#> ✔ Got rmarkdown 2.30 (x86_64-pc-linux-gnu-ubuntu-24.04) (2.64 MB)
#> ✔ Got textshaping 1.0.4 (x86_64-pc-linux-gnu-ubuntu-24.04) (190.05 kB)
#> ✔ Got TH.data 1.1-5 (x86_64-pc-linux-gnu-ubuntu-24.04) (8.76 MB)
#> ✔ Got glue 1.8.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (168.12 kB)
#> ✔ Got cachem 1.1.0 (x86_64-pc-linux-gnu-ubuntu-24.04) (67.49 kB)
#> ✔ Got memoise 2.0.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (48.86 kB)
#> ✔ Got callr 3.7.6 (x86_64-pc-linux-gnu-ubuntu-24.04) (449.24 kB)
#> ✔ Got ggplot2 4.0.2 (x86_64-pc-linux-gnu-ubuntu-24.04) (8.48 MB)
#> ✔ Got quantreg 6.1 (x86_64-pc-linux-gnu-ubuntu-24.04) (1.46 MB)
#> ✔ Got Hmisc 5.2-5 (x86_64-pc-linux-gnu-ubuntu-24.04) (3.64 MB)
#> ✔ Got sf 1.0-24 (x86_64-pc-linux-gnu-ubuntu-24.04) (9.47 MB)
#> ℹ Installing system requirements
#> ℹ Executing `sudo sh -c apt-get -y update`
#> Get:1 file:/etc/apt/apt-mirrors.txt Mirrorlist [144 B]
#> Hit:2 http://azure.archive.ubuntu.com/ubuntu noble InRelease
#> Hit:3 http://azure.archive.ubuntu.com/ubuntu noble-updates InRelease
#> Hit:4 http://azure.archive.ubuntu.com/ubuntu noble-backports InRelease
#> Hit:5 http://azure.archive.ubuntu.com/ubuntu noble-security InRelease
#> Hit:6 https://packages.microsoft.com/repos/azure-cli noble InRelease
#> Hit:7 https://packages.microsoft.com/ubuntu/24.04/prod noble InRelease
#> Reading package lists...
#> ℹ Executing `sudo sh -c apt-get -y install libcurl4-openssl-dev libssl-dev make pandoc libfreetype6-dev libjpeg-dev libpng-dev libtiff-dev libwebp-dev libabsl-dev cmake libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libicu-dev libfontconfig1-dev libfribidi-dev libharfbuzz-dev libudunits2-dev libxml2-dev`
#> Reading package lists...
#> Building dependency tree...
#> Reading state information...
#> libcurl4-openssl-dev is already the newest version (8.5.0-2ubuntu10.6).
#> libssl-dev is already the newest version (3.0.13-0ubuntu3.7).
#> make is already the newest version (4.3-4.1build2).
#> pandoc is already the newest version (3.1.3+ds-2).
#> libfreetype-dev is already the newest version (2.13.2+dfsg-1build3).
#> libjpeg-dev is already the newest version (8c-2ubuntu11).
#> libpng-dev is already the newest version (1.6.43-5ubuntu0.5).
#> libtiff-dev is already the newest version (4.5.1+git230720-4ubuntu2.4).
#> libwebp-dev is already the newest version (1.3.2-0.4build3).
#> libsqlite3-dev is already the newest version (3.45.1-1ubuntu2.5).
#> libicu-dev is already the newest version (74.2-1ubuntu3.1).
#> libfontconfig1-dev is already the newest version (2.15.0-1.1ubuntu2).
#> libfribidi-dev is already the newest version (1.0.13-3build1).
#> libharfbuzz-dev is already the newest version (8.3.0-2build2).
#> libxml2-dev is already the newest version (2.9.14+dfsg-1.3ubuntu3.7).
#> The following additional packages will be installed:
#> cmake-data default-libmysqlclient-dev gdal-data gdal-plugins hdf5-helpers
#>   libabsl20220623t64 libaec-dev libaec0 libaom-dev libarmadillo-dev
#>   libarmadillo12 libarpack2-dev libarpack2t64 libblosc-dev libblosc1
#>   libboost-dev libboost1.83-dev libcfitsio-dev libcfitsio-doc libcfitsio10t64
#>   libdav1d-dev libdav1d7 libde265-dev libfreexl-dev libfreexl1 libfyba-dev
#>   libfyba0t64 libgdal34t64 libgeos-c1t64 libgeos3.12.1t64 libgeotiff-dev
#>   libgeotiff5 libgif-dev libgif7 libhdf4-0-alt libhdf4-alt-dev
#>   libhdf5-103-1t64 libhdf5-cpp-103-1t64 libhdf5-dev libhdf5-fortran-102t64
#>   libhdf5-hl-100t64 libhdf5-hl-cpp-100t64 libhdf5-hl-fortran-100t64
#>   libheif-dev libjson-c-dev libjsoncpp25 libkml-dev libkmlbase1t64
#>   libkmlconvenience1t64 libkmldom1t64 libkmlengine1t64 libkmlregionator1t64
#>   libkmlxsd1t64 libltdl-dev libminizip-dev libminizip1t64 libnetcdf-dev
#>   libnetcdf19t64 libodbccr2 libodbcinst2 libogdi-dev libogdi4.1
#>   libopenjp2-7-dev libpoppler-dev libpoppler-private-dev libpoppler134
#>   libproj25 libqhull-dev libqhull-r8.0 libqhull8.0 libqhullcpp8.0 librhash0
#>   librttopo-dev librttopo1 libspatialite-dev libspatialite8t64 libsuperlu-dev
#>   libsuperlu6 libsz2 libudunits2-0 libudunits2-data liburiparser-dev
#>   liburiparser1 libx265-199 libx265-dev libxerces-c-dev libxerces-c3.2t64
#>   proj-bin proj-data python3-gdal python3-numpy unixodbc-common unixodbc-dev
#> Suggested packages:
#>   cmake-doc cmake-format elpa-cmake-mode ninja-build libgdal-grass libitpp-dev
#>   libboost-doc libboost1.83-doc libboost-atomic1.83-dev
#>   libboost-chrono1.83-dev libboost-container1.83-dev libboost-context1.83-dev
#>   libboost-contract1.83-dev libboost-coroutine1.83-dev
#>   libboost-date-time1.83-dev libboost-exception1.83-dev libboost-fiber1.83-dev
#>   libboost-filesystem1.83-dev libboost-graph-parallel1.83-dev
#>   libboost-graph1.83-dev libboost-iostreams1.83-dev libboost-json1.83-dev
#>   libboost-locale1.83-dev libboost-log1.83-dev libboost-math1.83-dev
#>   libboost-mpi-python1.83-dev libboost-mpi1.83-dev libboost-nowide1.83-dev
#>   libboost-numpy1.83-dev libboost-program-options1.83-dev
#>   libboost-python1.83-dev libboost-random1.83-dev libboost-regex1.83-dev
#>   libboost-serialization1.83-dev libboost-stacktrace1.83-dev
#>   libboost-system1.83-dev libboost-test1.83-dev libboost-thread1.83-dev
#>   libboost-timer1.83-dev libboost-type-erasure1.83-dev libboost-url1.83-dev
#>   libboost-wave1.83-dev libboost1.83-tools-dev libmpfrc++-dev libntl-dev
#>   libgdal-doc libgeotiff-epsg geotiff-bin libhdf4-doc hdf4-tools libhdf5-doc
#>   libtool-doc netcdf-bin netcdf-doc ogdi-bin libsuperlu-doc libx265-doc
#>   libxerces-c-doc python3-pytest
#> The following NEW packages will be installed:
#>   cmake cmake-data default-libmysqlclient-dev gdal-bin gdal-data gdal-plugins
#>   hdf5-helpers libabsl-dev libabsl20220623t64 libaec-dev libaec0 libaom-dev
#>   libarmadillo-dev libarmadillo12 libarpack2-dev libarpack2t64 libblosc-dev
#>   libblosc1 libboost-dev libboost1.83-dev libcfitsio-dev libcfitsio-doc
#>   libcfitsio10t64 libdav1d-dev libdav1d7 libde265-dev libfreexl-dev libfreexl1
#>   libfyba-dev libfyba0t64 libgdal-dev libgdal34t64 libgeos-c1t64 libgeos-dev
#> libgeos3.12.1t64 libgeotiff-dev libgeotiff5 libgif-dev libgif7 libhdf4-0-alt
#>   libhdf4-alt-dev libhdf5-103-1t64 libhdf5-cpp-103-1t64 libhdf5-dev
#>   libhdf5-fortran-102t64 libhdf5-hl-100t64 libhdf5-hl-cpp-100t64
#>   libhdf5-hl-fortran-100t64 libheif-dev libjson-c-dev libjsoncpp25 libkml-dev
#>   libkmlbase1t64 libkmlconvenience1t64 libkmldom1t64 libkmlengine1t64
#>   libkmlregionator1t64 libkmlxsd1t64 libltdl-dev libminizip-dev libminizip1t64
#>   libnetcdf-dev libnetcdf19t64 libodbccr2 libodbcinst2 libogdi-dev libogdi4.1
#>   libopenjp2-7-dev libpoppler-dev libpoppler-private-dev libpoppler134
#>   libproj-dev libproj25 libqhull-dev libqhull-r8.0 libqhull8.0 libqhullcpp8.0
#>   librhash0 librttopo-dev librttopo1 libspatialite-dev libspatialite8t64
#>   libsuperlu-dev libsuperlu6 libsz2 libudunits2-0 libudunits2-data
#>   libudunits2-dev liburiparser-dev liburiparser1 libx265-199 libx265-dev
#>   libxerces-c-dev libxerces-c3.2t64 proj-bin proj-data python3-gdal
#>   python3-numpy unixodbc-common unixodbc-dev
#> 0 upgraded, 100 newly installed, 0 to remove and 147 not upgraded.
#> Need to get 78.8 MB of archives.
#> After this operation, 475 MB of additional disk space will be used.
#> Get:1 file:/etc/apt/apt-mirrors.txt Mirrorlist [144 B]
#> Get:2 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 libjsoncpp25 amd64 1.9.5-6build1 [82.8 kB]
#> Get:3 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 librhash0 amd64 1.4.3-3build1 [129 kB]
#> Get:4 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 cmake-data all 3.28.3-1build7 [2155 kB]
#> Get:5 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 cmake amd64 3.28.3-1build7 [11.2 MB]
#> Get:6 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 default-libmysqlclient-dev amd64 1.1.0build1 [3132 B]
#> Get:7 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 python3-numpy amd64 1:1.26.4+ds-6ubuntu1 [4437 kB]
#> Get:8 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 gdal-data all 3.8.4+dfsg-3ubuntu3 [261 kB]
#> Get:9 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 gdal-plugins amd64 3.8.4+dfsg-3ubuntu3 [24.8 kB]
#> Get:10 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libaec0 amd64 1.1.2-1build1 [22.9 kB]
#> Get:11 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libarpack2t64 amd64 3.9.1-1.1build2 [106 kB]
#> Get:12 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libsuperlu6 amd64 6.0.1+dfsg1-1build1 [180 kB]
#> Get:13 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libarmadillo12 amd64 1:12.6.7+dfsg-1build2 [106 kB]
#> Get:14 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libblosc1 amd64 1.21.5+ds-1build1 [36.2 kB]
#> Get:15 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libcfitsio10t64 amd64 4.3.1-1.1build2 [528 kB]
#> Get:16 http://azure.archive.ubuntu.com/ubuntu noble-updates/universe amd64 libminizip1t64 amd64 1:1.3.dfsg-3.1ubuntu2.1 [22.2 kB]
#> Get:17 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libfreexl1 amd64 2.0.0-1build2 [41.7 kB]
#> Get:18 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libfyba0t64 amd64 4.1.1-11build1 [119 kB]
#> Get:19 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libgeos3.12.1t64 amd64 3.12.1-3build1 [849 kB]
#> Get:20 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libgeos-c1t64 amd64 3.12.1-3build1 [94.5 kB]
#> Get:21 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 proj-data all 9.4.0-1build2 [7885 kB]
#> Get:22 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libproj25 amd64 9.4.0-1build2 [1396 kB]
#> Get:23 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libgeotiff5 amd64 1.7.1-5build1 [62.9 kB]
#> Get:24 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 libgif7 amd64 5.2.2-1ubuntu1 [35.2 kB]
#> Get:25 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libhdf4-0-alt amd64 4.2.16-4build1 [282 kB]
#> Get:26 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libsz2 amd64 1.1.2-1build1 [5476 B]
#> Get:27 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libhdf5-103-1t64 amd64 1.10.10+repack-3.1ubuntu4 [1270 kB]
#> Get:28 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 liburiparser1 amd64 0.9.7+dfsg-2build1 [35.8 kB]
#> Get:29 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libkmlbase1t64 amd64 1.3.0-12build1 [49.9 kB]
#> Get:30 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libkmldom1t64 amd64 1.3.0-12build1 [156 kB]
#> Get:31 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libkmlengine1t64 amd64 1.3.0-12build1 [71.4 kB]
#> Get:32 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libhdf5-hl-100t64 amd64 1.10.10+repack-3.1ubuntu4 [56.0 kB]
#> Get:33 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libnetcdf19t64 amd64 1:4.9.2-5ubuntu4 [473 kB]
#> Get:34 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 unixodbc-common all 2.3.12-1ubuntu0.24.04.1 [8806 B]
#> Get:35 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 libodbcinst2 amd64 2.3.12-1ubuntu0.24.04.1 [30.7 kB]
#> Get:36 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libogdi4.1 amd64 4.1.1+ds-3build1 [226 kB]
#> Get:37 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 libpoppler134 amd64 24.02.0-1ubuntu9.8 [1118 kB]
#> Get:38 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libqhull-r8.0 amd64 2020.2-6build1 [193 kB]
#> Get:39 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 librttopo1 amd64 1.1.0-3build2 [191 kB]
#> Get:40 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libspatialite8t64 amd64 5.1.0-3build1 [1919 kB]
#> Get:41 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libxerces-c3.2t64 amd64 3.2.4+debian-1.2ubuntu2 [919 kB]
#> Get:42 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libgdal34t64 amd64 3.8.4+dfsg-3ubuntu3 [8346 kB]
#> Get:43 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 python3-gdal amd64 3.8.4+dfsg-3ubuntu3 [754 kB]
#> Get:44 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 gdal-bin amd64 3.8.4+dfsg-3ubuntu3 [277 kB]
#> Get:45 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 hdf5-helpers amd64 1.10.10+repack-3.1ubuntu4 [15.8 kB]
#> Get:46 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 libabsl20220623t64 amd64 20220623.1-3.1ubuntu3.2 [423 kB]
#> Get:47 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 libabsl-dev amd64 20220623.1-3.1ubuntu3.2 [1020 kB]
#> Get:48 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 libaom-dev amd64 3.8.2-2ubuntu0.1 [2266 kB]
#> Get:49 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libarpack2-dev amd64 3.9.1-1.1build2 [122 kB]
#> Get:50 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libhdf5-fortran-102t64 amd64 1.10.10+repack-3.1ubuntu4 [85.2 kB]
#> Get:51 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libhdf5-hl-fortran-100t64 amd64 1.10.10+repack-3.1ubuntu4 [31.5 kB]
#> Get:52 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libhdf5-cpp-103-1t64 amd64 1.10.10+repack-3.1ubuntu4 [128 kB]
#> Get:53 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libhdf5-hl-cpp-100t64 amd64 1.10.10+repack-3.1ubuntu4 [11.2 kB]
#> Get:54 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libaec-dev amd64 1.1.2-1build1 [19.8 kB]
#> Get:55 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libhdf5-dev amd64 1.10.10+repack-3.1ubuntu4 [2777 kB]
#> Get:56 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libsuperlu-dev amd64 6.0.1+dfsg1-1build1 [20.7 kB]
#> Get:57 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libarmadillo-dev amd64 1:12.6.7+dfsg-1build2 [409 kB]
#> Get:58 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libblosc-dev amd64 1.21.5+ds-1build1 [43.7 kB]
#> Get:59 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 libboost1.83-dev amd64 1.83.0-2.1ubuntu3.2 [10.7 MB]
#> Get:60 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 libboost-dev amd64 1.83.0.1ubuntu2 [4308 B]
#> Get:61 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libcfitsio-dev amd64 4.3.1-1.1build2 [598 kB]
#> Get:62 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libcfitsio-doc all 4.3.1-1.1build2 [2076 kB]
#> Get:63 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libdav1d7 amd64 1.4.1-1build1 [604 kB]
#> Get:64 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libdav1d-dev amd64 1.4.1-1build1 [25.3 kB]
#> Get:65 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 libde265-dev amd64 1.0.15-1build3 [12.6 kB]
#> Get:66 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libfyba-dev amd64 4.1.1-11build1 [178 kB]
#> Get:67 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libgeos-dev amd64 3.12.1-3build1 [54.4 kB]
#> Get:68 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 libgif-dev amd64 5.2.2-1ubuntu1 [22.7 kB]
#> Get:69 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libx265-199 amd64 3.5-2build1 [1226 kB]
#> Get:70 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libx265-dev amd64 3.5-2build1 [1351 kB]
#> Get:71 http://azure.archive.ubuntu.com/ubuntu noble-updates/universe amd64 libheif-dev amd64 1.17.6-1ubuntu4.2 [41.6 kB]
#> Get:72 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libkmlconvenience1t64 amd64 1.3.0-12build1 [49.8 kB]
#> Get:73 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libkmlregionator1t64 amd64 1.3.0-12build1 [21.4 kB]
#> Get:74 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libkmlxsd1t64 amd64 1.3.0-12build1 [29.7 kB]
#> Get:75 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 libltdl-dev amd64 2.4.7-7build1 [168 kB]
#> Get:76 http://azure.archive.ubuntu.com/ubuntu noble-updates/universe amd64 libminizip-dev amd64 1:1.3.dfsg-3.1ubuntu2.1 [30.2 kB]
#> Get:77 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libnetcdf-dev amd64 1:4.9.2-5ubuntu4 [58.0 kB]
#> Get:78 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 libodbccr2 amd64 2.3.12-1ubuntu0.24.04.1 [16.1 kB]
#> Get:79 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libogdi-dev amd64 4.1.1+ds-3build1 [22.8 kB]
#> Get:80 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 libpoppler-dev amd64 24.02.0-1ubuntu9.8 [5200 B]
#> Get:81 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 libpoppler-private-dev amd64 24.02.0-1ubuntu9.8 [197 kB]
#> Get:82 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libqhull8.0 amd64 2020.2-6build1 [192 kB]
#> Get:83 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libqhullcpp8.0 amd64 2020.2-6build1 [51.7 kB]
#> Get:84 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libqhull-dev amd64 2020.2-6build1 [496 kB]
#> Get:85 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 librttopo-dev amd64 1.1.0-3build2 [238 kB]
#> Get:86 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libudunits2-data all 2.2.28-7build1 [19.4 kB]
#> Get:87 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libudunits2-0 amd64 2.2.28-7build1 [54.1 kB]
#> Get:88 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libudunits2-dev amd64 2.2.28-7build1 [359 kB]
#> Get:89 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 liburiparser-dev amd64 0.9.7+dfsg-2build1 [15.0 kB]
#> Get:90 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libxerces-c-dev amd64 3.2.4+debian-1.2ubuntu2 [1815 kB]
#> Get:91 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libfreexl-dev amd64 2.0.0-1build2 [43.8 kB]
#> Get:92 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libproj-dev amd64 9.4.0-1build2 [75.9 kB]
#> Get:93 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libgeotiff-dev amd64 1.7.1-5build1 [88.7 kB]
#> Get:94 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libhdf4-alt-dev amd64 4.2.16-4build1 [387 kB]
#> Get:95 http://azure.archive.ubuntu.com/ubuntu noble/main amd64 libjson-c-dev amd64 0.17-1build1 [63.6 kB]
#> Get:96 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libkml-dev amd64 1.3.0-12build1 [659 kB]
#> Get:97 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 libopenjp2-7-dev amd64 2.5.0-2ubuntu0.4 [258 kB]
#> Get:98 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libspatialite-dev amd64 5.1.0-3build1 [2331 kB]
#> Get:99 http://azure.archive.ubuntu.com/ubuntu noble-updates/main amd64 unixodbc-dev amd64 2.3.12-1ubuntu0.24.04.1 [250 kB]
#> Get:100 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 libgdal-dev amd64 3.8.4+dfsg-3ubuntu3 [211 kB]
#> Get:101 http://azure.archive.ubuntu.com/ubuntu noble/universe amd64 proj-bin amd64 9.4.0-1build2 [164 kB]
#> Fetched 78.8 MB in 9s (8548 kB/s)
#> Selecting previously unselected package libjsoncpp25:amd64.
#> (Reading database ...
#> (Reading database ... 5%(Reading database ... 10%(Reading database ... 15%(Reading database ... 20%(Reading database ... 25%(Reading database ... 30%(Reading database ... 35%(Reading database ... 40%(Reading database ... 45%(Reading database ... 50%(Reading database ... 55%
#> (Reading database ... 60%
#> (Reading database ... 65%
#> (Reading database ... 70%
#> (Reading database ... 75%
#> (Reading database ... 80%
#> (Reading database ... 85%
#> (Reading database ... 90%
#> (Reading database ... 95%
#> (Reading database ... 100%(Reading database ... 229541 files and directories currently installed.)
#> Preparing to unpack .../00-libjsoncpp25_1.9.5-6build1_amd64.deb ...
#> Unpacking libjsoncpp25:amd64 (1.9.5-6build1) ...
#> Selecting previously unselected package librhash0:amd64.
#> Preparing to unpack .../01-librhash0_1.4.3-3build1_amd64.deb ...
#> Unpacking librhash0:amd64 (1.4.3-3build1) ...
#> Selecting previously unselected package cmake-data.
#> Preparing to unpack .../02-cmake-data_3.28.3-1build7_all.deb ...
#> Unpacking cmake-data (3.28.3-1build7) ...
#> Selecting previously unselected package cmake.
#> Preparing to unpack .../03-cmake_3.28.3-1build7_amd64.deb ...
#> Unpacking cmake (3.28.3-1build7) ...
#> Selecting previously unselected package default-libmysqlclient-dev:amd64.
#> Preparing to unpack .../04-default-libmysqlclient-dev_1.1.0build1_amd64.deb ...
#> Unpacking default-libmysqlclient-dev:amd64 (1.1.0build1) ...
#> Selecting previously unselected package python3-numpy.
#> Preparing to unpack .../05-python3-numpy_1%3a1.26.4+ds-6ubuntu1_amd64.deb ...
#> Unpacking python3-numpy (1:1.26.4+ds-6ubuntu1) ...
#> Selecting previously unselected package gdal-data.
#> Preparing to unpack .../06-gdal-data_3.8.4+dfsg-3ubuntu3_all.deb ...
#> Unpacking gdal-data (3.8.4+dfsg-3ubuntu3) ...
#> Selecting previously unselected package gdal-plugins:amd64.
#> Preparing to unpack .../07-gdal-plugins_3.8.4+dfsg-3ubuntu3_amd64.deb ...
#> Unpacking gdal-plugins:amd64 (3.8.4+dfsg-3ubuntu3) ...
#> Selecting previously unselected package libaec0:amd64.
#> Preparing to unpack .../08-libaec0_1.1.2-1build1_amd64.deb ...
#> Unpacking libaec0:amd64 (1.1.2-1build1) ...
#> Selecting previously unselected package libarpack2t64:amd64.
#> Preparing to unpack .../09-libarpack2t64_3.9.1-1.1build2_amd64.deb ...
#> Unpacking libarpack2t64:amd64 (3.9.1-1.1build2) ...
#> Selecting previously unselected package libsuperlu6:amd64.
#> Preparing to unpack .../10-libsuperlu6_6.0.1+dfsg1-1build1_amd64.deb ...
#> Unpacking libsuperlu6:amd64 (6.0.1+dfsg1-1build1) ...
#> Selecting previously unselected package libarmadillo12.
#> Preparing to unpack .../11-libarmadillo12_1%3a12.6.7+dfsg-1build2_amd64.deb ...
#> Unpacking libarmadillo12 (1:12.6.7+dfsg-1build2) ...
#> Selecting previously unselected package libblosc1:amd64.
#> Preparing to unpack .../12-libblosc1_1.21.5+ds-1build1_amd64.deb ...
#> Unpacking libblosc1:amd64 (1.21.5+ds-1build1) ...
#> Selecting previously unselected package libcfitsio10t64:amd64.
#> Preparing to unpack .../13-libcfitsio10t64_4.3.1-1.1build2_amd64.deb ...
#> Unpacking libcfitsio10t64:amd64 (4.3.1-1.1build2) ...
#> Selecting previously unselected package libminizip1t64:amd64.
#> Preparing to unpack .../14-libminizip1t64_1%3a1.3.dfsg-3.1ubuntu2.1_amd64.deb ...
#> Unpacking libminizip1t64:amd64 (1:1.3.dfsg-3.1ubuntu2.1) ...
#> Selecting previously unselected package libfreexl1:amd64.
#> Preparing to unpack .../15-libfreexl1_2.0.0-1build2_amd64.deb ...
#> Unpacking libfreexl1:amd64 (2.0.0-1build2) ...
#> Selecting previously unselected package libfyba0t64:amd64.
#> Preparing to unpack .../16-libfyba0t64_4.1.1-11build1_amd64.deb ...
#> Unpacking libfyba0t64:amd64 (4.1.1-11build1) ...
#> Selecting previously unselected package libgeos3.12.1t64:amd64.
#> Preparing to unpack .../17-libgeos3.12.1t64_3.12.1-3build1_amd64.deb ...
#> Unpacking libgeos3.12.1t64:amd64 (3.12.1-3build1) ...
#> Selecting previously unselected package libgeos-c1t64:amd64.
#> Preparing to unpack .../18-libgeos-c1t64_3.12.1-3build1_amd64.deb ...
#> Unpacking libgeos-c1t64:amd64 (3.12.1-3build1) ...
#> Selecting previously unselected package proj-data.
#> Preparing to unpack .../19-proj-data_9.4.0-1build2_all.deb ...
#> Unpacking proj-data (9.4.0-1build2) ...
#> Selecting previously unselected package libproj25:amd64.
#> Preparing to unpack .../20-libproj25_9.4.0-1build2_amd64.deb ...
#> Unpacking libproj25:amd64 (9.4.0-1build2) ...
#> Selecting previously unselected package libgeotiff5:amd64.
#> Preparing to unpack .../21-libgeotiff5_1.7.1-5build1_amd64.deb ...
#> Unpacking libgeotiff5:amd64 (1.7.1-5build1) ...
#> Selecting previously unselected package libgif7:amd64.
#> Preparing to unpack .../22-libgif7_5.2.2-1ubuntu1_amd64.deb ...
#> Unpacking libgif7:amd64 (5.2.2-1ubuntu1) ...
#> Selecting previously unselected package libhdf4-0-alt:amd64.
#> Preparing to unpack .../23-libhdf4-0-alt_4.2.16-4build1_amd64.deb ...
#> Unpacking libhdf4-0-alt:amd64 (4.2.16-4build1) ...
#> Selecting previously unselected package libsz2:amd64.
#> Preparing to unpack .../24-libsz2_1.1.2-1build1_amd64.deb ...
#> Unpacking libsz2:amd64 (1.1.2-1build1) ...
#> Selecting previously unselected package libhdf5-103-1t64:amd64.
#> Preparing to unpack .../25-libhdf5-103-1t64_1.10.10+repack-3.1ubuntu4_amd64.deb ...
#> Unpacking libhdf5-103-1t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Selecting previously unselected package liburiparser1:amd64.
#> Preparing to unpack .../26-liburiparser1_0.9.7+dfsg-2build1_amd64.deb ...
#> Unpacking liburiparser1:amd64 (0.9.7+dfsg-2build1) ...
#> Selecting previously unselected package libkmlbase1t64:amd64.
#> Preparing to unpack .../27-libkmlbase1t64_1.3.0-12build1_amd64.deb ...
#> Unpacking libkmlbase1t64:amd64 (1.3.0-12build1) ...
#> Selecting previously unselected package libkmldom1t64:amd64.
#> Preparing to unpack .../28-libkmldom1t64_1.3.0-12build1_amd64.deb ...
#> Unpacking libkmldom1t64:amd64 (1.3.0-12build1) ...
#> Selecting previously unselected package libkmlengine1t64:amd64.
#> Preparing to unpack .../29-libkmlengine1t64_1.3.0-12build1_amd64.deb ...
#> Unpacking libkmlengine1t64:amd64 (1.3.0-12build1) ...
#> Selecting previously unselected package libhdf5-hl-100t64:amd64.
#> Preparing to unpack .../30-libhdf5-hl-100t64_1.10.10+repack-3.1ubuntu4_amd64.deb ...
#> Unpacking libhdf5-hl-100t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Selecting previously unselected package libnetcdf19t64:amd64.
#> Preparing to unpack .../31-libnetcdf19t64_1%3a4.9.2-5ubuntu4_amd64.deb ...
#> Unpacking libnetcdf19t64:amd64 (1:4.9.2-5ubuntu4) ...
#> Selecting previously unselected package unixodbc-common.
#> Preparing to unpack .../32-unixodbc-common_2.3.12-1ubuntu0.24.04.1_all.deb ...
#> Unpacking unixodbc-common (2.3.12-1ubuntu0.24.04.1) ...
#> Selecting previously unselected package libodbcinst2:amd64.
#> Preparing to unpack .../33-libodbcinst2_2.3.12-1ubuntu0.24.04.1_amd64.deb ...
#> Unpacking libodbcinst2:amd64 (2.3.12-1ubuntu0.24.04.1) ...
#> Selecting previously unselected package libogdi4.1:amd64.
#> Preparing to unpack .../34-libogdi4.1_4.1.1+ds-3build1_amd64.deb ...
#> Unpacking libogdi4.1:amd64 (4.1.1+ds-3build1) ...
#> Selecting previously unselected package libpoppler134:amd64.
#> Preparing to unpack .../35-libpoppler134_24.02.0-1ubuntu9.8_amd64.deb ...
#> Unpacking libpoppler134:amd64 (24.02.0-1ubuntu9.8) ...
#> Selecting previously unselected package libqhull-r8.0:amd64.
#> Preparing to unpack .../36-libqhull-r8.0_2020.2-6build1_amd64.deb ...
#> Unpacking libqhull-r8.0:amd64 (2020.2-6build1) ...
#> Selecting previously unselected package librttopo1:amd64.
#> Preparing to unpack .../37-librttopo1_1.1.0-3build2_amd64.deb ...
#> Unpacking librttopo1:amd64 (1.1.0-3build2) ...
#> Selecting previously unselected package libspatialite8t64:amd64.
#> Preparing to unpack .../38-libspatialite8t64_5.1.0-3build1_amd64.deb ...
#> Unpacking libspatialite8t64:amd64 (5.1.0-3build1) ...
#> Selecting previously unselected package libxerces-c3.2t64:amd64.
#> Preparing to unpack .../39-libxerces-c3.2t64_3.2.4+debian-1.2ubuntu2_amd64.deb ...
#> Unpacking libxerces-c3.2t64:amd64 (3.2.4+debian-1.2ubuntu2) ...
#> Selecting previously unselected package libgdal34t64:amd64.
#> Preparing to unpack .../40-libgdal34t64_3.8.4+dfsg-3ubuntu3_amd64.deb ...
#> Unpacking libgdal34t64:amd64 (3.8.4+dfsg-3ubuntu3) ...
#> Selecting previously unselected package python3-gdal.
#> Preparing to unpack .../41-python3-gdal_3.8.4+dfsg-3ubuntu3_amd64.deb ...
#> Unpacking python3-gdal (3.8.4+dfsg-3ubuntu3) ...
#> Selecting previously unselected package gdal-bin.
#> Preparing to unpack .../42-gdal-bin_3.8.4+dfsg-3ubuntu3_amd64.deb ...
#> Unpacking gdal-bin (3.8.4+dfsg-3ubuntu3) ...
#> Selecting previously unselected package hdf5-helpers.
#> Preparing to unpack .../43-hdf5-helpers_1.10.10+repack-3.1ubuntu4_amd64.deb ...
#> Unpacking hdf5-helpers (1.10.10+repack-3.1ubuntu4) ...
#> Selecting previously unselected package libabsl20220623t64:amd64.
#> Preparing to unpack .../44-libabsl20220623t64_20220623.1-3.1ubuntu3.2_amd64.deb ...
#> Unpacking libabsl20220623t64:amd64 (20220623.1-3.1ubuntu3.2) ...
#> Selecting previously unselected package libabsl-dev:amd64.
#> Preparing to unpack .../45-libabsl-dev_20220623.1-3.1ubuntu3.2_amd64.deb ...
#> Unpacking libabsl-dev:amd64 (20220623.1-3.1ubuntu3.2) ...
#> Selecting previously unselected package libaom-dev:amd64.
#> Preparing to unpack .../46-libaom-dev_3.8.2-2ubuntu0.1_amd64.deb ...
#> Unpacking libaom-dev:amd64 (3.8.2-2ubuntu0.1) ...
#> Selecting previously unselected package libarpack2-dev:amd64.
#> Preparing to unpack .../47-libarpack2-dev_3.9.1-1.1build2_amd64.deb ...
#> Unpacking libarpack2-dev:amd64 (3.9.1-1.1build2) ...
#> Selecting previously unselected package libhdf5-fortran-102t64:amd64.
#> Preparing to unpack .../48-libhdf5-fortran-102t64_1.10.10+repack-3.1ubuntu4_amd64.deb ...
#> Unpacking libhdf5-fortran-102t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Selecting previously unselected package libhdf5-hl-fortran-100t64:amd64.
#> Preparing to unpack .../49-libhdf5-hl-fortran-100t64_1.10.10+repack-3.1ubuntu4_amd64.deb ...
#> Unpacking libhdf5-hl-fortran-100t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Selecting previously unselected package libhdf5-cpp-103-1t64:amd64.
#> Preparing to unpack .../50-libhdf5-cpp-103-1t64_1.10.10+repack-3.1ubuntu4_amd64.deb ...
#> Unpacking libhdf5-cpp-103-1t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Selecting previously unselected package libhdf5-hl-cpp-100t64:amd64.
#> Preparing to unpack .../51-libhdf5-hl-cpp-100t64_1.10.10+repack-3.1ubuntu4_amd64.deb ...
#> Unpacking libhdf5-hl-cpp-100t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Selecting previously unselected package libaec-dev:amd64.
#> Preparing to unpack .../52-libaec-dev_1.1.2-1build1_amd64.deb ...
#> Unpacking libaec-dev:amd64 (1.1.2-1build1) ...
#> Selecting previously unselected package libhdf5-dev.
#> Preparing to unpack .../53-libhdf5-dev_1.10.10+repack-3.1ubuntu4_amd64.deb ...
#> Unpacking libhdf5-dev (1.10.10+repack-3.1ubuntu4) ...
#> Selecting previously unselected package libsuperlu-dev:amd64.
#> Preparing to unpack .../54-libsuperlu-dev_6.0.1+dfsg1-1build1_amd64.deb ...
#> Unpacking libsuperlu-dev:amd64 (6.0.1+dfsg1-1build1) ...
#> Selecting previously unselected package libarmadillo-dev.
#> Preparing to unpack .../55-libarmadillo-dev_1%3a12.6.7+dfsg-1build2_amd64.deb ...
#> Unpacking libarmadillo-dev (1:12.6.7+dfsg-1build2) ...
#> Selecting previously unselected package libblosc-dev:amd64.
#> Preparing to unpack .../56-libblosc-dev_1.21.5+ds-1build1_amd64.deb ...
#> Unpacking libblosc-dev:amd64 (1.21.5+ds-1build1) ...
#> Selecting previously unselected package libboost1.83-dev:amd64.
#> Preparing to unpack .../57-libboost1.83-dev_1.83.0-2.1ubuntu3.2_amd64.deb ...
#> Unpacking libboost1.83-dev:amd64 (1.83.0-2.1ubuntu3.2) ...
#> Selecting previously unselected package libboost-dev:amd64.
#> Preparing to unpack .../58-libboost-dev_1.83.0.1ubuntu2_amd64.deb ...
#> Unpacking libboost-dev:amd64 (1.83.0.1ubuntu2) ...
#> Selecting previously unselected package libcfitsio-dev:amd64.
#> Preparing to unpack .../59-libcfitsio-dev_4.3.1-1.1build2_amd64.deb ...
#> Unpacking libcfitsio-dev:amd64 (4.3.1-1.1build2) ...
#> Selecting previously unselected package libcfitsio-doc.
#> Preparing to unpack .../60-libcfitsio-doc_4.3.1-1.1build2_all.deb ...
#> Unpacking libcfitsio-doc (4.3.1-1.1build2) ...
#> Selecting previously unselected package libdav1d7:amd64.
#> Preparing to unpack .../61-libdav1d7_1.4.1-1build1_amd64.deb ...
#> Unpacking libdav1d7:amd64 (1.4.1-1build1) ...
#> Selecting previously unselected package libdav1d-dev:amd64.
#> Preparing to unpack .../62-libdav1d-dev_1.4.1-1build1_amd64.deb ...
#> Unpacking libdav1d-dev:amd64 (1.4.1-1build1) ...
#> Selecting previously unselected package libde265-dev:amd64.
#> Preparing to unpack .../63-libde265-dev_1.0.15-1build3_amd64.deb ...
#> Unpacking libde265-dev:amd64 (1.0.15-1build3) ...
#> Selecting previously unselected package libfyba-dev:amd64.
#> Preparing to unpack .../64-libfyba-dev_4.1.1-11build1_amd64.deb ...
#> Unpacking libfyba-dev:amd64 (4.1.1-11build1) ...
#> Selecting previously unselected package libgeos-dev.
#> Preparing to unpack .../65-libgeos-dev_3.12.1-3build1_amd64.deb ...
#> Unpacking libgeos-dev (3.12.1-3build1) ...
#> Selecting previously unselected package libgif-dev:amd64.
#> Preparing to unpack .../66-libgif-dev_5.2.2-1ubuntu1_amd64.deb ...
#> Unpacking libgif-dev:amd64 (5.2.2-1ubuntu1) ...
#> Selecting previously unselected package libx265-199:amd64.
#> Preparing to unpack .../67-libx265-199_3.5-2build1_amd64.deb ...
#> Unpacking libx265-199:amd64 (3.5-2build1) ...
#> Selecting previously unselected package libx265-dev:amd64.
#> Preparing to unpack .../68-libx265-dev_3.5-2build1_amd64.deb ...
#> Unpacking libx265-dev:amd64 (3.5-2build1) ...
#> Selecting previously unselected package libheif-dev:amd64.
#> Preparing to unpack .../69-libheif-dev_1.17.6-1ubuntu4.2_amd64.deb ...
#> Unpacking libheif-dev:amd64 (1.17.6-1ubuntu4.2) ...
#> Selecting previously unselected package libkmlconvenience1t64:amd64.
#> Preparing to unpack .../70-libkmlconvenience1t64_1.3.0-12build1_amd64.deb ...
#> Unpacking libkmlconvenience1t64:amd64 (1.3.0-12build1) ...
#> Selecting previously unselected package libkmlregionator1t64:amd64.
#> Preparing to unpack .../71-libkmlregionator1t64_1.3.0-12build1_amd64.deb ...
#> Unpacking libkmlregionator1t64:amd64 (1.3.0-12build1) ...
#> Selecting previously unselected package libkmlxsd1t64:amd64.
#> Preparing to unpack .../72-libkmlxsd1t64_1.3.0-12build1_amd64.deb ...
#> Unpacking libkmlxsd1t64:amd64 (1.3.0-12build1) ...
#> Selecting previously unselected package libltdl-dev:amd64.
#> Preparing to unpack .../73-libltdl-dev_2.4.7-7build1_amd64.deb ...
#> Unpacking libltdl-dev:amd64 (2.4.7-7build1) ...
#> Selecting previously unselected package libminizip-dev:amd64.
#> Preparing to unpack .../74-libminizip-dev_1%3a1.3.dfsg-3.1ubuntu2.1_amd64.deb ...
#> Unpacking libminizip-dev:amd64 (1:1.3.dfsg-3.1ubuntu2.1) ...
#> Selecting previously unselected package libnetcdf-dev.
#> Preparing to unpack .../75-libnetcdf-dev_1%3a4.9.2-5ubuntu4_amd64.deb ...
#> Unpacking libnetcdf-dev (1:4.9.2-5ubuntu4) ...
#> Selecting previously unselected package libodbccr2:amd64.
#> Preparing to unpack .../76-libodbccr2_2.3.12-1ubuntu0.24.04.1_amd64.deb ...
#> Unpacking libodbccr2:amd64 (2.3.12-1ubuntu0.24.04.1) ...
#> Selecting previously unselected package libogdi-dev.
#> Preparing to unpack .../77-libogdi-dev_4.1.1+ds-3build1_amd64.deb ...
#> Unpacking libogdi-dev (4.1.1+ds-3build1) ...
#> Selecting previously unselected package libpoppler-dev:amd64.
#> Preparing to unpack .../78-libpoppler-dev_24.02.0-1ubuntu9.8_amd64.deb ...
#> Unpacking libpoppler-dev:amd64 (24.02.0-1ubuntu9.8) ...
#> Selecting previously unselected package libpoppler-private-dev:amd64.
#> Preparing to unpack .../79-libpoppler-private-dev_24.02.0-1ubuntu9.8_amd64.deb ...
#> Unpacking libpoppler-private-dev:amd64 (24.02.0-1ubuntu9.8) ...
#> Selecting previously unselected package libqhull8.0:amd64.
#> Preparing to unpack .../80-libqhull8.0_2020.2-6build1_amd64.deb ...
#> Unpacking libqhull8.0:amd64 (2020.2-6build1) ...
#> Selecting previously unselected package libqhullcpp8.0:amd64.
#> Preparing to unpack .../81-libqhullcpp8.0_2020.2-6build1_amd64.deb ...
#> Unpacking libqhullcpp8.0:amd64 (2020.2-6build1) ...
#> Selecting previously unselected package libqhull-dev:amd64.
#> Preparing to unpack .../82-libqhull-dev_2020.2-6build1_amd64.deb ...
#> Unpacking libqhull-dev:amd64 (2020.2-6build1) ...
#> Selecting previously unselected package librttopo-dev:amd64.
#> Preparing to unpack .../83-librttopo-dev_1.1.0-3build2_amd64.deb ...
#> Unpacking librttopo-dev:amd64 (1.1.0-3build2) ...
#> Selecting previously unselected package libudunits2-data.
#> Preparing to unpack .../84-libudunits2-data_2.2.28-7build1_all.deb ...
#> Unpacking libudunits2-data (2.2.28-7build1) ...
#> Selecting previously unselected package libudunits2-0.
#> Preparing to unpack .../85-libudunits2-0_2.2.28-7build1_amd64.deb ...
#> Unpacking libudunits2-0 (2.2.28-7build1) ...
#> Selecting previously unselected package libudunits2-dev.
#> Preparing to unpack .../86-libudunits2-dev_2.2.28-7build1_amd64.deb ...
#> Unpacking libudunits2-dev (2.2.28-7build1) ...
#> Selecting previously unselected package liburiparser-dev.
#> Preparing to unpack .../87-liburiparser-dev_0.9.7+dfsg-2build1_amd64.deb ...
#> Unpacking liburiparser-dev (0.9.7+dfsg-2build1) ...
#> Selecting previously unselected package libxerces-c-dev:amd64.
#> Preparing to unpack .../88-libxerces-c-dev_3.2.4+debian-1.2ubuntu2_amd64.deb ...
#> Unpacking libxerces-c-dev:amd64 (3.2.4+debian-1.2ubuntu2) ...
#> Selecting previously unselected package libfreexl-dev:amd64.
#> Preparing to unpack .../89-libfreexl-dev_2.0.0-1build2_amd64.deb ...
#> Unpacking libfreexl-dev:amd64 (2.0.0-1build2) ...
#> Selecting previously unselected package libproj-dev:amd64.
#> Preparing to unpack .../90-libproj-dev_9.4.0-1build2_amd64.deb ...
#> Unpacking libproj-dev:amd64 (9.4.0-1build2) ...
#> Selecting previously unselected package libgeotiff-dev:amd64.
#> Preparing to unpack .../91-libgeotiff-dev_1.7.1-5build1_amd64.deb ...
#> Unpacking libgeotiff-dev:amd64 (1.7.1-5build1) ...
#> Selecting previously unselected package libhdf4-alt-dev.
#> Preparing to unpack .../92-libhdf4-alt-dev_4.2.16-4build1_amd64.deb ...
#> Unpacking libhdf4-alt-dev (4.2.16-4build1) ...
#> Selecting previously unselected package libjson-c-dev:amd64.
#> Preparing to unpack .../93-libjson-c-dev_0.17-1build1_amd64.deb ...
#> Unpacking libjson-c-dev:amd64 (0.17-1build1) ...
#> Selecting previously unselected package libkml-dev:amd64.
#> Preparing to unpack .../94-libkml-dev_1.3.0-12build1_amd64.deb ...
#> Unpacking libkml-dev:amd64 (1.3.0-12build1) ...
#> Selecting previously unselected package libopenjp2-7-dev:amd64.
#> Preparing to unpack .../95-libopenjp2-7-dev_2.5.0-2ubuntu0.4_amd64.deb ...
#> Unpacking libopenjp2-7-dev:amd64 (2.5.0-2ubuntu0.4) ...
#> Selecting previously unselected package libspatialite-dev:amd64.
#> Preparing to unpack .../96-libspatialite-dev_5.1.0-3build1_amd64.deb ...
#> Unpacking libspatialite-dev:amd64 (5.1.0-3build1) ...
#> Selecting previously unselected package unixodbc-dev:amd64.
#> Preparing to unpack .../97-unixodbc-dev_2.3.12-1ubuntu0.24.04.1_amd64.deb ...
#> Unpacking unixodbc-dev:amd64 (2.3.12-1ubuntu0.24.04.1) ...
#> Selecting previously unselected package libgdal-dev.
#> Preparing to unpack .../98-libgdal-dev_3.8.4+dfsg-3ubuntu3_amd64.deb ...
#> Unpacking libgdal-dev (3.8.4+dfsg-3ubuntu3) ...
#> Selecting previously unselected package proj-bin.
#> Preparing to unpack .../99-proj-bin_9.4.0-1build2_amd64.deb ...
#> Unpacking proj-bin (9.4.0-1build2) ...
#> Setting up default-libmysqlclient-dev:amd64 (1.1.0build1) ...
#> Setting up libboost1.83-dev:amd64 (1.83.0-2.1ubuntu3.2) ...
#> Setting up libarpack2t64:amd64 (3.9.1-1.1build2) ...
#> Setting up proj-data (9.4.0-1build2) ...
#> Setting up hdf5-helpers (1.10.10+repack-3.1ubuntu4) ...
#> Setting up libproj25:amd64 (9.4.0-1build2) ...
#> Setting up libogdi4.1:amd64 (4.1.1+ds-3build1) ...
#> Setting up libpoppler134:amd64 (24.02.0-1ubuntu9.8) ...
#> Setting up libqhull8.0:amd64 (2020.2-6build1) ...
#> Setting up libgeos3.12.1t64:amd64 (3.12.1-3build1) ...
#> Setting up libjson-c-dev:amd64 (0.17-1build1) ...
#> Setting up libgeos-c1t64:amd64 (3.12.1-3build1) ...
#> Setting up proj-bin (9.4.0-1build2) ...
#> Setting up libqhull-r8.0:amd64 (2020.2-6build1) ...
#> Setting up libsuperlu6:amd64 (6.0.1+dfsg1-1build1) ...
#> Setting up libxerces-c3.2t64:amd64 (3.2.4+debian-1.2ubuntu2) ...
#> Setting up libaec0:amd64 (1.1.2-1build1) ...
#> Setting up gdal-data (3.8.4+dfsg-3ubuntu3) ...
#> Setting up libgeotiff5:amd64 (1.7.1-5build1) ...
#> Setting up libaom-dev:amd64 (3.8.2-2ubuntu0.1) ...
#> Setting up libltdl-dev:amd64 (2.4.7-7build1) ...
#> Setting up libcfitsio10t64:amd64 (4.3.1-1.1build2) ...
#> Setting up libde265-dev:amd64 (1.0.15-1build3) ...
#> Setting up libopenjp2-7-dev:amd64 (2.5.0-2ubuntu0.4) ...
#> Setting up libxerces-c-dev:amd64 (3.2.4+debian-1.2ubuntu2) ...
#> Setting up libgeos-dev (3.12.1-3build1) ...
#> Setting up libproj-dev:amd64 (9.4.0-1build2) ...
#> Setting up libjsoncpp25:amd64 (1.9.5-6build1) ...
#> Setting up unixodbc-common (2.3.12-1ubuntu0.24.04.1) ...
#> Setting up libqhullcpp8.0:amd64 (2020.2-6build1) ...
#> Setting up python3-numpy (1:1.26.4+ds-6ubuntu1) ...
#> Setting up libqhull-dev:amd64 (2020.2-6build1) ...
#> Setting up libhdf4-0-alt:amd64 (4.2.16-4build1) ...
#> Setting up libx265-199:amd64 (3.5-2build1) ...
#> Setting up libgif7:amd64 (5.2.2-1ubuntu1) ...
#> Setting up liburiparser1:amd64 (0.9.7+dfsg-2build1) ...
#> Setting up libfyba0t64:amd64 (4.1.1-11build1) ...
#> Setting up librttopo1:amd64 (1.1.0-3build2) ...
#> Setting up libudunits2-data (2.2.28-7build1) ...
#> Setting up libdav1d7:amd64 (1.4.1-1build1) ...
#> Setting up libminizip1t64:amd64 (1:1.3.dfsg-3.1ubuntu2.1) ...
#> Setting up libgif-dev:amd64 (5.2.2-1ubuntu1) ...
#> Setting up librhash0:amd64 (1.4.3-3build1) ...
#> Setting up libblosc1:amd64 (1.21.5+ds-1build1) ...
#> Setting up cmake-data (3.28.3-1build7) ...
#> Setting up libboost-dev:amd64 (1.83.0.1ubuntu2) ...
#> Setting up libabsl20220623t64:amd64 (20220623.1-3.1ubuntu3.2) ...
#> Setting up libkmlbase1t64:amd64 (1.3.0-12build1) ...
#> Setting up libsuperlu-dev:amd64 (6.0.1+dfsg1-1build1) ...
#> Setting up libogdi-dev (4.1.1+ds-3build1) ...
#> Setting up libcfitsio-doc (4.3.1-1.1build2) ...
#> Setting up libarmadillo12 (1:12.6.7+dfsg-1build2) ...
#> Setting up libsz2:amd64 (1.1.2-1build1) ...
#> Setting up libodbccr2:amd64 (2.3.12-1ubuntu0.24.04.1) ...
#> Setting up gdal-plugins:amd64 (3.8.4+dfsg-3ubuntu3) ...
#> Setting up libpoppler-dev:amd64 (24.02.0-1ubuntu9.8) ...
#> Setting up librttopo-dev:amd64 (1.1.0-3build2) ...
#> Setting up libodbcinst2:amd64 (2.3.12-1ubuntu0.24.04.1) ...
#> Setting up libkmlxsd1t64:amd64 (1.3.0-12build1) ...
#> Setting up libarpack2-dev:amd64 (3.9.1-1.1build2) ...
#> Setting up libcfitsio-dev:amd64 (4.3.1-1.1build2) ...
#> Setting up libgeotiff-dev:amd64 (1.7.1-5build1) ...
#> Setting up libblosc-dev:amd64 (1.21.5+ds-1build1) ...
#> Setting up libfyba-dev:amd64 (4.1.1-11build1) ...
#> Setting up libpoppler-private-dev:amd64 (24.02.0-1ubuntu9.8) ...
#> Setting up libaec-dev:amd64 (1.1.2-1build1) ...
#> Setting up liburiparser-dev (0.9.7+dfsg-2build1) ...
#> Setting up libkmldom1t64:amd64 (1.3.0-12build1) ...
#> Setting up libminizip-dev:amd64 (1:1.3.dfsg-3.1ubuntu2.1) ...
#> Setting up libdav1d-dev:amd64 (1.4.1-1build1) ...
#> Setting up libx265-dev:amd64 (3.5-2build1) ...
#> Setting up libabsl-dev:amd64 (20220623.1-3.1ubuntu3.2) ...
#> Setting up libfreexl1:amd64 (2.0.0-1build2) ...
#> Setting up cmake (3.28.3-1build7) ...
#> Setting up libhdf5-103-1t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Setting up unixodbc-dev:amd64 (2.3.12-1ubuntu0.24.04.1) ...
#> Setting up libspatialite8t64:amd64 (5.1.0-3build1) ...
#> Setting up libhdf5-hl-100t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Setting up libnetcdf19t64:amd64 (1:4.9.2-5ubuntu4) ...
#> Setting up libfreexl-dev:amd64 (2.0.0-1build2) ...
#> Setting up libhdf5-cpp-103-1t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Setting up libheif-dev:amd64 (1.17.6-1ubuntu4.2) ...
#> Setting up libhdf5-fortran-102t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Setting up libkmlengine1t64:amd64 (1.3.0-12build1) ...
#> Setting up libgdal34t64:amd64 (3.8.4+dfsg-3ubuntu3) ...
#> Setting up libspatialite-dev:amd64 (5.1.0-3build1) ...
#> Setting up libkmlconvenience1t64:amd64 (1.3.0-12build1) ...
#> Setting up libhdf5-hl-cpp-100t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Setting up python3-gdal (3.8.4+dfsg-3ubuntu3) ...
#> Setting up libkmlregionator1t64:amd64 (1.3.0-12build1) ...
#> Setting up libhdf5-hl-fortran-100t64:amd64 (1.10.10+repack-3.1ubuntu4) ...
#> Setting up gdal-bin (3.8.4+dfsg-3ubuntu3) ...
#> Setting up libhdf5-dev (1.10.10+repack-3.1ubuntu4) ...
#> update-alternatives: using /usr/lib/x86_64-linux-gnu/pkgconfig/hdf5-serial.pc to provide /usr/lib/x86_64-linux-gnu/pkgconfig/hdf5.pc (hdf5.pc) in auto mode
#> Setting up libnetcdf-dev (1:4.9.2-5ubuntu4) ...
#> Setting up libkml-dev:amd64 (1.3.0-12build1) ...
#> Setting up libarmadillo-dev (1:12.6.7+dfsg-1build2) ...
#> Setting up libhdf4-alt-dev (4.2.16-4build1) ...
#> Setting up libgdal-dev (3.8.4+dfsg-3ubuntu3) ...
#> Processing triggers for libc-bin (2.39-0ubuntu8.6) ...
#> Processing triggers for man-db (2.12.0-4build2) ...
#> Not building database; man-db/auto-update is not 'true'.
#> Processing triggers for sgml-base (1.31) ...
#> Processing triggers for install-info (7.1-3build2) ...
#> Setting up libudunits2-0 (2.2.28-7build1) ...
#> Setting up libudunits2-dev (2.2.28-7build1) ...
#> Processing triggers for libc-bin (2.39-0ubuntu8.6) ...
#> Running kernel seems to be up-to-date.
#> 
#> Restarting services...
#> Service restarts being deferred:
#>  systemctl restart getty@tty1.service
#>  systemctl restart networkd-dispatcher.service
#>  systemctl restart serial-getty@ttyS0.service
#>  systemctl restart systemd-logind.service
#> No containers need to be restarted.
#> 
#> User sessions running outdated binaries:
#>  runner @ user manager service: systemd[1144]
#> No VM guests are running outdated hypervisor (qemu) binaries on this host.
#> ✔ Installed askpass 1.2.1  (63ms)
#> ✔ Installed backports 1.5.0  (81ms)
#> ✔ Installed base64enc 0.1-6  (104ms)
#> ✔ Installed brew 1.0-10  (134ms)
#> ✔ Installed brio 1.1.5  (59ms)
#> ✔ Installed broom 1.0.12  (78ms)
#> ✔ Installed cachem 1.1.0  (1s)
#> ✔ Installed bslib 0.10.0  (1.1s)
#> ✔ Installed callr 3.7.6  (65ms)
#> ✔ Installed checkmate 2.3.4  (64ms)
#> ✔ Installed classInt 0.4-11  (63ms)
#> ✔ Installed cli 3.6.5  (90ms)
#> ✔ Installed colorspace 2.1-2  (92ms)
#> ✔ Installed commonmark 2.0.0  (63ms)
#> ✔ Installed covr 3.6.5  (62ms)
#> ✔ Installed crayon 1.5.3  (63ms)
#> ✔ Installed curl 7.0.0  (62ms)
#> ✔ Installed data.table 1.18.2.1  (72ms)
#> ✔ Installed DBI 1.2.3  (73ms)
#> ✔ Installed desc 1.4.3  (104ms)
#> ✔ Installed diffobj 0.3.6  (62ms)
#> ✔ Installed digest 0.6.39  (64ms)
#> ✔ Installed dplyr 1.2.0  (67ms)
#> ✔ Installed e1071 1.7-17  (63ms)
#> ✔ Installed evaluate 1.0.5  (58ms)
#> ✔ Installed farver 2.1.2  (59ms)
#> ✔ Installed fastmap 1.2.0  (93ms)
#> ✔ Installed fontawesome 0.5.3  (61ms)
#> ✔ Installed Formula 1.2-5  (59ms)
#> ✔ Installed fs 1.6.6  (60ms)
#> ✔ Installed generics 0.1.4  (62ms)
#> ✔ Installed ggplot2movies 0.0.1  (1s)
#> ✔ Installed ggplot2 4.0.2  (1.1s)
#> ✔ Installed glue 1.8.0  (68ms)
#> ✔ Installed gridExtra 2.3  (60ms)
#> ✔ Installed gtable 0.3.6  (60ms)
#> ✔ Installed hexbin 1.28.5  (66ms)
#> ✔ Installed highr 0.11  (63ms)
#> ✔ Installed Hmisc 5.2-5  (96ms)
#> ✔ Installed hms 1.1.4  (96ms)
#> ✔ Installed htmlTable 2.4.3  (62ms)
#> ✔ Installed htmltools 0.5.9  (62ms)
#> ✔ Installed htmlwidgets 1.6.4  (63ms)
#> ✔ Installed httr 1.4.8  (63ms)
#> ✔ Installed isoband 0.3.0  (62ms)
#> ✔ Installed jquerylib 0.1.4  (60ms)
#> ✔ Installed jsonlite 2.0.0  (95ms)
#> ✔ Installed knitr 1.51  (72ms)
#> ✔ Installed labeling 0.4.3  (71ms)
#> ✔ Installed later 1.4.6  (61ms)
#> ✔ Installed lazyeval 0.2.2  (62ms)
#> ✔ Installed lifecycle 1.0.5  (59ms)
#> ✔ Installed magrittr 2.0.4  (62ms)
#> ✔ Installed mapproj 1.2.12  (97ms)
#> ✔ Installed maps 3.4.3  (62ms)
#> ✔ Installed MatrixModels 0.5-4  (63ms)
#> ✔ Installed memoise 2.0.1  (61ms)
#> ✔ Installed mime 0.13  (59ms)
#> ✔ Installed multcomp 1.4-29  (59ms)
#> ✔ Installed munsell 0.5.1  (59ms)
#> ✔ Installed mvtnorm 1.3-3  (94ms)
#> ✔ Installed openssl 2.3.4  (62ms)
#> ✔ Installed pillar 1.11.1  (63ms)
#> ✔ Installed pkgbuild 1.4.8  (63ms)
#> ✔ Installed pkgconfig 2.0.3  (60ms)
#> ✔ Installed pkgload 1.5.0  (61ms)
#> ✔ Installed praise 1.0.0  (62ms)
#> ✔ Installed processx 3.8.6  (96ms)
#> ✔ Installed profvis 0.4.0  (64ms)
#> ✔ Installed proxy 0.4-29  (62ms)
#> ✔ Installed ps 1.9.1  (61ms)
#> ✔ Installed purrr 1.2.1  (63ms)
#> ✔ Installed quantreg 6.1  (63ms)
#> ✔ Installed quarto 1.5.1  (62ms)
#> ✔ Installed R6 2.6.1  (96ms)
#> ✔ Installed ragg 1.5.0  (97ms)
#> ✔ Installed rappdirs 0.3.4  (65ms)
#> ✔ Installed RColorBrewer 1.1-3  (61ms)
#> ✔ Installed Rcpp 1.1.1  (63ms)
#> ✔ Installed rex 1.2.1  (64ms)
#> ✔ Installed rlang 1.1.7  (67ms)
#> ✔ Installed rmarkdown 2.30  (110ms)
#> ✔ Installed roxygen2 7.3.3  (104ms)
#> ✔ Installed rprojroot 2.1.1  (64ms)
#> ✔ Installed rstudioapi 0.18.0  (61ms)
#> ✔ Installed s2 1.1.9  (65ms)
#> ✔ Installed S7 0.2.1  (64ms)
#> ✔ Installed sandwich 3.1-1  (61ms)
#> ✔ Installed sass 0.4.10  (99ms)
#> ✔ Installed scales 1.4.0  (100ms)
#> ✔ Installed SparseM 1.84-2  (1s)
#> ✔ Installed sf 1.0-24  (1.1s)
#> ✔ Installed stringi 1.8.7  (119ms)
#> ✔ Installed stringr 1.6.0  (65ms)
#> ✔ Installed svglite 2.2.2  (64ms)
#> ✔ Installed sys 3.4.3  (63ms)
#> ✔ Installed systemfonts 1.3.1  (69ms)
#> ✔ Installed testthat 3.3.2  (71ms)
#> ✔ Installed textshaping 1.0.4  (62ms)
#> ✔ Installed TH.data 1.1-5  (101ms)
#> ✔ Installed tibble 3.3.1  (63ms)
#> ✔ Installed tidyr 1.3.2  (65ms)
#> ✔ Installed tidyselect 1.2.1  (63ms)
#> ✔ Installed tinytex 0.58  (61ms)
#> ✔ Installed units 1.0-0  (61ms)
#> ✔ Installed utf8 1.2.6  (61ms)
#> ✔ Installed vctrs 0.7.1  (101ms)
#> ✔ Installed vdiffr 1.0.9  (66ms)
#> ✔ Installed viridisLite 0.4.3  (62ms)
#> ✔ Installed waldo 0.6.2  (60ms)
#> ✔ Installed withr 3.0.2  (62ms)
#> ✔ Installed wk 0.9.5  (64ms)
#> ✔ Installed xfun 0.56  (62ms)
#> ✔ Installed xml2 1.5.2  (111ms)
#> ✔ Installed yaml 2.3.12  (75ms)
#> ✔ Installed zoo 1.8-15  (46ms)
#> ✔ 1 pkg + 128 deps: kept 8, added 116, dld 116 (114.21 MB) [52.5s]
#> ✔ [2026-02-20 02:16:14] ggplot2 and dplyr installed successfully
```
