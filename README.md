# figuR

## Overview
`figuR` is a personal package to easier customisation of figures. 
It uses base R and graphics, but the functions use a syntax that I find more 
unified and intuitive. It is thus more configurable whilst being essentially the 
same. This is very experimental!

**WARNING** The functions `calc_intervals` and `plot_matrix` are often 
problematic if used in new situations. The former is used for many defaults in 
the other functions, so proceed with *EXTREME CAUTION*!

## Instructions
This package is mainly a personal package, so it is not available on CRAN.
To download this package directly from GitHub, you'll need to use the "devtools" 
package.
It's easiest to do this within RStudio.

1) Install the [devtools](https://github.com/hadley/devtools) package from CRAN: 
``` R
install.packages("devtools")
```

2) Load the devtools package:
```R
library(devtools)
```

3) Install figuR directly from GitHub:
```R
devtools::install_github("polarSaunderson/figuR")
```

4) Some of the functions in `figuR` require my `kulaR` package too.
If you want to use these functions, it is necessary to also install these and their dependencies:
```R
install.packages("khroma")  # necessary for kulaR
devtools::install_github("polarSaunderson/kulaR")
```

5) Load the figuR package
```R
library(figuR)
```
