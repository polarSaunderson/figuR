# figuR

## Overview
The figuR package is a personal package that allows easier customisation of figures. 
It uses base R, but the functions use a syntax that I find more intuitive, and thus it is more configurable whilst being essentially the same.

## Instructions
This package is mainly a personal package, so it is not available on CRAN.
To download this package directly from GitHub, you'll need to use the "devtools" package.
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

4) Some of the functions in `figuR` require my `domR` and `kulaR` packages too.
If you want to use these functions, it is necessary to also install these:
```R
devtools::install_github("polarSaunderson/domR")
devtools::install_github("polarSaunderson/kulaR")
```

5) Load the domR package
```R
library(domR)
```
