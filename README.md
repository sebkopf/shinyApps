shinyApps
=========

This repository is intended to hold the source code for all of my shiny apps (I'll put them online as I find the time to make them useful enough to share). For starters I put up my [isolabel](#isolabel) app and hope people find it useful. As always, please use the GitHub [issue tracker](https://github.com/sebkopf/shinyApps/issues) for any feedback, suggestions, bug reports, feature requests, etc.

## isolabel

A shiny app for estimating expected enrichments from stable isotope labeling in microbial populations. Based on the isotopic enrichment resulting from unbiased clonal growth in the presence of an isotopic label:

<!---
URL encode using: http://www.url-encode-decode.com/urlencode
F{\left(t\right)}=F_{N}\left(1-e^{-{\mu}\cdot{t}}\right)+F_{t_0}\cdot{e^{-{\mu}\cdot{t}}}
\left(\mu{}=Ln(2)/T\right)
t_{label}=\frac{T}{ln(2)}\cdot{ln\left(\frac{F_N-F_{t_0}}{F_N-F_f}\right)}
-->

![equation](http://latex.codecogs.com/gif.latex?F%7B%5Cleft%28t%5Cright%29%7D%3DF_%7BN%7D%5Cleft%281-e%5E%7B-%7B%5Cmu%7D%5Ccdot%7Bt%7D%7D%5Cright%29%2BF_%7Bt_0%7D%5Ccdot%7Be%5E%7B-%7B%5Cmu%7D%5Ccdot%7Bt%7D%7D%7D)

with ![equation](http://latex.codecogs.com/gif.latex?F_%7BN%7D) the isotopic composition of new biomass generated in the presence of the isotope label (using fractional abundances F is important for correct mass balance!). Substituting the growth rate for the generation time T (µ = ln(2) / T), one can readily calculate the required labeling time to reach a target isotopic composition ![equation](http://latex.codecogs.com/gif.latex?F_%7Bf%7D):

![equation](http://latex.codecogs.com/gif.latex?t_%7Blabel%7D%3D%5Cfrac%7BT%7D%7Bln%282%29%7D%5Ccdot%7Bln%5Cleft%28%5Cfrac%7BF_N-F_%7Bt_0%7D%7D%7BF_N-F_f%7D%5Cright%29%7D)

This app is intended to make it easy to explore this space for different isotope systems and takes care of all the isotope conversions. There are several ways you can run this app.

### Run online
An online version is available [here](https://sebkopf.shinyapps.io/isolabel/) and should run from any javascript enabled browser without installing anything extra (but is not necessarily always the most up to date version although I will try to keep it current).


### Run locally
To run the shiny app locally you need to install the following packages:
```coffee
packages <- c("shiny", "ggplot2", "reshape2", "lubridate", 
              "stringr", "devtools", "plyr", "RColorBrewer")
install.packages(packages, depen=T)
library(devtools)
devtools::install_github("shiny-incubator", "rstudio")
devtools::install_github("isotopia", "sebkopf", "v0.4")
```

#### Run locally from GitHub
Now you can just run the app either directly from GitHub (most convenient and always up to date):
```coffee
library(shiny)
runGitHub("shinyApps", "sebkopf", subdir = "isolabel")
```

#### Run local copy
Or you can copy the **server.R** and **ui.R** source files from the **isolabel** folder to your local workspace directory (e.g. into a folder named **isolabel**)  - or simply fork the entire repository and clone it to your computer - and then run it from this folder (adjust relative path from your working directory):
```coffee
library(shiny)
path <- "isolabel"
runApp(path)
```
