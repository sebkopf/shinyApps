shinyApps
=========

This repository is intended to hold the source code for all of my shiny apps (I'll put them online as I find the time to make them useful enough to share). For starters I put up my [isolabel](#isolabel) app and hope people find it useful. As always, please use the GitHub [issue tracker](https://github.com/sebkopf/shinyApps/issues) for any feedback, suggestions, bug reports, feature requests, etc.

## isolabel

A shiny app for calculating expected enrichments from stable isotope labeling in microbial populations. There are several ways you can run this app.

### Run online
An online version is available [here](https://sebkopf.shinyapps.io/isolabel/) and should run from any javascript enabled browser without installing anything extra (but is not necessarily always the most up to date version although I will try to keep it current).

### Run locally
To run the shiny app locally you need to install the following packages:
```coffee
packages <- c("shiny", "ggplot2", "lubridate", "stringr", "devtools")
install.packages(packages, depen=T)
devtools::install_github("shiny-incubator", "rstudio")
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
