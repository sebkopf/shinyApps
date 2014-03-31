shinyApps
=========

collection of shiny apps

## isolabel

A shiny app for calculating expected enrichments from stable isotope labeling in microbial populations. There are several ways you can run this app.

### Run online
An online version is available [here](https://sebkopf.shinyapps.io/isolabel/) and should run from any javascript enabled browser without installing anything extra (but is not necessarily always the most up to date version although I will try to keep it current).

### Run locally
To run the shiny app locally you need to install the following packages:
```coffee
packages <- c("shiny", "ggplot2", "lubridate", "stringr")
install.packages(packages, depen=T)
```

#### Run locally from GitHub
Now you can just run the app either directly from GitHub (most convenient):
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
