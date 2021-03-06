# Nitrogen cycle dynamics

This shiny app is a visual tool to explore the isotope dynamics of the nitrogen cycle. Its goal is to make the different processes more accessible and help students gain a more intuitive understanding of how they change the ocean's nitrogen reservoirs. All plots can be saved as PDF and can of course be used freely, especially for educational purposes. The application is released as open software under the GNU General Public License 3.0. 

### Run online
An online version is available [here](https://sebkopf.shinyapps.io/nitrogen_cycle_dynamics/) and should run from any javascript enabled browser without installing anything extra (but is not necessarily always the most up to date version although I will try to keep it current). Unfortunately, MathJax (the rendering of latex equations) doesn't always seem to work in the online version. Running locally is recommended if feasible.

### Run locally
To run the shiny app locally you need to [install R](https://cran.rstudio.com/bin/) and optionally (but highly recommend) the development interface [R-Studio](http://www.rstudio.com/products/rstudio/download/), then launch R or R-Studio and install the following packages (ggplot2 version 2.0 or better is required) from the command line:

```coffee
packages <- c("shiny", "shinydashboard", "shinyBS", "shinyjs", "DT",
              "readxl", "magrittr", "plyr", "reshape2", 
              "ggplot2", "scales", "gridExtra", "RColorBrewer",
              "latex2exp", "knitr")
install.packages(packages)
```

#### Run locally from GitHub
Now you can just run the app either directly from GitHub (most convenient and always up to date):
```coffee
library(shiny)
runGitHub("sebkopf/shinyApps", subdir = "nitrogen_cycle_dynamics")
```

#### Run local copy
Or you copy the `nitrogen_cycle_dynamics` folder to your local workspace directory and then run it from this folder (adjust relative path from your working directory):
```coffee
library(shiny)
path <- "nitrogen_cycle_dynamics"
runApp(path)
```
