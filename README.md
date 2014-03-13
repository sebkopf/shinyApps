shinyApps
=========

collection of shiny apps

to run "isolabel" shiny app you need to install the following packages:
```coffee
packages <- c("shiny", "ggplot2", "lubridate", "stringr")
install.packages(packages, depen=T)
```

then just run the app from GitHub:
```coffee
library(shiny)
runGitHub("shinyApps", "sebkopf", subdir = "isolabel")
```
