## Sigman Data Viewer

The Sigman Data Viewer is a browser based Isodat run monitoring system built using the [Shiny](http://shiny.rstudio.com/) web application framework and the [Isoread](https://github.com/sebkopf/isoread#isoread) IRMS data interface R module. As a browser based application it is very easy to run on almost any system without the need for other types of graphical user interfaces and can automatically be hosted on an instrument machine for easy network access by users on the same network. It was specifically designed for instrument performance monitoring and on-the-fly data monitoring of big batch N2O isotope ratio measurements in the [Sigman lab](http://www.princeton.edu/sigman/) at Princeton, but could easily be adapted for similar analyses (e.g. traditional big batch EA-IRMS or gasbench-IRMS runs).

![Screenshot of the Linearity Evaluation](/doc/linearity_evaluation.png?raw=true)

## Installation

Install all dependencies

```coffee
install.packages(c(
  "shiny",
  "htmltools",
  "ggplot2",
  "RColorBrewer",
  "gridExtra",
  "lattice",
  "scales",
  "plyr",
  "reshape2"
), dependencies = TRUE)
```

Install the *isotopia* and *isoread* package as well as a modified version of the *shinyFiles* package (to allow for selection of directories) from GitHub.

```coffee
library(devtools)
devtools::install_github("sebkopf/shinyFiles")
devtools::install_github("sebkopf/isotopia")
devtools::install_github("sebkopf/isoread")
```

### Installing on a system not connected to the internet

Download all the packages that are necessary. The easiest way to make this possible is by just pulling all current release packages from CRAN and transferring them to the offline system (warnings, these are a lot of files!). The CRAN directory depends on the operating system, R version (must match exactly!!) and source vs. binaries. For example, for R 3.1 for the windows binaries, run in terminal:

```coffee
mkdir CRAN_packages
cd CRAN_packages
wget -nc ftp://cran.r-project.org/pub/R/bin/windows/contrib/3.1/*.zip
```

As well as the packages hosted on GitHub (run in terminal within CRAN_packages folder), which need to be converted to tar balls for installation (zip files don't work with ```install.packages```):
```coffee
mkdir GitHub_packages
cd GitHub_packages
curl -L -o isotopia.zip https://github.com/sebkopf/isotopia/archive/master.zip
curl -L -o isoread.zip https://github.com/sebkopf/isoread/archive/master.zip
curl -L -o shinyFiles.zip https://github.com/sebkopf/shinyFiles/archive/master.zip
unzip isotopia.zip && tar -cvzf isotopia.tar.gz ./isotopia-* && rm -d -r isotopia-*
unzip isoread.zip && tar -cvzf isoread.tar.gz ./isoread-* && rm -d -r isoread-*
unzip shinyFiles.zip && tar -cvzf shinyFiles.tar.gz ./shinyFiles-* && rm -d -r shinyFiles-*
```

Transfer all these files to the offline system, then install the *isotopia* and *isoread* package as well as a modified version of the *shinyFiles* package (to allow for selection of directories) from the sources files downloaded from GitHub.

Run in R (after modifying the ```source_path``` variable) to generate the packages depencency tree (platform dependent):

```coffee
source_path <- "/Users/sk/Dropbox/Tools/software/R/CRAN_packages"
library(tools)
write_PACKAGES(source_path, type = "win.binary")
#write_PACKAGES(source_path, type = "mac.binary")
```

Then install the packages from there:

```coffee
install.packages(c(
  "shiny",
  "htmltools",
  "ggplot2",
  "RColorBrewer",
  "gridExtra",
  "lattice",
  "scales",
  "plyr",
  "reshape2"
), dependencies = TRUE,
  contriburl = paste0("file://", source_path))
```

And the packages from GitHub:

```coffee
install.packages(file.path(source_path, "GitHub_packages", "shinyFiles.tar.gz"), repos = NULL, type = "source")
install.packages(file.path(source_path, "GitHub_packages", "isoread.tar.gz"), repos = NULL, type = "source")
install.packages(file.path(source_path, "GitHub_packages", "isotopia.tar.gz"), repos = NULL, type = "source")
```

### Install the shinyApp itself

```coffee
curl -L -o shinyApps.zip https://github.com/sebkopf/shinyApps/archive/master.zip
unzip shinyApps.zip
mv shinyApps-master/sigman_data_viewer ./
rm -r -d shinyApps-master
```

Ideally this should be changed into an R package format but have not gotten around to doing this yet.