

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

As well as the packages hosted on GitHub (run in terminal):
```coffee
curl -o github_isotopia.zip https://github.com/sebkopf/isotopia/archive/master.zip
curl -o github_isoread.zip https://github.com/sebkopf/isoread/archive/master.zip
curl -o github_shinyFiles.zip https://github.com/sebkopf/shinyFiles/archive/master.zip
```

Transfer all these files to the offline system, then install the *isotopia* and *isoread* package as well as a modified version of the *shinyFiles* package (to allow for selection of directories) from the sources files downloaded from GitHub.

Run in R (after modifying the ```source_path``` variable) to generate the packages depencency tree (platform dependent):

```coffee
source_path <- "/Users/sk/Dropbox/Tools/software/R/CRAN_packages/"
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
# this should work if there isn't a problem with the zipfile!
install.packages(file.path(source_path, "github_isotopia.zip"), repos = NULL, type = "source")

# alternatively something along these lines
require(devtools)
unzip(file.path(source_path, "github_isotopia.zip"), exdir = source_path)
install(file.path(source_path))

```