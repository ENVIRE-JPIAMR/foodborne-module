## Script for loading libraries

## Set install_libraries to TRUE, or source manually the following
## block, in order to install all the libraries needed.
install_libraries = FALSE

## Install libraries
if (install_libraries)
{
  install.packages(c(
    "readxl",
    "ggplot2",
    "dplyr",
    "reshape2",
    "ggrepel",
    "gridExtra",
    "ggfortify",
    "here",
    "mc2d",
    "MCSim",
    "stats",
    "EnvStats",
    "extraDistr",
    "remotes",
    "FAdist")
  )
}

####################
## LOAD LIBRARIES ##
####################

library(readxl)       # for reading excel files
library(ggplot2)      # for plotting
library(dplyr)        # for plotting
library(tidyr)        # for plotting
library(reshape2)
library(ggrepel)
library(gridExtra)
library(ggfortify)
library(here)         # for script sourcing issues
library(mc2d)         # for truncated distributions
library(MCSim)
library(stats)
library(EnvStats)
library(extraDistr)
library(remotes)
library(FAdist)

## Cleanup
rm("install_libraries")



