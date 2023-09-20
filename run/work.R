# workbench
# reanalysis of FLUCTEST1 and FLUCTEST2, Sep 2023
# this project is based off the FLUCTEST2 work and scripts.

###

library(leprechaun) # for wrangle_fluxdata()
library(ggplot2)
library(ggthemr)

source("./R/easyplot_fluxdata.R")
source("./R/plot_mutrates.R") # core plotter function

# aesthetics pkg functions
# overlay_means_classic (aesfuncs.R)
# manually_theme (aesfuncs.R)
# auto_export (wranglers.R)

##### Part 1: Wrangle fluctest data #####

# specify the datasets
rifSource <- "FLUCTEST 1 2020 09 24"
stpSource <- "FLUCTEST 2 2023 02 06"

# wrangle RIF data
wrangle_fluxdata(testName = rifSource,
                 sheetOffset = 2, # summary + rep 0
                 countPops = 4,
                 dataPooling = FALSE,
                 excludeList = "Rep 13") # Rep 13 had no muts; also note countVars= is default
wrangle_fluxdata(testName = rifSource,
                 sheetOffset = 2,
                 countPops = 4,
                 dataPooling = TRUE)

# wrangle STP data
wrangle_fluxdata(testName = stpSource,
                 sheetOffset = 1, # summary
                 countPops = 3,
                 dataPooling = FALSE)
wrangle_fluxdata(testName = stpSource,
                 sheetOffset = 1,
                 countPops = 3,
                 dataPooling = TRUE)

##### Part 2: Plot fluctest data #####

# run the plotter
easyplot_fluxdata(dataPath = "./data/analyzed", # where the fluxxer .output.csvs are 
                  projNames = c("FLUCTEST1" ,"FLUCTEST2"), 
                  repTag = "Rep ", 
                  theme = manually_theme("pale"), 
                  export = TRUE)

