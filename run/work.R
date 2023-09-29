# workbench(2)
# second try on the analysis, this time with pseudo-package "luria"

###
library(ggplot2)
library(ggthemr)
library(poirot)
source("./R/wrangle.R")
source("./R/mutrates.R")

# also needed from pseudopkg "poirot"
# aesfuncs - manually_theme(), overlay_means_classic()
# wranglers - auto_export()

##### Part 1: wrangle for fluxxer.R #####

# set out the data sources
rif <- "./data/raws/FLUCTEST 1 2020 09 24.xlsx"
stp <- "./data/raws/FLUCTEST 2 2023 02 06.xlsx"

# RIF, "long" (all valid reps)
wrangle_fluxdata(dataFile = rif, 
                 countPops = 4, 
                 #countFract = c(P = 200, C = 200, D = 1E5),
                 poolAs = "Pooled",
                 exclude = c("Summary", "Rep 0", "Rep 13"),
                 save = "./data/RIF_long") 

# RIF, "short" (only valid, non-outlier reps)
wrangle_fluxdata(dataFile = rif, 
                 countPops = 4, 
                 #countFract = c(P = 200, C = 200, D = 1E5),
                 poolAs = "Pooled",
                 exclude = c("Summary", "Rep 0", "Rep 13", "Rep 2", "Rep 5"),
                 save = "./data/RIF_short") 
# STP
wrangle_fluxdata(dataFile = stp, 
                 countPops = 3, 
                 #countFract = c(P = 200, C = 200, D = 1E5),
                 poolAs = "Pooled",
                 #exclude = c("Summary", "Rep 0", "Rep 13"),
                 save = "./data/STP") 

##### Part 2: run fluxxer.R #####
#

##### Part 3: extract mutation rates #####

get_mutation_rates(dataPath = "./data/analyzed", 
                   projNames = c("FLUCTEST1_long", "FLUCTEST1_short", "FLUCTEST2"), 
                   repTag = "Rep ", 
                   theme = manually_theme("pale"), 
                   export = "both")
