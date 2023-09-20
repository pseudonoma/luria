# testbench
# for testing the new wrangle_fluxdata() + populate_rows() workflow

# source
source("./R/wrangle.R")

# data
rif <- "./data/raws/FLUCTEST 1 2020 09 24.xlsx"
stp <- "./data/raws/FLUCTEST 2 2023 02 06.xlsx"
outPath <- "./data/test"

# call
wrangle_fluxdata(dataFile = rif, 
                 countPops = 4, 
                 #countFract = c(P = 200, C = 200, D = 1E5),
                 poolAs = "AB3", # test with strain names
                 exclude = c("Summary", "Rep 0", "Rep 13"), # test with additional non-Summary sheet names
                 save = "./data/test" # test with folder name
                 ) 
