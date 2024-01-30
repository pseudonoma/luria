# testbed 30/01/24
# for testing the new luria pipeline, with standardized dir structure

###

source("./R/helpers.R")
source("./R/wrangle_raw_data.R")
source("./R/wrangle_clean_data.R")
testRaw <- ("./data/testing 2024 01 30/test_raw.xlsx")
testWrangled <- ("./data/testing 2024 01 30/test_wrangled.csv")

###
unlink("./output", recursive = TRUE) # remove /output/ since there's a stop() call
wrangle_raw_data(testRaw, 4)
wrangle_clean_data(testWrangled)
