# testbed 30/01/24
# for testing the new luria pipeline, with standardized dir structure

###

unlink("./output", recursive = TRUE) # remove /output/ because of that stop() call
source("./R/helpers.R")

###
# Part 1: Wrangle
source("./R/wrangle_raw_data.R")
source("./R/wrangle_clean_data.R")
testRaw <- ("./data/testing 2024 01 30/test_raw.xlsx")
testWrangled <- ("./data/testing 2024 01 30/test_wrangled.csv")

wrangle_raw_data(testRaw, 4, overwrite = TRUE, exclude = c("Summary", "Rep 0", "Rep 13"))
wrangle_clean_data(testWrangled, saveAs = "EL_test", overwrite = TRUE)

# Part 2: Analyze
source("./R/run_fluxxer.R")
source("./R/calculate_mut_rate.R")

run_fluxxer(overwrite = TRUE)

# Part 3: Plot
source("./R/plot_fluxxer.R")
source("./R/wrangle_plot_data.R")
source("./R/refactor_reps.R")
source("./R/test_logticks.R")
source("./R/plot_mutrates.R")
source("./R/export_mut_plot.R")

plot_fluxxer(overwrite = TRUE)
