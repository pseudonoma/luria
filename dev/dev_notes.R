# dev notes because head empty no thought
#
###

# Current to-do
# Rewrite rawfile wrangler
#   Put in dir setup


# Pipeline autofunction
# auto_flux() - Might need this to automatically decide if func 1-1 or 1-2 is required. Also not
#    clear if this is where dir structure should be handled. If core functions are designed to be
#    called by the user, then the wrapper can't be relied on to handle dir checking. If 

# Part 1 - Wrangling [DONE]
# wrangle_raw_data()
# wrangle_clean_data()
# Note: 
# These functions don't interact with each other; both of them (a) make the same pooled/unpooled 
# "clean" files for passing to Part 2, and (b) enforce the standard dir structure that Part 2 
# looks for before it continues. 
# Part 2 must not run independently or out of sequence.

# Part 2 - Analysis [DONE]
# run_fluxxer()
#    calculateMutRate()

# Part 3 - Export
# get_mutrates() - wrapper with plot/extr/both arg?
#   extract_mutrate() - pulls all relevant mutation rates and exports as RDATA object
#   plot_mutrate() - takes non-comparison CSVs and plots
# Notes:
# Plotting seems to require refactoring and some by-project shenanigans, so there's a 
# "core" plotter that generates the plot, and then a bunch of in-place wrangler shit around it.
# Instead of current implementation of the wrapper calling the plotter, maybe the plotter
# can call the wrangler beforehand instead?

# Epilogue - Cleanup
# Decide what the args should be called (esp. e.g. file vs filePath)


###

# Some stuff for documentation

#' Prepare a previously-wrangled CSV file for the pipeline.
#' 
#' If a correctly-formatted CSV file was wrangled manually, or the fluctuation analysis workbook
#' was not used, this function prepares the CSV file for use with the pipeline.
#' 
#' @details
#' This function produces two CSV files, one with all replicates pooled into one strain, and another 
#' CSV with the replicates kept separate. The output CSV has the standard headers required by 
#' [`run_fluxxer()`], the next function in the pipeline.
#' 
#' @import openxlsx
#' 
#' @inheritParams wrangle_raw_data
#' @param dataFile A CSV file with the standard headers `strain`, `plate`, `fraction`, and `CFU`.
#' @param exclude A character vector of replicates to skip.
#' 
#' @examples
#' wrangle_clean_data(dataFile = "./data/raws/FLUCTEST 1 2020 09 24.csv",
#'                    poolAs = "AB3", 
#'                    exclude = c(Rep 0", "Rep 13"), 
#'                    saveAs = "RIF_Aug2023")
#'                    
#' @return
#' Two CSVs with columns `strain`, `plate`, `fraction`, and `CFU`.
#'
#' @export


#' [`fluxxer.R`](https://github.com/barricklab/barricklab/blob/master/fluxxer.R)


