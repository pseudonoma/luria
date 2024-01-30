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

###

# might be able to extract project names by
pooledList <- grep("_pooled", dir("./data/analyzed"), value = TRUE)
sub("_pooled.output.csv$", "", pooledList)

