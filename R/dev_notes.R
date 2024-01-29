# dev notes because head empty no thought
#
###

# Pipeline autofunction
# auto_flux() - Might need this to automatically decide if func 1-1 or 1-2 is required. Also not
#    clear if this is where dir structure should be handled. If core functions are designed to be
#    called by the user, then the wrapper can't be relied on to handle dir checking. If 

# Part 1 - Wrangling
# 1. wrangle_raw()
# 2. wrangle_intermediate()
# Additional req: 
#  dir structure must be completed here; refuse to run Part 2 if dir structure not recognized

# Part 2 - Analysis
# run_fluxxer()
#   calculateMutRate()

# Part 3 - Export
# get_mutrates() - wrapper with plot/extr/both arg?
#   extract_mutrate() - pulls all relevant mutation rates and exports as RDATA object
#   plot_mutrate() - takes non-comparison CSVs and plots

