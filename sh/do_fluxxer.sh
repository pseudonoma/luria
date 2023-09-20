#!/bin/bash

# "do_fluxxer.sh"
# An experimental script to automatically run fluxxer.R and stuff

# establish data paths and naming conventions?
sourceDir=./data/wrangled
# dataBlocks


# target code:
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 1 2020 09 24_short_unpooled.csv" -o "FLUCTEST1_short_unpooled" -c
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 1 2020 09 24_short_pooled.csv" -o "FLUCTEST1_short_pooled"
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 1 2020 09 24_long_unpooled.csv" -o "FLUCTEST1_long_unpooled" -c
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 1 2020 09 24_long_pooled.csv" -o "FLUCTEST1_long_pooled"
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 2 2023 02 06_unpooled.csv" -o "FLUCTEST2_unpooled" -c
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 2 2023 02 06_pooled.csv" -o "FLUCTEST2_pooled"


# help
# -i --input: "Input CSV file name"
# -o --output: "Output file prefix"
# -c --comparisons: "Perform comparisons between fluctuation tests. Results 
#                   in the output of file comparisons.csv showing p-values 
#                   for tests that mutation rates are significantly different 
#                   between samples."