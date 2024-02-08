#!/bin/bash

# quick script to run fluxxer.R
# in the future this should become a better script (currently working on do_fluxxer.sh)

#####

# define file folder
sourceDir=./data/wrangled

# run fluxxer.R
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 1 2020 09 24_short_unpooled.csv" -o "FLUCTEST1_short_unpooled" -c
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 1 2020 09 24_short_pooled.csv" -o "FLUCTEST1_short_pooled"
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 1 2020 09 24_long_unpooled.csv" -o "FLUCTEST1_long_unpooled" -c
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 1 2020 09 24_long_pooled.csv" -o "FLUCTEST1_long_pooled"
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 2 2023 02 06_unpooled.csv" -o "FLUCTEST2_unpooled" -c
Rscript ./R/fluxxer.R -i "$sourceDir/FLUCTEST 2 2023 02 06_pooled.csv" -o "FLUCTEST2_pooled"