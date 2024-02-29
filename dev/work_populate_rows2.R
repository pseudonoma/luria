# workbench
# for rebuilding raw-file wrangling from the bottom up

#

library(dplyr)
library(openxlsx)

# get dataset
templateFile <- "./dev/data/raws redesign/testbed.xlsx"
allSheets <- openxlsx::getSheetNames(templateFile)
sheet <- allSheets[2]

# get current raw rep and clean up
currentSheet <- openxlsx::read.xlsx(templateFile, sheet = sheet)
cols <- c("Name", "Type", "Well", "Volume.taken", "Volume.plated", "Dilution.factor",
          "CFU.observed") # exclusion
currentSheet <- currentSheet[, cols]

# prep datasets
currentCounts <- currentSheet[currentSheet$Type == "Count", ]
currentMutants <- currentSheet[currentSheet$Type == "Selective", ]

# summarize Count data (should probably just use dplyr for everything)
count.dilution <- 1e5 # exclusion

# collapse CFUs
currentCounts <-
  currentCounts |>
  filter(Dilution.factor == count.dilution) |>
  dplyr::group_by(Name, Type, Well, Volume.taken, Volume.plated,
                  Dilution.factor) |>
  dplyr::summarize(CFU = mean(CFU.observed)) |>
  dplyr::ungroup()

# Establish total row count before currentData can be prepped
countLength <- nrow(currentCounts)
mutantLength <- nrow(currentMutants)
if(autofill){ # fill to standard plate capacity
  rowTotal <- 60
} else if(mutantLength >= 1){ # do not fill & >=1 mutant has CFUs
  rowTotal <- countLength + mutantLength
} else if(mutantLength == 0){
  warning(paste("Skipping <rep> because there are no mutants."))
}

# Check if every detected Count population is distinct
if(length(unique(currentCounts$Well)) != countLength){
  stop("No. of Count observations != No. of unique Count wells reported. This might be a problem.")
}

# Prep currentData
strain <- unique(currentSheet$Name)
if(length(strain) > 1){
  stop("Multiple replicate names detected in a single replicate dataset.")
}
currentData <- data.frame(strain = rep(strain, rowTotal),
                          plate = NA,
                          fraction = NA,
                          CFU = NA)

# Fill Count rows
for(i in 1:countLength){
  currentData$plate[i] <- "Count"
  currentData$fraction[i] <- currentCounts$Volume.plated[i]/(currentCounts$Volume.taken[i] *
                                                               currentCounts$Dilution.factor[i])
  currentData$CFU[i] <- currentCounts$CFU[i]
}

# Fill Selective rows
# Special case: no mutants counted. Needs its own branch?


# Begin filling Selective rows...
mutantStart <- countLength + 1
mutantEnd <- countLength + mutantLength
currentData$plate[mutantStart:(countLength + mutantLength)] <- "Selective"
currentData$fraction[mutantStart:(countLength + mutantLength)] <-
  currentMutants$Volume.plated/(currentMutants$Volume.taken * currentMutants$Dilution.factor)
currentData$CFU[mutantStart:(countLength + mutantLength)] <- currentMutants$CFU.observed

# ...and finish filling Selective rows with 0
if(rowTotal > countLength + mutantLength){ # implies fill was active
  fillStart <- mutantEnd + 1
  currentData$plate[fillStart:rowTotal] <- "Selective"
  currentData$fraction[fillStart:rowTotal] <-  mean(currentData$fraction[currentData$plate == "Selective"],
                                                    na.rm = TRUE)
  currentData$CFU[fillStart:rowTotal] <- 0
}






# 0 CFU mutant case
if(nrow(currentMutants) == 0){
  currentData$CFU[countLength+1] <- 0 #
} else{

}

### junk ###

# Previous implementation - no idea why I made nrow able to go over 60 depending on countPops
currentData <- data.frame(strain = rep(sheet, nrow(currentCounts) + (60 - countPops)),
                          plate = NA,
                          fraction = NA,
                          CFU = NA)



