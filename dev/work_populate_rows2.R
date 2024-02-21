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
currentCounts <- currentCounts[currentCounts$Dilution.factor == count.dilution, ]

# collapse CFUs
currentCounts <-
  currentCounts |>
  dplyr::group_by(Name, Type, Well, Volume.taken, Volume.plated,
                  Dilution.factor) |>
  #dplyr::mutate(CFU = mean(CFU.observed)) |>
  dplyr::summarize(CFU = mean(CFU.observed)) |>
  dplyr::ungroup()
# at this point, currentCounts is ready to populate allData
# do I even know how to code, fuck

# Establish total row count before currentData can be prepped
countLength <- nrow(currentCounts)
mutantLength <- nrow(currentMutants)
if(autofill){
  rowTotal <- 60 # ?
} else if(mutantLength == 0){
  rowTotal <- countLength + 1 # an extra Mutant observation, which will be set to 0
  mutantLength <- 1
} else if(mutantLength >= 1){
  rowTotal <- countLength + mutantLength
}

# nts 21/02/24: seems like there's two directions, rowTotal - data, vs. count + mutant
# ideally count + mutant = rowTotal? Why does there seem to be a conflict, is it because of the
#  +1 mutant with 0 observations step?

# Check if every detected Count population is distinct
if(length(unique(currentCounts$Well)) != countLength){
  stop("No. of Count observations != No. of unique Count wells reported.")
}

# Prep currentData
strain <- unique(currentSheet$Name)
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
mutantStart <- countLength + 1

# begin filling Selective rows (this code doesn't work in the case of the 0 row!)
currentData$plate[mutantStart:(countLength + mutantLength)] <- "Selective"
currentData$fraction[mutantStart:(countLength + mutantLength)] <-
  currentMutants$Volume.plated[i]/(currentMutants$Volume.taken[i] * currentMutants$Dilution.factor[i])

if(nrow(currentMutants) == 0){
  currentData$CFU[countLength+1] <- 0 #
} else{
  currentData$CFU[(countLength+1):(countLength+mutantLength)] <- currentMutants$Count
}

# finish filling Selective rows with 0



### junk ###

# Previous implementation - no idea why I made nrow able to go over 60 depending on countPops
currentData <- data.frame(strain = rep(sheet, nrow(currentCounts) + (60 - countPops)),
                          plate = NA,
                          fraction = NA,
                          CFU = NA)



