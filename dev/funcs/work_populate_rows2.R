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
currentCounts <- currentCounts[currentCounts$Dilution.factor == dilution, ]

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

# Prep currentData
strain <- unique(currentSheet$Name)
rowCount <-
currentData <- data.frame(strain = rep(strain, times),
                          plate = NA,
                          fraction = NA,
                          CFU = NA)


currentData <- data.frame(strain = rep(sheet, nrow(currentCounts) + (60 - countPops)),
                          plate = NA,
                          fraction = NA,
                          CFU = NA)

countLength <- nrow(currentCounts)
currentData$plate[1:countLength] <- "Count"
currentData$fraction[1:countLength] <- countFraction
currentData$CFU[1:countLength] <- currentCounts$mean






