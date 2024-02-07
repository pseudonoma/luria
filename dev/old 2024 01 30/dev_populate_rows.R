# populate_rows()
# A helper function to handle populating flux table.
# Agnostic to whether it's given pooled or unpooled data; the wrapping function differentiates
# pooled/unpooled by naming the $Strain column, which this function doesn't care about.

populate_rows <- function(currentData, currentCounts, currentMutants, countFraction, countPops){
  # ARGS:
  # currentData - usually currentFrame; the current df to be populated and returned
  # currentCounts - Count data from the current sheet
  # currentMutants - Selective data from the current sheet
  # countFraction - fraction value for Count pops; generated from countVars in the wrapper
  
  # 1. Populate Count rows (this fraction defined by user)
  countLength <- nrow(currentCounts)
  currentData$plate[1:countLength] <- "Count"
  currentData$fraction[1:countLength] <- countFraction
  currentData$CFU[1:countLength] <- currentCounts$mean
  
  # 2. Populate Selective rows (this fraction isn't optional; by the protocol it's 1)
  mutantLength <- nrow(currentMutants)
  if(mutantLength == 0){ # even if there's no mutants, you need one more row to fill 0 into
    mutantLength <- 1
  }
  currentData$plate[(countLength+1):(countLength+mutantLength)] <- "Selective"
  currentData$fraction[(countLength+1):(countLength+mutantLength)] <- 200/(200*1) # fract = 1
  if(nrow(currentMutants) == 0){
    currentData$CFU[countLength+1] <- 0 #
  } else{
    currentData$CFU[(countLength+1):(countLength+mutantLength)] <- currentMutants$Count
  }
  
  # 3. Fill the rest of the selective rows with 0 CFUs
  currentData$plate[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- "Selective"
  currentData$fraction[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- (200*1)/200
  currentData$CFU[(countLength + mutantLength + 1):(countLength + (60 - countPops))] <- 0
  
  
  return(currentData)
  
} # end populate_rows(). #

