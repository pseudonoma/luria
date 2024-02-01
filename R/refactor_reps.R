# helper function that refactors reps. Called by wrangle_plot_data

refactor_reps <- function(data){
  
  # Extract replicate prefix (as long as the format is <prefix><number>)
  reps <- data$strain
  repPrefix <- unique(gsub("[[:digit:]]*$", "", reps))
  if(length(repPrefix) > 1){
    stop("One or more replicates are not named consistently.")
  }
  
  # Automatically construct correct factor order for passing to plotter
  idealOrder <- paste0(repPrefix, 1:1000)
  badOrder <- reps
  if("Pooled" %in% badOrder){
    goodOrder <- c("Pooled", idealOrder[idealOrder %in% badOrder])
  } else {
    goodOrder <- idealOrder[idealOrder %in% badOrder]
  }
  
  
  return(goodOrder)
  
}
