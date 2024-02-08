# helper function that refactors reps. Called by wrangle_plot_data

refactor_reps <- function(data, pooledPrefix = NULL){
  
  # Extract replicate prefix (as long as the format is <prefix><number>)
  reps <- data$strain[which(data$strain != pooledPrefix)]
  repPrefix <- unique(gsub("[[:digit:]]*$", "", reps))
  if(length(repPrefix) > 1){
    stop("One or more replicates are not named consistently.")
  }
  
  # Automatically construct correct factor order for passing to plotter
  idealOrder <- paste0(repPrefix, 1:1000)
  badOrder <- reps
  goodOrder <- c(pooledPrefix, idealOrder[idealOrder %in% badOrder])
  
  
  return(goodOrder)
  
}
