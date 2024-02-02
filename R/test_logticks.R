# helper function for testing annotation_logticks error condition

test_logticks <- function(data){
  
  # Note for later: 
  # This is because annotation_logticks shits itself if max-min is less than one power of 10.
  # If this doesn't work and it still throws an error, it might be because annotation_logticks()
  # still doesn't like if max(range) - min(range) >= 1 if they don't actually span two logticks
  # in the final plot. If that's the case, a more conservative range should be used, i.e.
  # floor(max(range)) - ceiling(min(range)), which collapses the difference to zero in this case.
  
  range <- c(data$CI.95.lower, data$CI.95.higher)
  if(log(max(range)) - log(min(range)) >= 1){
    logMode <- TRUE
  } else {
    logMode <- FALSE
  }
  
  
  return(logMode)
  
}