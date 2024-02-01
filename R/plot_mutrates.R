# core plotting function

plot_mutrates <- function(data, levelOrder, log = NULL){
  # ARGS:
  # data - the data to plot. probably always combined data
  # levelOrder - the levels to refactor by, including if reps are missing
  
  # DEBUG
  # data <- plotData$data
  # levelOrder <- plotData$levels
  
  # make most of the plot
  plot <- ggplot(data, aes(x = forcats::fct_relevel(strain, levelOrder), 
                           y = mu)) +
    geom_point(shape = 16, size = 2, position = position_dodge(width = 0.75)) +
    geom_errorbar(aes(ymin=CI.95.lower, ymax=CI.95.higher), width = 0.15, linewidth = 0.4,
                  position = position_dodge(width = 0.75)) +
    scale_y_log10() +
    xlab(NULL) +
    ylab("Mutation rate") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  # if this flag is true, logticks can be added and won't cause an error
  if(isTRUE(log)){
    plot <- plot + annotation_logticks(sides = "l")
  }
  
  
  return(plot)
  
}
