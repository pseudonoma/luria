# core plotting function

plot_mutrates <- function(data, levelOrder){
  # ARGS:
  # data - the data to plot. probably always combined data
  # levelOrder - the levels to refactor by, including if reps are missing
  
  plot <- ggplot(data, aes(x = forcats::fct_relevel(strain, levelOrder), 
                                   y = mu)) +
    geom_point(shape = 16, size = 2, position = position_dodge(width = 0.75)) +
    geom_errorbar(aes(ymin=CI.95.lower, ymax=CI.95.higher), width = 0.15, linewidth = 0.4,
                  position = position_dodge(width = 0.75)) +
    scale_y_log10() +
    xlab(NULL) +
    ylab("Mutation rate") +
    annotation_logticks(sides = "l") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  
  return(plot)
  
}

