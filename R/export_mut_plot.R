# helper function for exporting mutation rate plots

export_mut_plot <- function(plot, project, exportPath){
  
  plotName <- paste0("plot_", project)
  plotPath <- exportPath
  ggsave(paste0(plotName, ".pdf"), plot = plot,
         height = 6, width = 8, units = "in", dpi = 300,
         limitsize = TRUE, path = exportPath)
  ggsave(paste0(plotName, ".png"), plot = plot,
         height = 6, width = 8, units = "in", dpi = 600,
         limitsize = FALSE, path = exportPath)
  
  
}