#' Analyze the data and estimate mutation rates.
#' 
#' Estimate mutation rates from wrangled fluctuation analysis data, using an individual file or
#' as the next step in the pipeline. 
#' 
#' @details
#' This function calls [`calculate_mut_rate()`] to estimate mutation rates. It can run on a single 
#' file, or cycle automatically through all files saved to ./output/wrangled/ by the previous step 
#' in the pipeline. For every input file, a matching output file ending in `.output.csv` will be 
#' saved to `./output/analyzed`. If `comparisons` is `TRUE`, an additional file will be generated 
#' ending in `.comparisons.csv`; this file contains pairwise comparisons between all replicates and 
#' p-values indicating how different the mutation rates are from each other. Supplying the `file` 
#' argument runs this function in single-file mode. In standard pipeline mode, comparisons are only 
#' made using unpooled data.
#' 
#' @import utils
#'
#' @inheritParams wrangle_raw_data
#' @param file The filename of a correctly-formatted CSV file to estimate mutation rates from.
#' Supplying this will cause the function to run in single-file mode.
#' @param comparisons Logical value indicating if a comparison file should be generated. If `file`
#' is also supplied, the function will generate a comparison even if only one strain is available.
#' 
#' @examples
#' run_fluxxer(file = "./data/raws/FLUCTEST 1 2020 09 24.csv",
#'             comparisons = "AB3", 
#'             overwrite = TRUE)
#'             
#' @return
#' A CSV file ending in `.output.csv`, containing mutation rate estimates and other statistics. If
#' `comparisons` is true, an additional CSV containing mutation rate comparisons and p-values.
#'
#' @export

run_fluxxer <- function(file = NULL, comparisons = TRUE, overwrite = FALSE){
  
  # Set mode explicitly for my own sanity
  runMode <- ifelse(is.null(file), yes = "standard", no = "single")
  
  # Create standard output folders regardless of mode
  outputPath <- prep_export(mode = "analyzed", overwrite)
  
  ##### Single-file mode #####
  if(runMode == "single"){
    
    # test for file validity
    if(utils::file_test("-d", file)){
      stop("Supplied file appears to be a folder. If running the standard pipeline, do not supply a filename.")
    }
    
    # extract filename
    baseName <- sub(".csv", "", basename(file))
    
    # run core fluxxer function
    calculate_mut_rate(filename = file,
                       outputPath, 
                       outputPrefix = baseName, 
                       comparisons)
    
    ##### Standard pipeline mode #####
  } else if(runMode == "standard"){
    
    # Test for standard pipeline dirs
    inputPath <- "./output/wrangled"
    if(!dir.exists(inputPath)){
      stop("Folder /wrangled/ not found. In standard mode, the pipeline must be run in order.")
    }
    
    # Define pooled/unpooled filenames
    pooledList <- grep("_pooled", dir(inputPath), value = TRUE)
    unpooledList <- grep("_unpooled", dir(inputPath), value = TRUE)
    
    # Run core fluxxer function on pooled, then unpooled, data
    for(file in pooledList){
      baseName <- sub(".csv", "", file)
      calculate_mut_rate(filename = paste0(inputPath, "/", file),
                         outputPath,
                         outputPrefix = baseName, 
                         comparisons = FALSE)
    }
    for(file in unpooledList){
      baseName <- sub(".csv", "", file)
      calculate_mut_rate(filename = paste0(inputPath, "/", file),
                         outputPath,
                         outputPrefix = baseName, 
                         comparisons = TRUE)
    }
  }
  
  message("\nDone. Check /analyzed/ for outputs.\n")
  
  
  return(invisible())
  
}


#' Calculate mutation rates.
#' 
#' A version of `calculateMutRate()` from the original [`fluxxer.R`](https://github.com/barricklab/barricklab/blob/master/fluxxer.R)
#' script, minimally-modified and converted to an R function.
#' 
#' @details
#' This function is made available for reference only. Do not call this function manually - 
#' use [`run_fluxxer()`] instead, which supplies the appropriate arguments to this function. This 
#' version does not produce plots, as it is handled by the pipeline function [`plot_fluxxer()`].
#' 
#' @import rSalvador tidyverse
#'
#' @param filename A correctly-formatted CSV to estimate mutation rates from.
#' @param outputPath The directory to save output files to.
#' @param outputPrefix A prefix to use when saving the output file, which will be named like
#' `<outputPrefix>.output.csv`
#' @param comparisons Logical. Indicates if a comparison file should be generated.
#' 
#' @examples
#' run_fluxxer(file = "./data/raws/FLUCTEST 1 2020 09 24.csv",
#'             comparisons = "AB3", 
#'             overwrite = TRUE)
#'             
#' @return
#' A CSV file ending in `.output.csv`, containing mutation rate estimates and other statistics. If
#' `comparisons` is true, an additional CSV containing mutation rate comparisons and p-values.
#' 
#' @export

calculate_mut_rate <- function(filename = NULL,
                               outputPath,
                               outputPrefix = "",
                               comparisons = FALSE){
  
  # require libraries:
  # suppressMessages(library(rsalvador))
  # suppressMessages(library(tidyverse))
  # suppressMessages(library(cowplot))
  # suppressMessages(library(optparse))
  
  # if ((output_prefix!= "") && !grepl('[./]$', output_prefix)) {
  #   output_prefix = paste0(output_prefix, ".")
  # }
  # 
  # if (is.null(filename)) {
  #   stop("No valid file supplied.")
  # }
  
  #read in file specified. Must be in same directory
  #for testing
  #data <- read_csv("example_dataset_2.csv") 
  data = read_csv(filename)  
  
  #do some checks of the input files to expand abbreviations
  data$plate = tolower(data$plate)
  data = mutate(data, plate = ifelse( (plate == "n") | (plate == "ns") | (plate == "count"), "nonselective", plate))
  data = mutate(data, plate = ifelse(plate == "s", "selective", plate))
  
  data$strain = as.factor(data$strain)
  data$plate = as.factor(data$plate)
  
  strains = levels(data$strain)
  
  #identify # of strains, use to build empty data frame
  num_strains <- length(strains)
  cat("Found", num_strains, "strains:\n")
  cat(strains, sep='\n')
  
  output_data <- tibble()
  
  #cycle through each column to calculate mutatation rate and confidence  
  for(this.strain in strains) {
    cat("\nSTRAIN:", this.strain, "\n")
    #locate Non_selective separator
    this.strain.data = data %>% filter(strain==this.strain)
    
    #extract selective values
    selective.rows = this.strain.data %>% filter(plate=="selective")
    nonselective.rows = this.strain.data %>% filter(plate=="nonselective")
    num_selective = nrow(selective.rows)
    num_nonselective = nrow(nonselective.rows)
    
    cat("Number of selective plate counts:", num_selective, "\n") 
    cat("Number of nonselective plate counts:", num_nonselective, "\n")
    
    if (num_selective == 0 || num_nonselective == 0 ) {
      cat("***ERROR! Did not find plate counts for selective/nonselective. Skipping strain.\n")
      next
    }
    
    nonselective_cell_counts = mean(nonselective.rows$CFU/nonselective.rows$fraction)
    cat("Estimated cells per culture:", nonselective_cell_counts, "(", nrow(nonselective.rows), "nonselective plates )\n")
    
    #all selective plates must have the same fraction
    selective_fraction_list = selective.rows %>% count(fraction)
    if (nrow(selective_fraction_list) > 1) {
      cat("***ERROR! Multiple fractions found for selective plates. Skipping strain.\n")
      next
    }
    selective_fraction = selective_fraction_list$fraction[1]
    cat("Fraction or efficiency of selective cultures plated (e):", selective_fraction, "\n")
    
    if (selective_fraction == 1) {
      m = newton.LD(selective.rows$CFU)
    } else {
      m = newton.LD.plating(selective.rows$CFU, e=selective_fraction)
    }
    
    mu = m / nonselective_cell_counts
    cat("Maximum likelihood mutation rate (mu):", mu, "\n")
    
    if (selective_fraction == 1) {
      CI = confint.LD(selective.rows$CFU, alpha=0.05)/nonselective_cell_counts
    } else {
      CI = confint.LD.plating(selective.rows$CFU, alpha=0.05, e=selective_fraction)/nonselective_cell_counts
    }
    cat("         95% confidence interval (mu): [", CI[1], ",", CI[2] , "]\n")
    
    output_data = rbind(output_data, data.frame(strain = this.strain, num_nonselective_plates = num_nonselective, num_selective_plates = num_selective, selective_fraction = selective_fraction, avg_cells_per_culture = nonselective_cell_counts, mu = mu, CI.95.lower = CI[1], CI.95.higher = CI[2]))
  }
  
  write_csv(output_data, paste0(outputPath, "/", outputPrefix, ".output.csv"))
  
  # ##make chart for pretty values
  # plot <- ggplot(output_data, aes(x = strain, y = mu)) +
  #   geom_point() +
  #   geom_linerange(aes(ymin = CI.95.lower, ymax = CI.95.higher)) +
  #   scale_y_log10() + 
  #   ggtitle("Mutation Rates") + 
  #   xlab("Strains") +
  #   ylab("Mutation rate MLE") +
  #   annotation_logticks(sides = "l")+
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  # 
  # save_plot(paste0(output_prefix, "plot.pdf"), plot)
  
  
  if (!comparisons) {
    return()
  }
  
  comparison_data = data.frame()
  # Optional code that performs comparisons between rates
  
  
  
  for(i in 1:length(strains)) {
    j = i
    while(T) {
      j = j + 1
      if (j > length(strains)) {
        break
      }
      cat("\nComparing mutation rates...", "\n")
      cat("  STRAIN 1:", strains[i], "\n")
      cat("  STRAIN 2:", strains[j], "\n")
      
      this.strain.i = strains[i]
      this.strain.j = strains[j]
      
      this.strain.data.i = data %>% filter(strain==this.strain.i)
      this.strain.data.j = data %>% filter(strain==this.strain.j)
      
      selective.rows.i = this.strain.data.i %>% filter(plate=="selective")
      selective.rows.j = this.strain.data.j %>% filter(plate=="selective")
      
      nonselective.rows.i = this.strain.data.i %>% filter(plate=="nonselective")
      nonselective.rows.j = this.strain.data.j %>% filter(plate=="nonselective")
      
      if (nrow(selective.rows.i) == 0 || nrow(nonselective.rows.i) == 0 ) {
        cat("***ERROR! Did not find plate counts for selective/nonselective. Skipping pair\n")
        next
      }
      
      if (nrow(selective.rows.i) == 0 || nrow(nonselective.rows.j) == 0 ) {
        cat("***ERROR! Did not find plate counts for selective/nonselective. Skipping pair\n")
        next
      }
      
      #all selective plates must have the same fraction
      selective_fraction_list.i = selective.rows.i %>% count(fraction)
      if (nrow(selective_fraction_list.i) > 1) {
        cat("***ERROR! Multiple fractions found for selective plates. Skipping pair\n")
        next
      }
      selective_fraction.i = selective_fraction_list.i$fraction[1]
      
      selective_fraction_list.j = selective.rows.j %>% count(fraction)
      if (nrow(selective_fraction_list.j) > 1) {
        cat("***ERROR! Multiple fractions found for selective plates. Skipping pair\n")
        next
      }
      selective_fraction.j = selective_fraction_list.j$fraction[1]
      
      nonselective_cell_counts.i = mean(nonselective.rows.i$CFU/nonselective.rows.i$fraction)
      nonselective_cell_counts.j = mean(nonselective.rows.j$CFU/nonselective.rows.j$fraction)
      
      cat("  R:", nonselective_cell_counts.j/nonselective_cell_counts.i, "\n")
      cat("  e1:", selective_fraction.i, "\n")
      cat("  e2:", selective_fraction.j, "\n")
      
      # Use simpler rSalvador function with plating efficiencies are 100%
      # because it is more robust to failures...
      this.result = c()
      if ((selective_fraction.i==1) & (selective_fraction.j==1)) {
        this.result = LRT.MK(
          selective.rows.i$CFU, 
          selective.rows.j$CFU, 
          R = nonselective_cell_counts.j/nonselective_cell_counts.i
        )
      } else {
        this.result = LRT.LD.plating(
          selective.rows.i$CFU, 
          selective.rows.j$CFU, 
          R = nonselective_cell_counts.j/nonselective_cell_counts.i,
          e1 = selective_fraction.i,
          e2 = selective_fraction.j
        )
      }
      this.p.value = this.result[2]
      
      cat("  p-value:", this.p.value, "\n")
      
      comparison_data = rbind(comparison_data, data.frame(strain.1 = this.strain.i, strain.2 = this.strain.j, p.value = this.p.value))
      
    }
  }
  
  write_csv(comparison_data, paste0(outputPath, "/", outputPrefix, ".comparisons.csv"))
  
  
}
