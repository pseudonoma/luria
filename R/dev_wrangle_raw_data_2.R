# wrangle_raw_data, v2
# for use with the new template

wrangle_raw_data_2 <- function(templateFile){
  # ARGS:
  # templateFile - The Excel workbook, each sheet a separate rep; each workbook a separate project.
  # ?

  # Things to consider
  # countFract no longer useful, will be calculated by row
  # "Well": if used in raw, then parse in wrangler. if no parse in wrangler, then use row/col?
  #

  ###

  # Read in the file
  allData <- openxlsx::read.xlsx(file)


}
