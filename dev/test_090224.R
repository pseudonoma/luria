# Package test using A.W.'s data

luria::wrangle_raw_data("./dev/data/AW_fluc.xlsx", countPops = 3, saveAs = "AW_test")

excelData <- read.csv("./output/wrangled/AW_test.csv")
csvData <- read.csv("./dev/data/AW_honours.csv")
output <- rbind(excelData, csvData)

output$strain[output$strain == "Rifampicin"] <- "R0"

luria::wrangle_clean_data("./dev/data/AW_combined_test.csv", poolAs = "Pooled")

luria::run_fluxxer()
luria::plot_fluxxer()
luria::extract_mutrates()
