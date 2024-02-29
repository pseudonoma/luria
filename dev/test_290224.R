# testbed
# 29/02/24 v0.2.0 - New wrangler function + prerelease

###

library(luria)

# Test folder
testPath <- "./dev/data/testing 2024 02 29"
rawTest <- paste0(testPath, "/rawtest.xlsx")
cleanTest <- paste0(testPath, "/cleantest_AW.csv")

# Raw file tests
wrangle_raw_data(rawTest, exclude = c("Example layout", "Column guide", "RepNoMut"))
wrangle_clean_data()
run_fluxxer()
plot_fluxxer()

# Clean file tests
wrangle_clean_data(cleanTest)
run_fluxxer()
plot_fluxxer()
