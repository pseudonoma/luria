
<!-- README.md is generated from README.Rmd. Please edit that file -->

# luria

<!-- badges: start -->
<!-- badges: end -->

This package provides a general framework and data analysis pipeline for
conducting fluctuation analysis. Its functions are built around a
modification of the fluxxer.R script written by J. Barrick & D.
Deatherage, which in turn calls functions from the rSalvador package to
produce mutation rate estimates and comparisons. It should work fine
with any microbial system, but was primarily built to work with
*Escherichia coli* and *Acinetobacter baylyi* experiments in the
Engelstaedter/Letten group - your mileage with other systems may vary.

The package has slightly more functionality under the hood, so you
should read the function documentation for more information.

## Setup

To install the package, run the following lines:

``` r
install.packages("devtools")
devtools::install_github("pseudonoma/luria")
```

You may also wish to record your data using the bundled Excel workbook
template. You can save a copy of the Excel file by running the following
line, replacing PATH with the folder you want to save the file to:

``` r
get_template("PATH")
```

The first two sheets of the template are usage guides. Once you’ve read
them, you can begin recording your data on the empty sheets. These
sheets are named “Replicate 1” and “Replicate 2”, but you can rename
them as you please (e.g. to the date of the experiments), copying the
format to additional sheets as needed.

## Use

The pipeline is split into three sections: wrangling the raw data into a
pipeline structure, analyzing the data, and finally plotting and/or
exporting the data. Output files from each section is saved to the
folder `output/`.

To begin, load the package into the library by running:

``` r
library(luria)
```

### Part 1 - Wrangling

Before analysis can begin, your raw data has to be processed into the
correct format. If your raw data is contained in the Excel workbook
template, run:

``` r
wrangle_raw_data("PATH")
```

replacing PATH with the location of your Excel workbook. By default,
this assumes that (a) your *Count* populations were diluted
1:10<sup>5</sup>; and (b) any *Selective* wells missing from the raw
file were in fact plated but had no growth. If you aren’t using the
standard design, and for more information on additional options, read
the documentation by calling `?wrangle_raw_data`.

At this point, your raw data is now a tidy CSV file with column names
`strain`, `plate`, `fraction`, and `CFU`, and is contained in the folder
`output/wrangled/`. It needs to be converted into the structure the
pipeline expects, so run:

``` r
wrangle_clean_data()
```

This produces a pair of CSV files, also in `output/wrangled/`: a
“pooled” file consisting of all your replicates combined into one, and
an “unpooled” file, where each replicate is kept separate.

If, for whatever reason, your data is not in the Excel template and is
instead already in the tidy CSV format with the correct column names,
you probably already have some experience with this pipeline. In this
case, you should run `wrangle_clean_data()` but supply the file via the
`dataFile` argument.

### Part 2 - Analysis

Now that your raw data is processed, obtaining the mutation rate
estimates is straightforward. Simply run:

``` r
run_fluxxer()
```

By default, this produces three files, this time saved to
`output/analyzed/`. Two are the mutation rate estimates from the pooled
and unpooled data, while the third is a “comparisons” file, containing
pairwise comparisons of every replicate’s mutation rate estimate.

### Part 3 - Plotting and export

The data in `output/analyzed/` can be plotted automatically by running:

``` r
plot_fluxxer()
```

This produces a dotplot with both the replicate mutation rates and the
pooled estimate in a single plot.

Additionally, if you would like to export the pooled mutation rate as an
RData object for later use (instead of copy-pasting, for example), you
can extract the pooled estimate by running:

``` r
extract_mutrates()
```

Plots and extracted mutation rates are by default saved back into
`output/analyzed/`.

## A Little More Detail

The package actually implicitly runs in one of two modes: the standard
pipeline mode described above, and a single-file mode which allows you
to pass some of the functions individual files without having to follow
the pipeline format.

### Pipeline mode

When wrangling raw files with `wrangle_raw_data()` or
`wrangle_clean_data()`, multiple datasets can be wrangled and saved to
the standard output folder `output/wrangled/`. Each file wrangled this
way constitutes a “project”; by default the original filename is
preserved in output files as a “project name”, to which is appended (for
example) a suffix like `.output` that indicates what kind of output file
it is. You can change the filename of the wrangled file using the
`saveAs` argument in either of the `wrangle_` functions.

In pipeline mode, `run_fluxxer()`, `plot_fluxxer()`, and
`export_mutrates()` automatically process the appropriate input files by
project, meaning for example that the file pair
`fluctest1_unpooled.output.csv` and `fluctest1_pooled.output.csv` is
understood to belong to the same dataset and are processed together.

### Single-file mode

The functions `run_fluxxer()`, `plot_fluxxer()`, and
`extract_mutrates()` can also take individual files via the `file`
argument, like:

``` r
run_fluxxer(file = "./data/my_clean_data.csv")
```

Doing so runs the function in single-file mode, forcing the functions to
take the input file as-is without assuming there are matched pairs of
input files ordered by project. This mode is useful if, for example, you
would like a quick look at your replicate estimates without needing to
combine them. Naturally, it’s up to you to make sure the input file
format is valid.
