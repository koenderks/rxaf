# rxaf: Read and Analyze Dutch XML Audit Files <img src='https://github.com/koenderks/rxaf/raw/master/logo.png' width='150' height='150' align='right'/>

The `rxaf` package provides functionality in R for reading Dutch XML Audit Files (XAF), which are used in the Netherlands for financial and auditing purposes. The functionality is also implemented in a Python package, `pyxaf`, which is available in the corresponding [repository](https://github.com/koenderks/pyxaf).

## Installation

You can install `rxaf` directly from the GitHub repository. To do this, you will need to have `remotes` package installed on your system. Once you have this package, you can install `rxaf` by running the following command in your terminal:

```r
# install.packages("remotes")
remotes::install_github("koenderks/rxaf")
```

### Usage

Here is a minimal working example of how to use `rxaf` to read a XAF file and write it to a CSV file:

```r
library(rxaf)

# Replace 'example.xaf' with the path to the .xaf file you want to read
dataset <- read.xaf("example.xaf")

# The imported XAF file can be exported to CSV
write.csv(dataset, "example.csv", row.names = FALSE)
```
