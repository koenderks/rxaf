# rxaf: Read and Analyze Dutch XML Audit Files <img src='https://github.com/koenderks/rxaf/raw/master/man/figures/logo.png' width='150' height='150' align='right'/>

The `rxaf` package provides functionality in R for reading and analyzing Dutch XML Audit Files (XAF), which are used in the Netherlands for financial and auditing purposes.

## Installation

You can install `rxaf` directly from the GitHub repository. To do this, you will need to have `remotes` package installed on your system. Once you have this package, you can install `rxaf` by running the following command in your terminal:

```r
# install.packages("remotes")
remotes::install_github("koenderks/rxaf")
```

### Usage

Here is a minimal working example of how to use `rxaf` to read and analyze a XAF file:

```r
library(rxaf)

# Read a XAF file as data.frame
dataset <- read_xaf("https://github.com/koenderks/rxaf/raw/master/tests/testthat/ExactOnline.xaf")

# Create a balance sheet or income statement at december 20, 2023
balance_sheet(dataset, date = "20-12-2023") # or income_statement(dataset, date = "20-12-2023")

#                      Categorie Nummer                            Grootboek      Saldo
# 1      Vaste activa en passiva   0110 Personeelsactiviteiten "buitenshuis"    8264.46
# 2                                0300                             Gebouwen   16987.60
# 3                                0301                Afschrijving gebouwen   -5000.00
# 4                    Subtotaal                                               20252.06
# 5  Vlottende activa en passiva   1000                                  Kas  -34358.00
# 6                                1100                                 Bank  396704.46
# 7                                1290         Kruisposten liquide middelen  -11162.00
# 8                                1300                           Debiteuren -304220.09
# 9                                1310      Voorziening oninbare debiteuren   -1250.00
# 10                               1500                Af te dragen BTW hoog  -15898.04
# 11                               1510                Af te dragen BTW laag    -649.37
# 12                               1520                      Te vorderen BTW   12427.83
# 13                               1600                          Crediteuren  -60400.00
# 14                               1670           Tussenrekening assurantiën   13250.00
# 15                               1700                        Loonheffingen   77059.00
# 16                   Subtotaal                                               71503.79
# 17            Tussenrekeningen   2000                          Vraagposten   31127.70
# 18                               2100                          Netto lonen    2298.54
# 19                   Subtotaal                                               33426.24
# 20          Voorraadrekeningen   3310               Voorraad indoor sports     -48.17
# 21                               3320              Voorraad outdoor sports   -3677.90
# 22                               3330                Voorraad supplementen    -155.00
# 23                   Subtotaal                                               -3726.07
# 24                      Totaal                                              121301.02

# Write a XAF file to csv
write.csv(dataset, "example.csv", row.names = FALSE)
```
