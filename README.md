
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmdref

<!-- badges: start -->
<!-- badges: end -->

The goal of rmdref is to help manage reference files for Rmarkdown.

## Installation

You can install the development version of rmdref like so:

``` r
remotes::install("joelnitta/rmdref")
```

## Examples

Extract citation keys (beginning with `@`) from an Rmd file (or files):

``` r
library(rmdref)
#> 
#> Attaching package: 'rmdref'
#> The following object is masked from 'package:utils':
#> 
#>     cite

# Write an example Rmd to a temporary file
tempfile <- tempfile(fileext = ".Rmd")
lines <- c(
  "---",
  "title: 'Report'",
  "output_format: html_document",
  "---",
  "This statement is supported by @Nitta2021",
  "[@Foo1983; @Blah2001; but see @Smith2020]."
)
write(lines, tempfile)

# Extract citations
extract_citations(tempfile)
#> # A tibble: 4 × 1
#>   key      
#>   <chr>    
#> 1 Blah2001 
#> 2 Foo1983  
#> 3 Nitta2021
#> 4 Smith2020

# Cleanup
file.remove(tempfile)
#> [1] TRUE
```

More examples to come soon…

## License

[MIT](LICENSE.md)
