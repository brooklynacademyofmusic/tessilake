
# tessilake

<!-- badges: start -->
[![codecov](https://codecov.io/gh/skysyzygy/tessilake/branch/master/graph/badge.svg?token=B8FAIEVRJW)](https://codecov.io/gh/skysyzygy/tessilake)
[![R-CMD-check](https://github.com/skysyzygy/tessilake/workflows/R-CMD-check/badge.svg)](https://github.com/skysyzygy/tessilake/actions)
<!-- badges: end -->

Ingests data from Tessitura or other SQL database and caches it using Apache Arrow and Parquet formats

## Installation

Install the latest version of this package by entering the following in R:

```
install.packages("remotes")
remotes::install_github("skysyzygy/tessilake")
```

Create a yml file in your R_USER directory called `config.yml` and add the following keys to it, filling in information
for your particular machine configuration:
```
default:
# tessilake settings
  tessilake:
    depths:
      deep: path_to_deep_storage
      shallow: path_to_shallow_storage
      somewhere: another_path
    tessitura: ODBC_name_for_database
```

## Example

``` r
library(tessilake)

write_cache(data,"giant_table","somewhere","subdir")
read_cache("giant_table","somewhere","subdir")
read_tessi("customers")
read_sql_table("T_CUSTOMER")
read_sql("select top 10 customer_no from T_CUSTOMER")
```
