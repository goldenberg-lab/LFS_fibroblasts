# read and join all four plates together.

# old speed comparison in this comment block.
#####
# Comparing speed of dplyr bind_rows with data.table rbindlist, both are
# essentially the same speed, no significant difference between the two methods.
# import_data_dplyr <- function(){
#   stopifnot(require(tidyverse) && require(data.table))
#   data_dir <- './data/Datasets/'
#   csvs <- list.files(data_dir) %>%
#     .[grepl("\\.csv", .)] %>% paste0(data_dir, .)
#
#   lapply(csvs, function(f){ read_csv(f) %>% mutate(Plate = str_extract(f, "\\d"))}) %>%
#     bind_rows(.)
#
# }
#
#
# import_data_dt <- function(){
#   stopifnot(require(tidyverse) && require(data.table))
#   data_dir <- './data/Datasets/'
#   csvs <- list.files(data_dir) %>%
#     .[grepl("\\.csv", .)] %>% paste0(data_dir, .)
#
#   lapply(csvs, function(f){ read_csv(f) %>% mutate(Plate = str_extract(f, "\\d"))}) %>%
#     rbindlist(.)
#
# }
#
# library(microbenchmark)
# microbenchmark(import_data_dplyr(), import_data_dt(), times = 10)
#
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# import_data_dplyr() 6.593090 6.876078 7.374450 7.549870 7.831120 8.181775    10
# import_data_dt() 6.540901 7.351698 7.706112 7.799534 8.009514 9.034379    10
#####

# Import the original Datasets as received from Miriam, merging the 4 files into
# a single table.
# path : character string, path to folder containing the data files.
import_data <- function(data_dir = './data/Datasets/'){
  stopifnot(require(tidyverse))
  csvs <- list.files(data_dir) %>%
    .[grepl("\\.csv", .)] %>% paste0(data_dir, .)

  lapply(csvs, function(f){ read_csv(f,col_types = cols(.default = col_double())) %>% mutate(Plate = str_extract(f, "Plate_\\d"))}) %>%
    bind_rows(.)
}

