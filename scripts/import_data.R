# Data importing and data cleaning functions.

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

#' Import fibroblast Data
#'
#' Import the original Datasets as received from Miriam, merging the 4 files into
#' a single table.
#'
#' @param path a character string, path to folder containing the data files.
#'
#' @return Tibble of fibroblast data
#'
#' @example import_data()
#'
#' @export
import_data <- function(data_dir = './data/Datasets/'){
  stopifnot(require(tidyverse))
  csvs <- list.files(data_dir) %>%
    .[grepl("\\.csv", .)] %>% paste0(data_dir, .)

  lapply(csvs, function(f){ read_csv(f,col_types = cols(.default = col_double())) %>% mutate(Plate = str_extract(f, "Plate_\\d"))}) %>%
    bind_rows(.)
}

#' Remove perfect correlations
#'
#' Check all pairs of numeric cols x,y and if abs(cor(x,y)) == 1, then one of
#' the columns will be kept and the column will be renamed to: "name(x);name(y)".
#'
#' @param dat a tibble of data columns
#' @param threshold a number which if the absolute value of a correlation is
#'                  greater than or equal to it then one of the two columns will
#'                  be removed.
#'
#' @return a tibble with the aforementioned changes applied
#'
#' @export
rmv_correlated_cols <- function(dat, threshold = 1){
  stopifnot(require(tidyverse))
  source('../Helpers/Sort_rowwise.R')
  cors <- dat %>% select_if(is.numeric) %>% cor(use = "na.or.complete") %>%
    as_tibble(rownames = "Var1") %>%
    gather(key = 'Var2', value = 'Cor', -Var1) %>%
    filter(Var1 != Var2 & abs(Cor) >= threshold)
  perfect_cors <- cors %>% sort2cols(Var1, Var2) %>%
     distinct()
  rmv_cols <- perfect_cors$Var1
  rn_cols <- perfect_cors$Var2

  dat %>% select(-one_of(rmv_cols)) %>%
    rename(!!paste0(rn_cols[[1]], ';', rmv_cols[[1]]) := !!rn_cols[[1]],
           !!paste0(rn_cols[[2]], ';', rmv_cols[[2]]) := !!rn_cols[[2]])
}




