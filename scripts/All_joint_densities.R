# create an object for each 2d visualization.

library(tidyverse);library(cowplot);library(NMF)
library(gridExtra);library(ggforce)
source('./scripts/import_data.R')
source('./scripts/Visualization_helpers.R')
source('./scripts/diffDensity2d.R')

varGtrZero <- function(x){(!is.numeric(x) || var(x, na.rm = T) > 0)}
dat <- import_data('./data/Datasets/') %>% select_if(varGtrZero)

i <- 1
run_diffDensity2d <- function(x, y, dat, grp, directory){
  stopifnot(require(MASS) & require(tidyverse))
  i <<- i + 1
  x <- enquo(x); y <- enquo(y); grp <- enquo(grp)
  dendiff <- diffDensity2d(x, y, dat, grp)

  plt <- density2d_my_style(dendiff, x, y, quo(density))

  plt_name = paste(directory, quo_name(x), quo_name(y), '2ddensity', sep = '_')
  save(list = 'plt', file = plt_name)

  print(i)

  plt_name
  #ggsave(filename = plt_name, plot = plt, device = 'pdf')
  # ggsave is unable to allocate enough memory for some variable combinations,
  # couldn't figure out how to catch this particular error perhaps a problem for
  # another time.
}

mycenter <- function(x){x - mean(x, na.rm = T)}
centeredDat <- dat %>% dplyr::mutate(mutant = Row > 3) %>%
  dplyr::select(-S, -M, -Row, -Column, -Plate) %>%
  group_by(mutant) %>%
  dplyr::mutate_if(.predicate  = is.numeric, .funs = list(mycenter))

measures_char <- names(centeredDat)[!grepl('mutant', names(centeredDat))]
combinations <- expand.grid(measures_char, measures_char, stringsAsFactors = F) %>%
  sort2cols(Var1, Var2) %>% dplyr::filter(Var1 != Var2) %>% distinct()

run_diffDensity2d(Eccentricity, Solidity,
                  centeredDat, mutant, './2ddensity_diffs_test/')

plts <- mapply(FUN = run_diffDensity2d, syms(combinations[[1]][(1:3)]), syms(combinations[[2]][(1:3)]),
               MoreArgs = list(centeredDat, sym("mutant"), './2ddensity_diffs_test/'),
               SIMPLIFY = F)
