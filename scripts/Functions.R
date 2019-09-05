# LFS Fibroblast specific function storage, once script is large enough
# should be split into smaller scripts.

#' Split the LFS Fibroblast data
#'
#' Split the data into n bins for n fold cross validation. Separated randomly,
#' while keeping the same ratio of classes in each bin.
#'
#' @param dat a tibble of all data to be split into 10 bins.
#' @param bincol a sym to be the column name for the bin indicator column
#' @param response a sym which is the column name that contains the classes
#' @param n an integer indicating the number of bins to split the data into.
#'
#' @return a tibble with an additional column indicating bin.
#'
#' @export
split_dat <- function(dat, bincol, response, n = 10){
  stopifnot(require(tidyverse))
  bincol <- enquo(bincol); response <- enquo(response)

  classes <- unique(dat %>% select(!!response) %>% .[[1]])
  sep_by_class <- lapply(classes,
                         function(class, x){
                           x %>%
                             filter(!!response == class) %>%
                             mutate(!!bincol := sample(1:n, nrow(.), replace = T))
                           }, dat)
  rbind(sep_by_class[[1]], sep_by_class[[2]])
}
