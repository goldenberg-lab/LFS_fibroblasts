# Half finished, might be useful functions that part way through implementing I
# realised will not help in the way I want them to.


##### This idea was that if I separate the rows into different training and
##### testing sets then I could compare the performance where the same rows are
##### not in the training set that are in the test set and when the same rows
##### are in both, but now I'm thinking there isn't any reason
##### to assume that separating by different rows wouldn't perform as well if it
##### is a row effect, since the row difference could still hold even if I split
##### the sets in this way so that it would still learn this difference in the
##### test set. i.e. the row effect would generalize to all instances of
##### different rows I have sample in so it could still be a well performing
##### model.
#' Split the LFS Fibroblast data
#'
#' Split the data into n bins for ten fold cross validation. If grps is not
#' NULL then the columns in grps will be kept together in each fold, and
#' splitting the bins to have their number of observations as even as possible.
#' If grps is NULL then it is assumed each row is it's own group.
#'
#' @param dat a tibble of all data to be split into 10 bins.
#' @param bincol a sym to be the column name for the bin indicator column
#' @param groups a list of quos which are the columns that define the groups.
#' @param response a sym which is the column name that contains the classes
#' @param n an integer indicating the number of bins to split the data into.
#'
#' @return a tibble with an additional column indicating bin.
#'
#' @export
split_dat <- function(dat, bincol, grps, response, n = 10){
  stopifnot(require(tidyverse))
  browser()
  if(is.null(grps)){
    dat <- dat %>% mutate(grp = 1:nrow(dat))
    grps <- list(quo(grp))
  }
  bincol <- enquo(bincol); response <- enquo(response)

  classes <- unique(dat %>% select(!!response) %>% .[[1]])
  sep_by_class <- lapply(classes, function(class, x){x %>% filter(!!response == class)}, dat)


  grpsizes <- dat %>%
    group_by(!!! grps) %>%
    mutate(grp_id = group_indices()) %>%
    group_by(grp_id) %>%
    summarize(n = n()) %>%
    arrange(desc(n))

  bincounts <- rep(0, n)
  binassignments <- rep(0, nrow(grpsizes))
  for(i in 1:nrow(grpsizes)){
    bin <- min(which(bincounts == min(bincounts)))
    bincounts[bin] <- bincounts[[bin]] + grpsizes$n[[i]]
    binassignments[i] <- bin
  }
  grpsizes %>% mutate(!!bincol := binassignments) %>%
    left_join(dat, ., by = !!!groups)
}











