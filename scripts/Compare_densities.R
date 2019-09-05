# calculate 2dKernelDensity p-values using bootstrap samples to generate one of
# the 2d kernels. Test statistic is the number of points in the 2dkernel
# estimate which are statistically significant under the binomial distribution
# for each point estimated by the previous classes kernel density.
library(tidyverse)
source('./scripts/Visualization_helpers.R')
source('./scripts/import_data.R')
source('./scripts/diffDensity2d.R')

#' Plot 2d density
#'
#' Plots the joint distribution estimate given a tibble of two variables and the
#' density estimates of their joint distribution. This function comes from an
#' answer to the question:
#' https://stackoverflow.com/questions/28521145/r-calculate-and-plot-difference-between-two-density-countours
#' Thank you eipi10!!!
#'
#' @param dens a tibble of at least three columns
#' @param xcol quosure symbol of the column name for the x-axis
#' @param ycol quosure symbol of the column name for the y-axis
#' @param denscol quosures symbol of the column name for the density values.
#'
#' @return a ggplot object of the 2d density.
#'
#' @export
#'
density2d_my_style <- function(dens, xcol, ycol, denscol){
  ggplot(dens, aes(x = !!xcol, y = !!ycol, z=!!denscol, fill=!!denscol)) +
    geom_tile() +
    stat_contour(aes(colour=..level..), binwidth=0.001) +
    scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0) +
    scale_colour_gradient2(low=scales::muted("red"), mid="white", high=scales::muted("blue"), midpoint=0) +
    guides(colour=FALSE)
}


#' Calculate test statistic for pvalue of two joint densities
#'
#' In this case the test statistic will be the number of "pixels" in the joint
#' distribution which has a significant pvalue.
#'
#' @param prob nxn matrix of expected probabilities of a single observation
#' occuring in a space
#' @param success number of observations that actually occured in the given space.
#' @param trial number of observations in total
#' @param significance threshold of significance for the pvalues from binom.test
#'
#' @return the number of significant binomial distributions spanning the joint
#' probability space.
#'
#' @export
calc_test_statistic <- function(prob, success, trial, significance, alternative = 'two.sided'){
  res <- mapply(function(E_p, count, trial){
                  binom.test(count, trial, p = E_p, alternative = alternative)
                }, prob, success, trial, SIMPLIFY = F)

  list(sum(sapply(res, function(x){x$p.value < significance})), res)
}

#Same function as above, except going to try and vectorise it either using
#data.table or dplyr to calculate the binom.test in a faster manner.
#Q: Can I make the calc_test_statistic function faster?
# calc_test_statistic_fast <- function(prob, success, trial, significance){
#   stopifnot(require(data.table))
#   dt <- data.table(prob = prob, success = success, trial = trial)
#   res <- dt[, pp:=binom.test(x = success, n = trial, p = prob)$p.value, by = c('success', 'trial', 'prob')]$pp
#
#   sum(sapply(res, function(x){x < significance}))
# }
#
# calc_test_statistic_dplyr <- function(prob, success, trial, significance){
#   tib <- tibble(prob = prob, success = success, trial = trial)
#   res <- tib %>% rowwise() %>% mutate(pp = binom.test(success, trial, prob)$p.value) %>% .$pp
#
#   sum(sapply(res, function(x){x < significance}))
# }

#A: These two methods are not faster, I don't think I can get a faster function
#because each calculation is a different probability and count combination, so
#there is no benefit to memoization in this case.

#' Calculate pvalue for two joint densities
#'
#' Calculate a pvalue for the null hypothesis that two joint distibutions are
#' generated from the same underlying distribution. Pvalue is calculated given,
#' the assumption that for a given density point in the 2d_density the number of
#' observations in the second class counted in the same square should have
#' expected value equivalent to the count of the previous class in that square,
#' then our test statistic becomes the count of the number of points out of the
#' total that have a p_value for these binomial distribution that is below some
#' threshold. We can then generate new samples under the null by bootstrapping
#' the second class and calculating the binomial pvalues again. The final pvalue then
#' becomes the percentage of nboot where the random sample has a lesser statistic
#' than the true distribution.
#'
#' @param dat A tibble of data containing col1, col2, and a column indicating
#' the two classes to compare the joint distributions for.
#' @param col1 Symbol of column name for first of the two variables
#' @param col2 Symbol of the column name for the second of the two variables
#' @param grp Symbol of the column name for the group indicator, the column
#' should be a logical vector.
#' @param threshold numeric value which is the significance threshold to classify
#' a binomial test statistic as significant.
#' @param nboot number of bootstrap samples to compare the test statistic over
#'
#' @return pvalue for the area of the density which is significantly different
#' via binomial test.
density_p_value <- function(dat, x, y, grp, threshold = 0.05, nboot = 1000){
  x <- enquo(x); y <- enquo(y); grp <- enquo(grp)

  browser()

  smldat <- dat %>% select(!!x, !!y, !!grp) %>% rmv_outliers(5)

  xlims <- range(dplyr::select(smldat, !!x)[[1]])
  ylims <- range(dplyr::select(smldat, !!y)[[1]])

  # Estimate binoial probabilities with the non-mutant cells
  dens1 <- smldat %>% dplyr::filter(!!grp == F) %>%
    {MASS::kde2d(.[[1]], .[[2]], n = 100, lims = c(xlims, ylims))}

  get_counts <- function(dat, x, y, gridx, gridy){
    x <- enquo(x); y <- enquo(y)
    tmp <- dat %>%
      mutate(x_bin := ((!!x - gridx[[1]]) %/% (gridx[[2]] - gridx[[1]])) + 1,
             y_bin := ((!!y - gridy[[1]]) %/% (gridy[[2]] - gridy[[1]])) + 1) %>%
      group_by(x_bin, y_bin) %>%
      add_count() %>%
      select(x_bin, y_bin, n) %>%
      distinct()
    # fill in implicit missing rows with counts of 0.
    expand.grid(x_bin = 1:length(gridx), y_bin =  1:length(gridy), n = 0L,
                ntotal = nrow(dat)) %>%
      as_tibble() %>%
      left_join(., tmp, by = c('x_bin', 'y_bin')) %>%
      mutate(n = if_else(is.na(n.y), n.x, n.y)) %>%
      select(-n.x,-n.y)
  }

  obs_counts <- get_counts(smldat %>% dplyr::filter(!!grp == T), x, y, dens1$x, dens1$y)
  obs_counts_mat <- matrix(data = obs_counts$n, nrow = 100, ncol = 100, byrow = TRUE)


  res <- calc_test_statistic(prob = dens1$z,
                                   success = obs_counts_mat,
                                   trial = nrow(smldat %>% dplyr::filter(!!grp == T)),
                                   significance = 0.01, alternative = 'greater')

  pvals <- res[[2]]
  true_stat <- res[[1]]

  # Create grid of p.values to try and see where in joint distribution the
  # significant pixels are. Compare with the difference in joint distributions,
  # to gauge if these binomial p.values seem reasonable.
  tbp <- tibble(row = rep(1:100, 100), col = (0:9999 %/% 100) + 1, value = sapply(pvals, function(p) p$p.value))

  dens2 <- smldat %>% dplyr::filter(!!grp == T) %>%
    {MASS::kde2d(.[[1]], .[[2]], n = 100, lims = c(xlims, ylims))}

  colnames(dens1$z) <- dens1$y
  dens <- as_tibble(dens1$z - dens2$z)
  dendiff <- dens %>% mutate(x = dens1$x) %>% gather('y', 'density', -x) %>%
    mutate_all(as.numeric) %>% rename(!!x := x, !!y := y)

  A <- ggplot(tbp, aes(x = col, y = row, fill = log10(value))) + geom_tile()
  B <- density2d_my_style(dendiff, x, y, quo(density)) + ggtitle('WT - Mutant joint distribution')
  cowplot::plot_grid(A, B)

  # last part get the boot strapped samples and compare stat to true.
  boot_stats <- lapply(1:999,
                       function(n, dat, gridx, gridy, probabilities){
                         if(n%%10 == 0){print(n)}
                          new_x <- dat %>% select(!!x) %>% sample_frac(size = 1, replace = T)
                          new_y <- dat %>% select(!!y) %>% sample_frac(size = 1, replace = T)
                          dat %>% mutate(!!x := new_x[[1]], !!y := new_y[[1]]) %>%
                            get_counts(x, y, gridx, gridy) %>%
                            {matrix(data = .$n, nrow = 100, ncol = 100, byrow = T)} %>%
                            calc_test_statistic(probabilities, ., nrow(dat),
                                                significance = 0.01)},
                       smldat %>% dplyr::filter(!!grp == F),
                       dens1$x, dens1$y, dens1$z)

  save(list = ls(all.names = TRUE), file = 'compare_densities_test.RData')

  # Right now it's returning the number of binom.tests which are below the
  # threshold, and the returned objects from those tests for each permutation,
  # in future can change this to just return the final calculated pvalue, I have
  # it like this to more easily investigate the results of this test.
  c(true_stat, boot_stats)
}

varGtrZero <- function(x){(!is.numeric(x) || var(x, na.rm = T) > 0)}
dat <- import_data('./data/Datasets/') %>% select_if(varGtrZero) %>%
  mutate(Mutant = Row >= 3)

pvals_Centroid <- density_p_value(dat, Centroid_1, Centroid_2, Mutant)
pvals_Major_minor <- density_p_value(dat, MajorAxisLength, MinorAxisLength, Mutant)


perm_pval <- lapply(pvals, '[[', 1) %>% unlist() %>% {. >= .[[1]]} %>% {sum(.)/length(.)}

######
# Test internal function get_counts
testdat <- tibble(v1 = c(1,1,1,2,2,2), v2 = c(1,2,1,2,1,2))
testdens <- testdat %>% {MASS::kde2d(.[[1]], .[[2]], n = 2, lims = c(range(testdat$v1), range(testdat$v2)))}
get_counts(testdat, v1, v2, testdens$x, testdens$y)

testdat <- tibble(v1 = c(runif(4, min = 0, max = 1), runif(4, min = 1, max = 2)),
                  v2 = c(runif(4, min = 3, max = 4), runif(4, min = 4, max = 5)))
testdens <- testdat %>% {MASS::kde2d(.[[1]], .[[2]], n = 2, lims = c(range(testdat$v1), range(testdat$v2)))}
get_counts(testdat, v1, v2, testdens$x, testdens$y)

# TODO: Problem just found with get_counts, the returned z value is a point density
# estimate in a 2d grid, but my algo is assuming it is a probability for being
# in a square with that point as the bottom right corner, at the very least I
# should make it so that the point estimate is in the center of the square when
# counting successes!

######