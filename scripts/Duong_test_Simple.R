# Functions to implement a simple/specific duong test.

#' sigma_2
#'
#' Calculate sigma squared value for Duongs test given the assumptions of this
#' pipeline. Under assumption of standard normal multivariate kernel with
#' dimension 2 then R(K) = 1/(4pi)
#' and m2(K) = 1. x, f1x and f2x must be vectors of the same dimension.
#'
#' @param H1 a vector of diagonal entries for a matrix of bandwidths
#' @param H2 a vector of diagonal entries for a matrix of bandwidths
#' @param f1x point density estimate given first sample and f1 kernel for point x
#' @param f2x point density estimate given second sample and f2 kernel for point x
#' @param n1 number of samples from the first distribution
#' @param n2 number of samples from the second distribution
#'
#' @return the sigma^2 value as outlined in Duongs test
sigma_2 <- function(f1x, f2x, H1, H2, n1, n2){
  # ((1/n1)*prod(H1)^(-1/2)*f1x + (1/n2)*prod(H2)^(-1/2)*f2x)*(1/(4*pi))
  ((1/n1)*prod(H1)^(-1)*f1x + (1/n2)*prod(H2)^(-1)*f2x)*(1/(4*pi))
}

#' Simple Duong local difference test
#'
#' Naive/simple implementation of Duong method for local significant differences
#' from nonparametric two_sample tests, using bandwidth.nrd from MASS package as
#' the bandwidth matrix selector, and a gaussian kernel,
#' @param f1 the kernel density estimate using gaussian kernel and some
#' bandwidth H1 for point X
#' @param f2 Similar to f1 but for the second distribution.
#' @param H1 bandwidth used in f1
#' @param H2 bandwidth used in f2
#' @param n1 number of samples for the first distribution
#' @param n2 number of samples for the second distribution
#'
#' @return uncorrected p-value for each x
local_chisq <- function(f1x, f2x, H1, H2, n1, n2){
  #1 - pchisq(1/sigma_2(f1x, f2x, H1, H2, n1, n2) * (f1x - f2x)^2, df = 1)
  1/sigma_2(f1x, f2x, H1, H2, n1, n2) * (f1x - f2x)^2
}

#' local significance test wrapper
#'
#' Run Duongs test with the assumption the kernel density estimate is calculated
#' by the kde2d function from which uses a bivariate normal kernel function.
#'
#' @param dat a tibble containing all variables
#' @param x a symbol of the column name for the x axis in the joint distribution
#' @param y a symbol of the column name for the y axis in the joint distribution
#' @param grp a symbol of logical vector which signifies the groups of data
#' @param xrng 2d vector of region of interest where xrng[1] is lower bound and
#'             xrng[2] is upper bound
#' @param yrng 2d vector for y same as xrng
#'
#' @return a list containing two kde matrices and a matrix of pvalues from duong
#' test to be multiple hypothesis corrected
wrapper_local_sig <- function(dat, x, y, grp, xrng, yrng){
  x <- enquo(x); y <- enquo(y); grp <- enquo(grp)

  # H1 <- dat %>% filter(!!grp == F) %>% select(!!x, !!y) %>%
  #   vapply(., bandwidth.nrd, 0)
  H1 <- 5
  # H2 <- dat %>% filter(!!grp == T) %>% select(!!x, !!y) %>%
  #   vapply(., bandwidth.nrd, 0)
  H2 <- 10
  n1 <- dat %>% filter(!!grp == F) %>% .[[1]] %>% length()
  n2 <- dat %>% filter(!!grp == T) %>% .[[1]] %>% length()

  xlims <- range(dplyr::select(dat, !!x)[[1]])
  ylims <- range(dplyr::select(dat, !!y)[[1]])

  den1 <- dat %>% dplyr::filter(!!grp == F) %>%
    {MASS::kde2d(.[[1]], .[[2]], h = H1, n = 50, lims = c(xlims, ylims))}
  den2 <- dat %>% dplyr::filter(!!grp == T) %>%
    {MASS::kde2d(.[[1]], .[[2]], h = H2, n = 50, lims = c(xlims, ylims))}

  # Chose visually an area with a large relative difference in the joint diff
  # density plot
  f1x <- den1$z[den1$x > xrng[1] & den1$x < xrng[2], den1$y > yrng[1] & den1$y < yrng[2]]
  f2x <- den2$z[den2$x > xrng[1] & den2$x < xrng[2], den2$y > yrng[1] & den2$y < yrng[2]]

  loc_pvals <- local_chisq(f1x, f2x, H1, H2, n1, n2)
  colnames(loc_pvals) <- den1$y[den1$y > yrng[1] & den1$y < yrng[2]]
  pval_tib <- as_tibble(loc_pvals) %>%
    mutate(!!x := den1$x[den1$x > xrng[1] & den1$x < xrng[2]]) %>%
    gather(key = 'tmp', value = 'chisq', -!!x) %>%
    rename(!!y := tmp) %>%
    mutate_all(as.numeric) %>%
    mutate(pval = 1 - pchisq(chisq, df = 1))

  list(den1 = den1, den2 = den2, loc_pvals = pval_tib)
}

#' density list to tibble
#'
#' Convert a list of matrices as returned from the function kde2d into a tibble
#' for easy plotting.
#'
#' @param dens list of matrices as retunred by kde2d
#' @param x symbol name for x variable
#' @param y symbol name for y variable
#'
#' @return tibble with column x y and density for ease of use with ggplot.
dens_to_tibble <- function(dens, x, y){
  stopifnot(require(rlang) & require(tidyverse))
  x <- enquo(x); y <- enquo(y)
  density <- dens$z
  colnames(density) <- dens$y

  as_tibble(density) %>% mutate(!!x := dens$x) %>%
    gather(key = 'tmp', value = 'density', -!!x) %>%
    rename(!!y := tmp) %>% mutate_all(as.numeric)
}

#' My kernel density estimate for 2-d data
#'
#' Estimate the kernel density for 2-d data, using a bivariate standard gaussian,
#' i.e. mean 0, and sd 1. Motivation: I cannot figure out what is wrong with my
#' implementation of Duongs method, so perhaps what is wrong is my assumptions about
#' how kde2d works, so I am implementing it myself so that I fully understand.
#'
#' @param dat tibble containing variable x and y
#' @param x symbol for x variable in dat
#' @param y symbol for y variable in dat
#' @param h vector of bandwidths, h[1] for x and h[2] for y
#' @param lims bounded region to calculate the kde for, default is entire range
#'
#' @return tibble with columns x, y, density indicating the density estimate at
#' point (x,y). columns x and y take the names of the variables in the original
#' tibble respectively.
mykde2d <- function(dat, x, y, h = c(bandwidth.nrd(x), bandwidth.nrd(y)),
                    n = 100, lims = c(range(x), range(y))){
  x <- enquo(x); y <- enquo(y)
  x <- dat %>% select(!!x) %>% .[[1]]
  y <- dat %>% select(!!y) %>% .[[1]]
  n <- rep(n, length.out = 2)
  # get a sequence of points to estimate the kd at given the range
  cx <- seq.int(lims[1], lims[2], length.out = n[1])
  cy <- seq.int(lims[3], lims[4], length.out = n[2])
  h <- rep(h, length.out = 2)
  #TODO finish this reimplementation of kde2d see if the changes better match with
  #the assumptions you made implementing duongs test.


}

if (sys.nframe() == 0){
  # Run some tests for the above functions, on the joint distribution that was
  # centered over top of each other.
  source('./scripts/import_data.R')
  source('./scripts/Visualization_helpers.R')
  stopifnot(require(MASS) & require(tidyverse))

  dat <- import_data()

  smldat <- dat %>% mutate(mutant = Row > 3) %>%
    group_by(mutant) %>%
    mutate(CInt_3 = CInt_3 - mean(CInt_3), CInt_4 = CInt_4 - mean(CInt_4)) %>%
    select(CInt_3, CInt_4, mutant) %>%
    ungroup()

  ggplot(smldat, aes(x = CInt_3, y = CInt_4, colour = mutant)) + geom_point()

  test <- wrapper_local_sig(smldat, CInt_3, CInt_4, mutant, c(-Inf, Inf), c(-Inf, Inf))

  dens_to_tibble(test$den1, CInt_3, CInt_4) %>%
    density2d_my_style(., quo(CInt_3), quo(CInt_4), quo(density), binwidth = 0.01)

  dens_to_tibble(test$den2, CInt_3, CInt_4) %>%
    density2d_my_style(., quo(CInt_3), quo(CInt_4), quo(density), binwidth = 0.01)

  density2d_my_style(test$loc_pvals, quo(CInt_3), quo(CInt_4), quo(pval), binwidth=0.1)
  # See how many squares are significant with no correction
  test$loc_pvals %>% mutate(significant = pval < 0.05) %>%
    ggplot(., aes(x = CInt_3, y = CInt_4, fill = significant)) + geom_tile()

  # With correction
  adjusted <- test$loc_pvals %>%
    mutate(adj_pval = p.adjust(pval, method = 'hochberg'),
           bon_adj_pval = p.adjust(pval, method = 'bonferroni'),
           mybon_adj = pval*4355,
           hoch_significant = adj_pval < 0.05,
           bon_significant = bon_adj_pval < 0.05,
           significant = hoch_significant & bon_significant)

  ggplot(adjusted, aes(x = x, y = y, fill = significant)) + geom_tile()

  # Try not centering the CInt_3 and CInt_4 and then calculate test statistic.
  # Did not use wrapper function to easily generate the plot and then select region of interest.
  smldat <- dat %>% mutate(mutant = Row > 3) %>%
    select(CInt_3, CInt_4, mutant) %>%
    ungroup()

  H1 <- smldat %>% filter(mutant == F) %>% select(CInt_3, CInt_4) %>%
    vapply(., bandwidth.nrd, 0)
  # Bandwidth selected by bandwidth.nrd is a scalar, Duong method suggests a
  # full matrix even over a diagonal matrix, perhaps the density estimate could
  # be improved with a better choice of bandwidth matrix?
  H2 <- smldat %>% filter(mutant == T) %>% select(CInt_3, CInt_4) %>%
    vapply(., bandwidth.nrd, 0)

  n1 <- smldat %>% filter(mutant == F) %>% .[[1]] %>% length()
  n2 <- smldat %>% filter(mutant == T) %>% .[[1]] %>% length()

  xlims <- range(dplyr::select(smldat, CInt_3)[[1]])
  ylims <- range(dplyr::select(smldat, CInt_4)[[1]])

  den1 <- smldat %>% dplyr::filter(mutant == F) %>%
    {MASS::kde2d(.[[1]], .[[2]], h = H1, n = 100, lims = c(xlims, ylims))}
  den2 <- smldat %>% dplyr::filter(mutant == T) %>%
    {MASS::kde2d(.[[1]], .[[2]], h = H2, n = 100, lims = c(xlims, ylims))}

  # Chose visually an area with a large relative difference in the joint diff
  # density plot
  diff_mat <- den1$z - den2$z
  rownames(diff_mat) <- den1$x
  colnames(diff_mat) <- den1$y

  diff_den <- as_tibble(diff_mat, rownames = 'x') %>%
    gather(key = 'y', value = 'z', -x) %>%
    mutate(x = as.numeric(x), y = as.numeric(y))

  density2d_my_style(diff_den, quo(x), quo(y), quo(z))

  f1x <- den1$z[den1$x >= 0 & den1$x <= 2e08, den1$y > 0 & den1$y < 2e08]
  f2x <- den2$z[den2$x >= 0 & den2$x <= 2e08, den2$y > 0 & den2$y < 2e08]

  local_chisq(f1x, f2x, H1, H2, n1, n2)

  # Simulate joint distributions see what the cut off is for significant findings

  dat <- tibble(x = c(rnorm(1000000, 0, 1), rnorm(1000000, 0, 1)),
                y = c(rnorm(1000000, 0, 1), rnorm(1000000, 0,1)),
                grp = c(rep(F, 1000000), rep(T, 1000000)))

  ggplot(dat, aes(x = x, y = y, colour = grp)) + geom_point()

  test <- wrapper_local_sig(dat, x, y, grp, c(-Inf,Inf), c(-Inf,Inf))

  dens_to_tibble(test$den1, x, y) %>%
    density2d_my_style(., quo(x), quo(y), quo(density), binwidth = 0.01)

  dens_to_tibble(test$den2, x, y) %>%
    density2d_my_style(., quo(x), quo(y), quo(density), binwidth = 0.01)

  density2d_my_style(test$loc_pvals, quo(x), quo(y), quo(pval), binwidth=0.01)
  # See how many squares are significant with no correction
  test$loc_pvals %>% mutate(significant = pval < 0.05) %>%
    ggplot(., aes(x = x, y = y, fill = significant)) + geom_tile()

  # With correction
  adjusted <- test$loc_pvals %>%
    mutate(adj_pval = p.adjust(pval, method = 'hochberg'),
           bon_adj_pval = p.adjust(pval, method = 'bonferroni'),
           mybon_adj = pval*4355,
           hoch_significant = adj_pval < 0.05,
           bon_significant = bon_adj_pval < 0.05,
           significant = hoch_significant & bon_significant)

  ggplot(adjusted, aes(x = x, y = y, fill = significant)) + geom_tile()

  hist(adjusted$pval)
  hist(adjusted$adj_pval)
}

rchi <- rchisq(1e6,df=1)
q_chi <- quantile(rchi,probs = seq(0,1,0.01))
q_emp <- quantile(test$loc_pvals$chisqval,probs = seq(0,1,0.01))

tibble(chisqvals = c(test$loc_pvals$chisq, rchi), quant = c(q_emp, q_chi))

ff = function(cc,v1,v2) {
  (coef(lm(I(cc*v1) ~ v2))[2] - 1)^2
}
cstar <- optimise(ff,interval = c(0,1000),v1=q_chi,v2=q_emp)$minimum
hist(rchi * cstar)

plot(q_chi[-c(1,101)], quantile(q_emp / cstar,seq(0.01,0.99,0.01)))
abline(a=0,b=1)


