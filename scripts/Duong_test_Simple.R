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
  ((1/n1)*prod(H1)^(-1/2)*f1x + (1/n2)*prod(H2)^(-1/2)*f2x)*(1/(4*pi))
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
local_signif <- function(f1x, f2x, H1, H2, n1, n2){
  browser()
  1 - pchisq(1/sigma_2(f1x, f2x, H1, H2, n1, n2) * (f1x - f2x)^2, df = 1)
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
#' @return matrix of pvalues to be multiple hypothesis corrected
wrapper_local_sig <- function(dat, x, y, grp, xrng, yrng){
  x <- enquo(x); y <- enquo(y); grp <- enquo(grp)

  H1 <- dat %>% filter(!!grp == F) %>% select(!!x, !!y) %>%
    vapply(., bandwidth.nrd, 0)

  H2 <- dat %>% filter(!!grp == T) %>% select(!!x, !!y) %>%
    vapply(., bandwidth.nrd, 0)

  n1 <- dat %>% filter(!!grp == F) %>% .[[1]] %>% length()
  n2 <- dat %>% filter(!!grp == T) %>% .[[1]] %>% length()

  xlims <- range(dplyr::select(dat, !!x)[[1]])
  ylims <- range(dplyr::select(dat, !!y)[[1]])

  den1 <- dat %>% dplyr::filter(!!grp == F) %>%
    {MASS::kde2d(.[[1]], .[[2]], h = H1, n = 100, lims = c(xlims, ylims))}
  den2 <- dat %>% dplyr::filter(!!grp == T) %>%
    {MASS::kde2d(.[[1]], .[[2]], h = H2, n = 100, lims = c(xlims, ylims))}

  # Chose visually an area with a large relative difference in the joint diff
  # density plot
  f1x <- den1$z[den1$x > xrng[1] & den1$x < xrng[2], den1$y > yrng[1] & den1$y < yrng[2]]
  f2x <- den2$z[den2$x > xrng[1] & den2$x < xrng[2], den2$y > yrng[1] & den2$y < yrng[2]]
  local_signif(f1x, f2x, H1, H2, n1, n2)
}



if (sys.nframe() == 0){
  # Run some tests for the above functions, on the joint distribution that was
  # centered over top of each other.
  source('./scripts/import_data.R')
  stopifnot(require(MASS) & require(tidyverse))

  dat <- import_data()

  smldat <- dat %>% mutate(mutant = Row > 3) %>%
    group_by(mutant) %>%
    mutate(CInt_3 = CInt_3 - mean(CInt_3), CInt_4 = CInt_4 - mean(CInt_4)) %>%
    select(CInt_3, CInt_4, mutant) %>%
    ungroup()

  wrapper_local_sig(smldat, CInt_3, CInt_4, mutant, c(-2e08, -1e08), c(-2e08, -1e08))

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
  source('./scripts/Visualization_helpers.R')
  density2d_my_style(diff_den, quo(x), quo(y), quo(z))

  f1x <- den1$z[den1$x >= 0 & den1$x <= 2e08, den1$y > 0 & den1$y < 2e08]
  f2x <- den2$z[den2$x >= 0 & den2$x <= 2e08, den2$y > 0 & den2$y < 2e08]

  local_signif(f1x, f2x, H1, H2, n1, n2)

  # Simulate joint distributions see what the cut off is for significant findings

  dat <- tibble(x = c(rnorm(500, 0, 1), rnorm(500, 0.5, 1)),
                y = c(rnorm(500, 0, 1), rnorm(500, 0.5,1)),
                grp = c(rep(F, 500), rep(T, 500)))

  wrapper_local_sig(dat, x, y, grp, c(-0.25,0.25), c(-0.25,0.25))
}

