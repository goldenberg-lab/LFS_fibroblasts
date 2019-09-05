#' Calculate the difference in two 2d densities
#'
#' Calculates the 2d density using kde2d from package MASS and then subtracts
#' the density for the False group from the True group
#'
#' @param x quosure symbol of the x axis column for the joint density calculation
#' @param y quosure symbol of the y axis column for the joint density calculation
#' @param dat a tibble which contains columns x, y, and grp
#' @param grp quosure symbol of the logical column which indicates the groups
#'
#' @return returns a tibble with a grid of points and the difference in densities for those same points.
#'
#' @export
diffDensity2d <- function(x, y, dat, grp){
  # Load required packages
  stopifnot(require(MASS) & require(tidyverse) & require(rlang))
  # Check arguments are valid
  stopifnot(is_quosure(x) & is_quosure(y) & is_quosure(grp) & is_tibble(dat))# & quo_is_symbol(x) & quo_is_symbol(y))

  smldat <- dat %>% dplyr::select(!!x, !!y, !!grp) %>% ungroup() %>% rmv_outliers(5)

  if(length(smldat[[1]]) == 0){
    return("error in outlier removal, all observations removed.")
  }

  xlims <- range(dplyr::select(smldat, !!x)[[1]])
  ylims <- range(dplyr::select(smldat, !!y)[[1]])

  den1 <- smldat %>% dplyr::filter(!!grp == F) %>%
    {MASS::kde2d(.[[1]], .[[2]], n = 100, lims = c(xlims, ylims))}
  den2 <- smldat %>% dplyr::filter(!!grp == T) %>%
    {MASS::kde2d(.[[1]], .[[2]], n = 100, lims = c(xlims, ylims))}

  colnames(den1$z) <- den1$y
  den <- as_tibble(den1$z - den2$z)
  dendiff <- den %>% mutate(x = den1$x) %>% gather('y', 'density', -x) %>%
    mutate_all(as.numeric) %>% rename(!!x := x, !!y := y)
  dendiff
}
