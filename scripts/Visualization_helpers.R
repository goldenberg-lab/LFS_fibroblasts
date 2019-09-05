# Visualization functions. Written with roxygen2 formating for ease of
# transition to package in future if wanted. Also seems like as good a standard
# as any to document functions.

#' Remove outlier data points
#'
#' Rmv all points which are more than sd std away from the mean in any variable.
#'
#' @param dat tibble, assumed all columns are numeric
#' @param stds number, numeric vector of length 1
#'
#' @return A tibble with rows that had outlier measurements removed
#'
#' @export
rmv_outliers <- function(dat, stds){
  stopifnot(require(tidyverse))
  dat %>% dplyr::filter_all(all_vars(abs(. - mean(.)) < stds*sd(., na.rm = T)))
}

#' Sort rows in a set of two columns
#'
#' Sort values in each row for two columns in a tibble, col1 will have the lesser
#' of the values and col2 will get the greater.
#'
#' @param tib A tibble, that contains col1 and col2
#' @param col1 symbol, name of a column in tib which is of comparable type to
#'    col2
#' @param col2 symbol, name of a different column in tib which is of
#'  comparable type to col1
#'
#' @return A tibble with the values in the rows for col1 and col2 sorted.
#'
#' @export
sort2cols <- function(tib, col1, col2){
  stopifnot(require(tidyverse))
  col1 <- enquo(col1); col2 <- enquo(col2)

  tib %>% dplyr::mutate(first = if_else(!!col1 <= !!col2, !!col1, !!col2),
                        second = if_else(!!col1 > !!col2, !!col1, !!col2)) %>%
    dplyr::select(-!!col1, -!!col2) %>%
    dplyr::rename(!!col1 := first, !!col2 := second)

}

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
    # bug where the binwidth for the contour lines is so small compared to the
    # range of values, there are many many contour lines which is causing the
    # memory overload.
    scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0) +
    scale_colour_gradient2(low=scales::muted("red"), mid="white", high=scales::muted("blue"), midpoint=0) +
    guides(colour=FALSE)
}













