library(tidyverse)
library(cowplot)
library(NMF)
library(gridExtra)
library(ggforce)
source('./scripts/import_data.R')

# Preliminary Visualizations
varGtrZero <- function(x){(!is.numeric(x) || var(x, na.rm = T) > 0)}
dat <- import_data() %>% select_if(varGtrZero)

cormat <- cor(dat %>% select_if(is.numeric), use = "na.or.complete")
aheatmap(cormat, color = "-RdBu:100", breaks = 0,filename = './figures/Allcorrelation.pdf')
# A couple of variables look to have almost perfect correlation, check if that
# is exactly the case, can then remove one of the two for each such pair.
cormat %>% as_tibble(rownames = 'Var1') %>%
  gather(key = 'Var2', value = 'cor', -Var1) %>%
  filter(Var1 != Var2 & abs(cor) == 1)
# There are two pairs of variables with perfect correlation, cor(DNA_Area, NArea) == 1, and
# cor(Ab_Int_Nuc, NInt_2) == 1

# Look for batch effects across the plates
TBP <- dat %>% select(-Row, -Column, -S, -M, -starts_with('Focus'), -DNA_Area, -Ab_Int_Nuc)
measures <- syms(names(TBP)[!grepl(pattern = "Plate", x = names(TBP))])
plts <- lapply(X = measures, FUN = function(y, dat, x){
                                            x <- enquo(x); y <- enquo(y)
                                            smldat <- dat %>% select(!!x, !!y)
                                            ggplot(smldat, aes(x = !!x, y = !!y)) +
                                              geom_boxplot()}, TBP, Plate)
arrangedPlts <- marrangeGrob(plts, nrow=2, ncol=2)
ggsave('./figures/Across_Plates.pdf', arrangedPlts)

# Automatically plot a boxplot where the view is scaled to focus on the box and
# whiskers cutting off the outliers.
Boxplt_adjust_view <- function(y, dat, x){
  stopifnot(require(tidyverse))
  x <- enquo(x); y <- enquo(y)
  smldat <- dat %>% select(!!x, !!y) #x is always column 1 now.
  ylim1 <- boxplot.stats(smldat[[2]])$stats[c(1,5)]  %>%
    {. + c(-0.05, 0.05)*diff(.)}
  ggplot(smldat, aes(x = !!x, y = !!y)) +
    geom_boxplot() +
    coord_cartesian(ylim = ylim1)
}

TBP <- dat %>% select(-Row, -Column, -S, -M, -starts_with('Focus'), -DNA_Area, -Ab_Int_Nuc)
measures <- syms(names(TBP)[!grepl(pattern = "Plate", x = names(TBP))])
plts <- lapply(X = measures, FUN = Boxplt_adjust_view, TBP, Plate)

arrangedPlts <- marrangeGrob(plts, nrow=2, ncol=2)
ggsave("./figures/zoomed_acrossPlates_test.pdf", arrangedPlts)

# Compare the differences in the predictor variables separating by cell Line.
# Recall, each row is a different cell line, rows 2:3 are WT, rows 4:7 are mutant.

TBP <- dat %>% mutate(mutant = Row > 3)
measures <- syms(names(TBP)[!grepl(pattern = '(Plate)|(mutant)', x = names(TBP))])
plts <- lapply(X = measures, FUN = function(x, dat, y){
                                            x <- enquo(x);y <- enquo(y)
                                            smldat <- dat %>% select(!!x, !!y)
                                            ggplot(smldat, aes(x = !!x, colour = !!y)) +
                                              stat_density(geom='line')}, TBP, mutant)

arrangedPlts <- marrangeGrob(plts, nrow=2, ncol=2)
ggsave("./figures/mutant_distributions.pdf", arrangedPlts)

# Zoom in on the plot to see the bulk of the data better.
density_adj_view <- function(x, dat, grp, percentiles){
  stopifnot(require(tidyverse))
  x <- enquo(x); grp <- enquo(grp)
  smldat <- dat %>% select(!!x, !!grp)
  xlim <- smldat %>% select(!!x) %>%
    summarise_all(list(~quantile(., probs = 0.01),
                       ~quantile(., probs = 0.99)), na.rm = T) %>%
    unlist(.[1,]) %>% {. + c(-0.05, 0.05)*diff(.)}
  ggplot(smldat, aes(x = !!x, colour = !!grp)) +
    stat_density(geom = 'line') + coord_cartesian(xlim = xlim)
}

TBP <- dat %>% mutate(mutant = Row > 3)
measures <- syms(names(TBP)[!grepl(pattern = '(Plate)|(mutant)', x = names(TBP))])
plts <- lapply(X = measures, FUN = density_adj_view, TBP, mutant, c(0.01, 0.99))

arrangedPlts <- marrangeGrob(plts, nrow=2, ncol=2)
ggsave("./figures/mutant_distributions_zoomed.pdf", arrangedPlts)






# speed testing different row sorting methods.
test <- expand.grid(measures_char, measures_char, stringsAsFactors = F)

sort1 <- function(tib){
  tib %>% mutate(first = if_else(Var1 <= Var2, Var1, Var2), second = if_else(Var1 > Var2, Var1, Var2)) %>%
    select(Var1 = first, Var2 = second)
}
sort2 <- function(tib){
  t(apply(tib, 1, sort))
}
library(microbenchmark)
microbenchmark(sort1(test), sort2(test))
all_equal(sort1(test), sort2(test) %>% as_tibble() %>% rename(Var1 = V1, Var2 = V2))

test2 <- rnorm(100) %>% expand.grid(.,.)
microbenchmark(sort1(test2), sort2(test2))




