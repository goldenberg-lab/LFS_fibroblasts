library(tidyverse)

# Randomize Mouse Fibroblast placement on plates for cell imaging experiment.
# Want to extend this to a general function for future ease of use.

assign_well_groups <- function(groups, num_columns, num_rows, out_file){
  #' Assign Well Groups
  #'
  #' Given a set of groups, and the number of members for each group place them in
  #' a table of size num_rows by num_colmns in such a way as to have as where
  #' the groups are evenly distributed over the columns, and randomly
  #' distributed across the rows.
  #'
  #' @param groups list. list of character vectors, each vector contains the
  #' labels for all members of a group.
  #' @param num_columns integer. Length one integer vector
  #' @param num_rows integer. Length one integer vector
  #' @param out_file character. Length one, the path of where to write a csv
  #' with the created assignment. If Null will not write to any file. If
  #' out_file is NULL then no file will be written.
  #'
  #' @return Data.frame which is the same as the one written to out_file
  #'
  stopifnot(is.list(groups))
  stopifnot(sum(sapply(groups, is.character))/length(groups) == 1)

  all_cols = rep(list(vector('character', length = num_rows)), num_columns)

  # Build sampled column vectors
  tmp_groups = vector('list', length=0)
  # Set the initial pool of groups to sample from
  if(length(tmp_groups) == 0){
    tmp_groups <- groups
    for(g in names(groups)){
      tmp_groups[[g]] <- sample(tmp_groups[[g]])
    }
  }
  for(i in 1:length(all_cols)){
    # Permute the order of groups in tmp_groups first, this serves the purpose
    # of shuffling the order in the column as well.
    tmp_groups <- sample(tmp_groups)
    for(j in 1:length(all_cols[[i]])){
      all_cols[[i]][j] <- tmp_groups[[(j %% length(tmp_groups)) + 1]][1]
      # Remove the added sample from the structure of all samples
      if(length(tmp_groups[[(j %% length(tmp_groups)) + 1]]) > 1){
        tmp_groups[[(j %% length(tmp_groups)) + 1]] <-
          tmp_groups[[(j %% length(tmp_groups)) + 1]] %>% .[2:length(.)]
      } else{
        tmp_groups <- tmp_groups[-((j %% length(tmp_groups)) + 1)]
        # Reset the pool of groups if the last group was removed.
        if(length(tmp_groups) == 0){
          tmp_groups <- groups
          for(g in names(groups)){
            tmp_groups[[g]] <- sample(tmp_groups[[g]])
          }
        }
      }
    }
    # Attempt to prevent strange pattern of groups landing in specific rows
    # frequently...
    all_cols[[i]] <- sample(all_cols[[i]])
  }

  df <- as.data.frame(all_cols)
  names(df) <- paste0('col', 1:length(df))
  if(!is.null(out_file)){
    write_csv(df, out_file)
  }

  df
}

# Input Arguments
#####
set.seed(6496492L)
groups <- list(c('FR89', 'FR35', 'FR45', 'FR23'),
               c('MR19', 'MR83', 'MR41', 'MR21'),
               c('MC36', 'MC51', 'MC16', 'MC68'),
               c('FC25', 'FC13', 'FC14'))

num_columns = 20
num_rows = 6

out_file = '/data/Jaryd/R/LFS_fibroblasts/Mice_plating_group_one.csv'

#####

set.seed(6496492L)
df <- assign_well_groups(groups=groups, num_columns=num_columns,
                         num_rows = num_rows, out_file=out_file)

#####
# Run KS test on samples to see if the groups are uniformly distributed.

# compare the count of rows with the uniform distribution, which would be
# perfectly distributed.

#Use package dgof which implements a discrete kolmogorov smirnov test.
library(dgof)

num_run <- 1000
lst_pvals <- vector('list', length = num_run)
counts <- vector('list', length=num_run)
True_stats <- vector('list', length = num_run)

for(i in 1:num_run){
  samp_df <- assign_well_groups(groups=groups, num_columns=20,
                                num_rows = num_rows, out_file=NULL)

  counts[[i]] <- samp_df %>% mutate(row = 1:6) %>%
    gather(-row, key="col", value = "group") %>%
    mutate(group = substr(group, 1,2)) %>%
    group_by(row) %>%
    count(group)

  lst_pvals[[i]] <- vector('numeric', length = length(unique(counts[[i]]$group)))
  names(lst_pvals[[i]]) <- unique(counts[[i]]$group)
  True_stats[[i]] <- vector('numeric', length = length(unique(counts[[i]]$group)))
  names(True_stats[[i]]) <- unique(counts[[i]]$group)
  for(g in unique(counts[[i]]$group)){
    #TODO: this test is not correct, the ecdf is testing a uniform distribution
    # over the values of 1:6. Need to come up with a distribution for what the
    # theoretical distribution should be.
    lst_pvals[[i]][g] <- ks.test(unlist(mapply(rep, 1:6, counts[[i]] %>%
                                                 filter(group == g) %>% .$n %>%
                                                 .[1:6] %>% replace_na(0))),
                        y = ecdf(1:6))$statistic

    True_stats[[i]][g] <- ks.test(sample(1:6,
                                 size=length(unlist(
                                   mapply(rep, 1:6, counts[[i]] %>%
                                            filter(group == g) %>% .$n %>%
                                            .[1:6] %>% replace_na(0)))),
                                 replace = T),
                          y = ecdf(1:6))$p.value
  }
  print(i)
}

t.test(unlist(lst_pvals), unlist(True_stats))



