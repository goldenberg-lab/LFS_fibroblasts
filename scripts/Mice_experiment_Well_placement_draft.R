# Randomize Mouse Fibroblast placement on plates for cell imaging experiment.
# Want to extend this to a general function for future ease of use.


assign_well_groups <- function(group_names, group_sizes, num_columns, num_rows, out_file){
  #' Assign Well Groups
  #'
  #' Given a set of groups, and the number of members for each group place them in
  #' a table of size num_rows by num_colmns in such a way as to have as where
  #' the groups are evenly distributed over the columns, and randomly
  #' distributed across the rows.
  #'
  #' @param group_names character. The label for each group to
  #' @param group_sizes integer. Number of members for each group, must be the
  #' same length as group_names
  #' @param num_columns integer. Length one integer vector
  #' @param num_rows integer. Length one integer vector
  #' @param out_file character. Length one, the path of where to write a csv
  #' with the created assignment. If Null will not write to any file.
  #'
  #' @return Data.frame which is the same as the one written to out_file
  #'
  stopifnot(is.character(group_names))
  stopifnot(is.integer(group_sizes))
  stopifnot(length(group_names) == length(group_sizes))

  groups = vector('list', length = length(group_names))
  for(i in 1:length(group_names)){
    groups[[i]] <- vector('character', length = group_sizes[i])
    names(groups)[[i]] <- group_names[i]
    for(j in 1:length(groups[[i]])){
      groups[[i]][j] <- paste0(names(groups)[i], j)
    }
  }

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
    for(j in 1:length(all_cols[[i]])){
      all_cols[[i]][j] <- tmp_groups[[(j %% length(tmp_groups)) + 1]][1]
      # Remove the added sample from the structure of all samples
      if(length(tmp_groups[[(j %% length(tmp_groups)) + 1]]) > 1){
        tmp_groups[[(j %% length(tmp_groups)) + 1]] <- tmp_groups[[(j %% length(tmp_groups)) + 1]] %>% .[2:length(.)]
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
    # shuffle order of the column
    all_cols[[i]] <- sample(all_cols[[i]])
  }

  df <- as.data.frame(all_cols)
  names(df) <- paste0('col', 1:length(df))
  write_csv(df, out_file)

  df
}

# Input Arguments
#####

group_names <- c('MR', 'MC', 'FR', 'FC', 'ext')
group_sizes <- c(10L, 10L, 8L, 7L, 2L)
stopifnot(is.character(group_names))
stopifnot(is.integer(group_sizes))
stopifnot(length(group_names) == length(group_sizes))

num_columns = 20
num_rows = 6

out_file = '/data/Jaryd/R/LFS_fibroblasts/Mice_plating.csv'

#####
# 37 Mice, generate vector of all labels

# Build all base variables
groups = vector('list', length = length(group_names))
for(i in 1:length(group_names)){
  groups[[i]] <- vector('character', length = group_sizes[i])
  names(groups)[[i]] <- group_names[i]
  for(j in 1:length(groups[[i]])){
    groups[[i]][j] <- paste0(names(groups)[i], j)
  }
}

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
  for(j in 1:length(all_cols[[i]])){
    all_cols[[i]][j] <- tmp_groups[[(j %% length(tmp_groups)) + 1]][1]
    # Remove the added sample from the structure of all samples
    if(length(tmp_groups[[(j %% length(tmp_groups)) + 1]]) > 1){
      tmp_groups[[(j %% length(tmp_groups)) + 1]] <- tmp_groups[[(j %% length(tmp_groups)) + 1]] %>% .[2:length(.)]
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
  # shuffle order of the column
  all_cols[[i]] <- sample(all_cols[[i]])
}

table(unlist(all_cols))

df <- as.data.frame(all_cols)
names(df) <- paste0('col', 1:length(df))
write_csv(df, out_file)



