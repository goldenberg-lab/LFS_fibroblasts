library(optparse)
library(stringr)

# This script will take a given path, and randomly select a given percentage of
# the files in that location, and create copies in a new given location.\
#
# --SRC: A path which is the source folder to read all files in from.
# --PERC: A numeric value greater than zero and less or equal to 1.
# --DEST: A path which is the destination forlder for copies of the randomly
# selected files.
# --SEED: (optional) Integer used as the seed.
if( sys.nframe() == 0){
  option_list = list(
    make_option(c("--SRC"),
                type="character",
                help="Path to directory containing set of files to sample from",
                metavar="character"),
    make_option(c("--PERC"),
                type="double",
                help="How many of the original set should be copied",
                default=0.1,
                metavar="character"),
    make_option(c("--DEST"),
                type="character",
                help="Path to directory containing set of files to sample from",
                metavar="character"),
    make_option(c("--SEED"),
                type="double",
                help="Path to directory containing set of files to sample from",
                default=854829670,
                metavar="character"),
    make_option(c("--GRP"),
                type="character",
                help="Regex pattern which extracts part of the file name. All
                files which have equivalent regex groups will be sampled together.",
                metavar="character")
  )
  argv <- parse_args(OptionParser(option_list=option_list))
  # Example arguments below, use if running interactively.
  # argv <- list(SRC = "/media/carbondrive/Miriam\ Data/Zeiss\ Widefield/20180709_LFSplate1/",
  #            PERC = "0.1", DEST = "/data/Jaryd/R/LFS_fibroblasts/sample_images/Plate1",
  #            GRP = '.*?-([B-G][0-9]{1,2}).+(M[0-9]+)')
  print(argv)
  if (length(argv) < 3) {
    stop("A path to search in, percentage to copy, and path to copy to must be provided as arguments.")
  }

  if(!dir.exists(argv$SRC)){
    stop("source directory does not exist")
  }

  num_keep = as.numeric(argv$PERC)
  if(is.na(num_keep)){
    stop("second argument could not be converted to numeric")
  } else if(num_keep > 1 || num_keep <= 0){
    stop("second argument is not in (0, 1]")
  }

  if(!dir.exists(argv$DEST)){
    stopifnot(dir.create(argv$DEST, recursive = T))
  }

  if(!is.null(argv$SEED)){
    if(is.na(argv$SEED)){
      stop("fourth argument could not be converted to numeric to be used as seed")
    }
  }

  all_files <- list.files(argv$SRC, recursive = T, full.names = T)

  if(!is.null(argv$GRP)){
    fs <- unique(str_match(all_files, argv$GRP))
    # Takes advantage of the R environment structure to create a function with
    # memory, so that we can Reduce even though it's used with apply.
    fun <- function(x){
      x <- x
      function(y, sep){
        z <- paste(x, y, sep = sep)
        x <<- z
        z
      }
    }
    paste_wrap <- fun('')
    # This is sort of a Reduce over the columns now, except it stores all the
    # intermediary steps as well. which is less memory efficient, but it works for now.
    fs <- apply(fs[,2:ncol(fs)], MARGIN=2, paste_wrap, sep = '.*')
    fs <- unique(fs[,ncol(fs)])
  } else {
    fs <- all_files
  }

  subset <- sample(fs, floor(length(fs)*num_keep))

  if(!is.null(argv$GRP)){
    subset <- grep(paste(subset,collapse="|"), all_files, value=TRUE)
  }

  sapply(subset, function(f) file.copy(f, paste0(argv[3], '/', basename(f))))
}
