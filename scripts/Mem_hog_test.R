mem_hog <- function(){
    i <- TRUE
    j <- 1:2^30
    mat <- data.frame(j)
    while(i){
        mat <- cbind(j,j)
    }
}

tryCatch(mem_hog(), error = "caught error")
