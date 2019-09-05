library(tidyverse)

base_dir <- '/data/Jaryd/R/LFS_fibroblasts/'
in_dir <- '2ddensity_diffs_test/'
out_dir <- 'density_difference_plots/'
plt_paths <- list.files(paste0(base_dir, in_dir))

for(f in plt_paths){
    load(paste0(base_dir, in_dir, f))
    pdf_name <- paste0(base_dir, out_dir, f, '.pdf')

    new_plt <- ggplot(plt$data, aes(x = !!syms(names(plt$data))[[1]],
				    y = !!syms(names(plt$data))[[2]],
				    fill = density, z = density)) +
        geom_tile() +
	scale_colour_gradient2(low=scales::muted("red"),
			       mid="white", high=scales::muted("blue"),
			       midpoint=0) +
        scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0)

    ggsave(pdf_name, plot = new_plt)
}
