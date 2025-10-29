
#' Print time between first and second function call
#'
#' @param plot is a ggplot object
#' @param path is where to save the plot
#' @param width width of plot
#' @param height height of plot
#' @param dpi pixel density
#' @return save plot and print path where plot saved
#' @export
temp_plot <- function(plot, path = '/projects/canderson2@xsede.org/zhang-lab/cite-seq/analysis-versions/version002/cd4/results/temp-plot.png', width = 6, height = 4, dpi = 300) {
  if (inherits(plot, 'ggplot')) {
    ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi, bg = 'white')
  } else {
    png(filename = path, width = width, height = height, units = 'in', res = dpi)
    print(plot)   # ensures the plot is drawn
    dev.off()
  }
  message('Saved temporary plot to: ', path)
  invisible(path)
}

