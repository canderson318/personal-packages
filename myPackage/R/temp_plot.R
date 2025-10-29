
#' Save plot to quick directory
#'
#' @param plot is a ggplot object
#' @param path is where to save the plot
#' @param width width of plot
#' @param height height of plot
#' @param dpi pixel density
#' @return save plot and print path where plot saved
#' @export
temp_plot <- function(plot,
                      path = '~/temp-plot.png',
                      width = 6, height = 4, dpi = 300) {
  require(ggplot2)

  if (inherits(plot, 'ggplot')) {
    ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi, bg = 'white')
  } else {
    png(filename = path, width = width, height = height, units = 'in', res = dpi)
    print(plot)
    dev.off()
  }
  message('Saved temporary plot to: ', path)
  invisible(path)
}

