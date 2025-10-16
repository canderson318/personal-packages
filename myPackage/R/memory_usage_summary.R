
#' Print dataframe of environment object sizes
#'
#' @return A dataframe of local environment objects and their sizes
#' @export
memory_usage_summary <- function() {
  require(dplyr)
  # Get names and sizes of all objects in the global environment
  obj_info <- sapply(ls(envir = .GlobalEnv), function(obj_name) {
    object.size(get(obj_name, envir = .GlobalEnv))
  })

  # Convert to data frame, sort by size in descending order
  obj_df <- data.frame(
    Object = names(obj_info),
    Size = obj_info
  ) %>%
    arrange(desc(Size)) %>%
    mutate(Size_MB = round(Size / (1024 ^ 2), 2),
           Size_GB = round(Size / 1e9, 3)) %>%
    filter(Size_MB>0)

  # Print the summary table
  print(obj_df, row.names = FALSE)
}

