




# make package step 1
if(FALSE){

  setwd("~/bin/personal-packages/")
  # Step 1: Create a new package
  package_path <- "myPackage"
  dir.create(package_path)

  # Create the package
  usethis::create_package(package_path)
}



# Load necessary libraries
pacman::p_load(usethis,devtools,roxygen2, dplyr, magrittr)

setwd("~/bin/personal-packages/myPackage")

rm(list = ls()); gc()

detach("package:myPackage", unload = TRUE)
remove.packages("myPackage")
unlink(file.path(.libPaths()[1], "myPackage"), recursive = TRUE)

# Step 2: Set up Git and GitHub (optional, uncomment if you want Git integration)
# usethis::use_git()
# usethis::use_github()

# Step 3: Add a license and README
# usethis::use_ccby_license()
# usethis::use_readme_md()  # Note, this will open an interactive window for editting


# Define a functions
memory_usage_summary <- "
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
"

notify.me <- "
#' Print message to notifications when called
#'
#' @param message is string to display in notif. banner
#' @return notification banner
#' @export
notify.me <- function(message = \"R completed\") {

  command <- paste0(
    \"terminal-notifier -message '\", message,
    \"' -title 'R Notification' -subtitle 'Completion Notice' \",
    \"-sound /System/Library/Sounds/Ping.aiff\"
  )

  system(command)
}
"

how.long <- "
#' Print time between first and second function call
#'
#' @param clear is logical arg to clear current time marker
#' @return time passed between calls
#' @export
how.long <- function(clear = FALSE){
  if(clear & ('how.long.time' %in% ls(envir = .GlobalEnv))){
    rm('how.long.time', envir = .GlobalEnv)
    how.long.time = Sys.time()
    return(assign('how.long.time', how.long.time, envir = .GlobalEnv))
  }else{
    if('how.long.time' %in% ls(envir = .GlobalEnv)){
      print(difftime(Sys.time() , how.long.time, units = 'auto' ))
      rm('how.long.time', envir = .GlobalEnv)
    }else{
      how.long.time = Sys.time()
      return(assign('how.long.time', how.long.time, envir = .GlobalEnv))
    }
  }
}
"

temp_plot <- "
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
"

functions <- list(
  memory_usage_summary=memory_usage_summary,
  notify.me=notify.me,
  how.long=how.long,
  temp_plot = temp_plot
  )


# Write the function to an R script file
for(func_nm in names(functions)){
  writeLines(functions[[func_nm]], con = sprintf("R/%s.R", func_nm))
}

# Step 4: Document the package (generate NAMESPACE and Rd files)
devtools::document()

# Step 5: Add dependencies (optional)
# usethis::use_package("dplyr")

# Step 6: Install the package
devtools::install()

# Step 7: Run a basic check on the package
devtools::check()

# Optional: Test the function after installation
library("myPackage")


# Step 8: Build the package (creates a tarball)
devtools::build()

# Step 9: Release the package on GitHub (optional, uncomment to use)
# usethis::use_github_release()
