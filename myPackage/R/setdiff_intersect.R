
#' Print set diff and intersection in up to 3 sets
#'
#' @param sets a list
#' @param labels labels for each set, defaults to set list names
#' @param values logical for whether or not to how set values
#' @return print counts (and values) of distinctions and intersections
#' @export
  setdiff_intersect <- function(sets, labels = names(sets), values = FALSE) {
    require(dplyr)
    if (length(sets) < 2 || length(sets) > 3) {
      stop('Only 2 or 3 sets are supported.')
    }

    sets <- lapply(sets, unique) %>%
      lapply(na.omit)

    labels <- if (is.null(labels)) paste0('Set', seq_along(sets)) else labels

    if (length(sets) == 2) {
      a <- sets[[1]]
      b <- sets[[2]]
      la <- labels[1]
      lb <- labels[2]

      only_b <- setdiff(b, a)
      only_a <- setdiff(a, b)
      inter_ab <- intersect(a, b)

      cat(paste('Only', la, ':', length(only_a), '
'))
      if (values) cat(paste(only_a, collapse = ', '), '
')

      cat(paste('Only', lb, ':', length(only_b), '
'))
      if (values) cat(paste(only_b, collapse = ', '), '
')

      cat(paste(la, '∩', lb, ':', length(inter_ab), '
'))
      if (values) cat(paste(inter_ab, collapse = ', '), '
')

    } else if (length(sets) == 3) {
      a <- sets[[1]]
      b <- sets[[2]]
      c <- sets[[3]]
      la <- labels[1]
      lb <- labels[2]
      lc <- labels[3]

      list_out <- list(
        setdiff(a, union(b, c)),
        setdiff(b, union(a, c)),
        setdiff(c, union(a, b)),
        setdiff(intersect(a, b), c),
        setdiff(intersect(a, c), b),
        setdiff(intersect(b, c), a),
        intersect(intersect(a, b), c)
      )
      names(list_out) <- c( paste('Only', la),
                            paste('Only', lb),
                            paste('Only', lc),
                            paste(la, '∩', lb),
                            paste(la, '∩', lc),
                            paste(lb, '∩', lc),
                            paste(la, '∩', lb, '∩', lc)
      )

      for (label in names(list_out)) {
        vals <- list_out[[label]]
        cat(paste0(label, ': ', length(vals), '
'))
        if (values) cat(paste(vals, collapse = ', '), '

')
      }
    }
  }

