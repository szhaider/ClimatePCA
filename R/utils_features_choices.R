#' features_choices
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'
#'
#Making a grouped list of choices for 2 portfolios in shiny

data("data_pca")

mychoices <- list(
  `Survey Data` = c(names(data_pca[2:34])),
  `Geospatial Data` = c
  (names(data_pca[35:56]))
)


