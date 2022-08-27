
#' District Shapefile - Pakistan
#'
#' @source <World Bank>
#' @format Data frame with columns - prepared and oriented
#' \describe{
#' \item{admin0Pcod}{Country Name.}
#' \item{admin1Pcod}{District Code.}
#' \item{admin1Name}{District Name.}
#' \item{geometry}{Geometry Coords.}
#' }
#'
#'
"pak_district"

################################################################################

#' Raw Data set
#' @source Author prepared based on data inputs by CCDR TEAM - World Bank
#' @format List
#' #' \describe{
#' \item{Country}{Pakistan.}
#' \item{admin1_District}{Data frame in wide format.}
#' \item{metadata}{Description of the variables.}
#' \item{context}{context of the variables - in case of the need to reverse color scheme}
#'}

"data"

################################################################################

#' Final Data set to be used in PCA algorithm
#' @source CCDR TEAM - World Bank
#' @format Wide data frame - tibble
#'

"data_pca"

################################################################################

#' District Names - Pakistan
#' @source Derived from Shape file
#' @format tibble
#'
"districts"

################################################################################

#' Metadata for the tool
#' @source Prpared in advance
#' @format tibble
#'
"legend"
