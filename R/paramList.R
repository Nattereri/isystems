#' Returns a vector of parameters that can be graphed with graphParameter
#'
#' Returns a vector of parameters that can be graphed with graphParameter
#' function. Used to check if the parameter can be plotted.
#'
#' @return vector of parameters
#'
#' @examples
#' paramList()
#'
#' @export
paramList <- function() {

  c("Total Viable Count", "Temperature", "Molybdate", "pH", "Conductivity",
    "Glycol", "Total Iron", "Total Copper", "Pseudomonas Species", "Total Sodium",
    "Nitrite", "Chloride", "Sulphate", "TDS")
}
