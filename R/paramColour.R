#' Returns colour of Parameter to show on the graph.
#'
#' @param lookUpParameter parameter for which colour is required
#'
#' @return Colour as a string
#'
#' @examples
#' paramColour("Conductivity")
#'
#' @export
paramColour <- function (lookUpParameter) {

  unitsList <- c(#Look up table as named vector
    "Temperature" = "steelblue",
    "Molybdate"  = "lightseagreen",
    "Glycol" = "slategrey",
    "pH" = "darkorange",
    "Pseudomonas Species" = "yellow3",
    "Total Viable Count" = "darkgoldenrod3",
    "Conductivity" = "purple4",
    "Chloride" = "burlywood",
    "Nitrite" = "coral3",
    "Total Iron" =  "darkred",
    "Total Copper" = "seagreen",
    "Total Sodium"  = "honeydew4",
    "TDS"  = "mistyrose2",
    "Sulphate" = "dodgerblue")

  unitsList[[lookUpParameter]]

}
