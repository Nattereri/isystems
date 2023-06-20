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
    "TVC22" = "darkgoldenrod3",
    "Total Viable Count 37" = "darkgoldenrod2",
    "TVC37" = "darkgoldenrod2",
    "Conductivity" = "purple4",
    "Chloride" = "burlywood",
    "Nitrite" = "coral3",
    "Total Iron" =  "darkred",
    "Total Copper" = "seagreen",
    "Total Sodium"  = "honeydew4",
    "TDS"  = "mistyrose2",
    "Sulphate" = "dodgerblue",
    "Suspended Solids" = "cornsilk4",
    "SRB" = "grey35",
    "Inhibitor" = "moccasin",
    "Soluble Iron" = "red",
    "Soluble Copper" = "darkorange4",
    "Ammoniacal Nitrogen" = "greenyellow",
    "Phosphorous" = "brown")

  unitsList[[lookUpParameter]]

}
