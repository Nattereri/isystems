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
    "Ammoniacal Nitrogen" = "greenyellow",
    "Ammonia" = "greenyellow",
    "BOD" = "#EF2917",
    "Molybdate"  = "lightseagreen",
    "Glycol" = "slategrey",
    "pH" = "darkorange",
    "Pseudomonas Species" = "yellow3",
    "Conductivity" = "purple4",
    "Chloride" = "burlywood",
    "Nitrite" = "coral3",
    "Temperature" = "steelblue",
    "Soluble Iron" = "red",
    "Soluble Copper" = "darkorange4",
    "Soluble Aluminium"  = "powderblue",
    "Soluble Zinc"  = "cadetblue",
    "Total Viable Count" = "darkgoldenrod3",
    "Total Iron" =  "darkred",
    "Total Copper" = "seagreen",
    "Total Sodium"  = "honeydew4",
    "Total Aluminium"  = "azure3",
    "Total Viable Count 37" = "darkgoldenrod2",
    "Total Zinc"  = "azure2",
    "TDS"  = "mistyrose2",
    "TVC22" = "darkgoldenrod3",
    "TVC37" = "darkgoldenrod2",
    "Sulphate" = "dodgerblue",
    "Suspended Solids" = "cornsilk4",
    "SRB" = "grey35",
    "Inhibitor" = "moccasin",
    "Turbidity"  = "cadetblue",
    "Phosphorous" = "brown",
    "NRB" = "lightgreen",
    "Manganese" = "grey80" )

  unitsList[[lookUpParameter]]

}
