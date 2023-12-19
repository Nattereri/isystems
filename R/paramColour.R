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
paramColour <- function(lookUpParameter) {
  unitsList <- c( # Look up table as named vector
    "Ammoniacal Nitrogen" = "greenyellow",
    "Ammonia" = "greenyellow",
    "BOD" = "#EF2917",
    "Calcium" = "aquamarine2",
    "Chloride" = "burlywood",
    "COD" = "darkcyan",
    "Conductivity" = "purple4",
    "Hardness" = "darkslateblue",
    "Iron" = "darkred",
    "Magnesium" = "aliceblue",
    "Manganese" = "grey80",
    "Nitrate" = "steelblue",
    "Nitrite" = "coral3",
    "Nitrogen" = "blue",
    "pH" = "darkorange",
    "Phosphate" = "yellow2",
    "Phosphorous" = "brown",
    "Potassium" = "tomato",
    "Sodium" = "honeydew4",
    "Sulphate" = "dodgerblue",
    "TDS" = "mistyrose2",
    "Zinc" = "azure2",
    "Molybdate" = "lightseagreen",
    "Glycol" = "slategrey",
    "Pseudomonas Species" = "yellow3",
    "Temperature" = "steelblue",
    "Soluble Iron" = "red",
    "Soluble Copper" = "darkorange4",
    "Soluble Aluminium" = "powderblue",
    "Soluble Zinc" = "cadetblue",
    "Total Viable Count" = "darkgoldenrod3",
    "Total Iron" = "darkred",
    "Total Copper" = "seagreen",
    "Total Sodium" = "honeydew4",
    "Total Aluminium" = "azure3",
    "Total Viable Count 37" = "darkgoldenrod2",
    "Total Zinc" = "azure2",
    "TVC22" = "darkgoldenrod3",
    "TVC37" = "darkgoldenrod2",
    "Sulphate" = "dodgerblue",
    "Suspended Solids" = "cornsilk4",
    "SRB" = "grey35",
    "Inhibitor" = "moccasin",
    "Turbidity" = "cadetblue",
    "NRB" = "lightgreen"
  )

  unitsList[[lookUpParameter]]
}
