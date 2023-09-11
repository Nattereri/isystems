#' Returns alternate text for Parameter name.
#'
#' @param lookUpParameter parameter for which alternate text is required
#'
#' @return Alternate text as a string
#'
#' @examples
#' altText("TDS")
#'
#' @export
altText <- function (lookUpParameter) {

  Txt <- c(
    "Appearance",
    "Chloride" = "Chloride",
    "Conductivity" = "Conductivity",
    "Glycol" = "Glycol",
    "Molybdate" = "Molybdate",
    "Inhibitor" = "Inhibitor",
    "Nitrite" = "Nitrite",
    "pH" = "pH",
    "Pseudomonas Species" = "Pseudomonads Species",
    "Sulphate" = "Sulphate",
    "TDS" = "Total Dissolved Solids",
    "Temperature" = "Temperature",
    "Total Copper" = "Total Copper",
    "Soluble Copper" = "Soluble Copper",
    "Total Iron" = "Total Iron",
    "Soluble Iron" = "Soluble Iron",
    "Total Sodium" = "Total Sodium",
    "Total Viable Count" = "Total Viable Count @22\u00B0C",
    "Total Viable Count 37" = "Total Viable Count @37\u00B0C",
    "NRB" = "Nitrite Reducing Bacteria"
  )

  Txt[[lookUpParameter]]

}
