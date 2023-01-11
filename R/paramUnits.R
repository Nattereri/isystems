#' Parameter name and returns the units of measurement.
#'
#' @param lookUpParameter parameter for which units are required
#'
#' @return Units of measuremenmt as a string
#'
#' @examples
#' paramUnits("Conductivity")
#'
#' @export
paramUnits <- function (lookUpParameter) {

  unitsList <- c(#Look up table as named vector
    "Temperature" = "(\u00B0C)",
    "Molybdate"  = "(MoO4 mg/l)",
    "Glycol" = "(mg/l)",
    "pH" = "",
    "Pseudomonas Species" = "(cfu/100ml)",
    "Total Viable Count" = "(cfu/100ml)",
    "Conductivity" = "(mS/m)",
    "Chloride" = "(mg/l)",
    "Nitrate" = "(NO3-N mg/l)",
    "Total Iron" = "(mg/l)",
    "Total Copper" = "(mg/l)",
    "Total Sodium"  = "(mg/l)",
    "TDS"  = "(mg/l)",
    "Sulphate" = "(mg/l)")

  unitsList[[lookUpParameter]]

}
