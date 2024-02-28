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
paramUnits <- function(lookUpParameter) {
  unitsList <- c( # Look up table as named vector
    "Ammonia" = "(mg/l as NH3)",
    "Appearance" = "",
    "BOD" = "(mg/l)",
    "Calcium" = "(mg/l)",
    "Chloride" = "(mg/l)",
    "COD" = "(mg/l)",
    "Conductivity" = "(mS/m)",
    "Hardness" = "(mg/l)",
    "Iron" = "(mg/l)",
    "Magnesium" = "(mg/l)",
    "Manganese" = "(mg/l)",
    "Nitrate" = "(mg/l)",
    "Nitrite" = "(NO2-N mg/l)",
    "Nitrogen" = "(mg/l)",
    "pH" = "",
    "Phosphate" = "(mg/l)",
    "Phosphorus" = "(mg/l)",
    "Potassium" = "(mg/l)",
    "Sodium" = "(mg/l)",
    "Sulphate" = "(mg/l)",
    "TDS" = "(mg/l)",
    "Sulphate" = "(mg/l)",
    "Zinc" = "(mg/l)",
    "Temperature" = "(\u00B0C)",
    "Molybdate" = "(MoO4 mg/l)",
    "Glycol" = "(%v/v)",
    "Pseudomonas Species" = "(cfu/100ml)",
    "Pseudomonas aeruginosa" = "(cfu/100ml)",
    "Total Viable Count" = "(cfu/100ml @22\u00B0C)",
    "TVC22" = "(cfu/100ml @22\u00B0C)",
    "Total Viable Count 37" = "(cfu/100ml @37\u00B0C)",
    "TVC37" = "(cfu/100ml @37\u00B0C)",
    "Total Iron" = "(mg/l)",
    "Total Copper" = "(mg/l)",
    "Total Sodium" = "(mg/l)",
    "Total Aluminium" = "(mg/l)",
    "Total Zinc" = "(mg/l)",
    "Suspended Solids" = "(mg/l)",
    "SRB" = "present/not detected",
    "Inhibitor" = "(mg/l)",
    "Soluble Iron" = "(mg/l)",
    "Soluble Copper" = "(mg/l)",
    "Soluble Aluminium" = "(mg/l)",
    "Soluble Zinc" = "(mg/l)",
    "Ammoniacal Nitrogen" = "(mg/l as N)",
    "Phosphorous" = "(mg/l as P)",
    "NRB" = "(cfu/100ml)",
    "Turbidity" = "NTU"
  )

  unitsList[[lookUpParameter]]
}
