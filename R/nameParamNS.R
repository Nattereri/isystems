#' Takes a North Site test name and makes a consistent parameter name
#'
#' Latis lab names only
#'
#' @param df data frame with test column
#'
#' @return Parameter
#'
#' @examples
#' NS_df <- tibble(test = c("Appearance", "Total Molybdenum", "Electrical Conductivity", "Total Sulphate"))
#' nameParamNS(NS_df)
#'
#' @export
nameParamNS <- function(df) {

  df %>%
    mutate(Parameter = case_when(
      test ==  "Appearance" ~ "Appearance",
      test ==  "Electrical Conductivity" ~ "Conductivity",
      test ==  "TVC22/72hr" ~ "Total Viable Count",
      test ==  "Total Alkalinity" ~ "Total Alkalinity",
      test ==  "Phenolphthalein Alkalinity" ~ "Phenolphthalein Alkalinity",
      test ==  "Total Sulphate" ~ "Sulphate",
      test ==  "Nitrite" ~ "Nitrite",
      test ==  "pH" ~ "pH",
      test ==  "Chloride" ~ "Chloride",
      test ==  "Total Dissolved Solids" ~ "Total Dissolved Solids",
      test ==  "Total Suspended Solids" ~ "Suspended Solids",
      test ==  "Total Hardness" ~ "Total Hardness",
      test ==  "Total Molybdenum" ~ "Molybdate",
      test ==  "Total Magnesium" ~ "Total Magnesium",
      test ==  "Total Calcium" ~ "Total Calcium",
      test ==  "Total Copper" ~ "Total Copper",
      test ==  "Total Iron" ~ "Total Iron",
      test ==  "Total Aluminium" ~ "Total Aluminium",
      test ==  "Soluble Copper" ~ "Soluble Copper",
      test ==  "Soluble Iron" ~ "Soluble Iron",
      test ==  "Odour" ~ "Odour"))
}
