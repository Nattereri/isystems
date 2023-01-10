#' Takes a Latis name and makes a consistent parameter name
#'
#' Latis lab names only
#'
#' @param df data frame with LatisName column
#'
#' @return Parameter
#'
#' @examples
#' latis_df <- tibble(LatisName = c("Appearance", "ChloridG01", "Cu36", "Fe36" ))
#' nameParameter(latis_df)
#'
#' @export
nameParameter <- function(df) {

  df %>%
    mutate(Parameter = case_when(
      LatisName ==  "Appearance" ~ "Appearance",
      LatisName ==  "TVC22s" ~ "Total Viable Count",
      LatisName ==  "Temperature" ~ "Temperature",
      LatisName ==  "Temp" ~ "Temperature",
      LatisName ==  "Molybdat13" ~ "Molybdate",
      LatisName ==  "pH Kap" ~ "pH",
      LatisName ==  "EC @ 20°C" ~ "Conductivity",
      LatisName ==  "Refrac Gly" ~ "Glycol",
      LatisName ==  "Fe Total" ~ "Total Iron",
      LatisName ==  "Fe36" ~ "Total Iron",
      LatisName ==  "Cu Total" ~ "Total Copper",
      LatisName ==  "Cu36" ~ "Total Copper",
      LatisName ==  "Pseuds" ~ "Pseudomonas Species",
      LatisName ==  "Ps.sp/100m" ~ "Pseudomonas Species",
      LatisName ==  "Na Total" ~ "Total Sodium",
      LatisName ==  "Nitrite as" ~ "Nitrite",
      LatisName ==  "NitriteG03" ~ "Nitrite",
      LatisName ==  "Chloride" ~ "Chloride",
      LatisName ==  "ChloridG01" ~ "Chloride",
      LatisName ==  "Sulphate" ~ "Sulphate",
      LatisName ==  "SulphatG01" ~ "Sulphate",
      LatisName ==  "TDS 105"  ~"TDS",
      LatisName ==  "TDS 100" ~"TDS"))
}
