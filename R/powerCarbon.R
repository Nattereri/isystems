#' Returns CO2 emissions from power (kW)
#'

#' In 2022 (Bulb), these figures were 0.193 kg of CO2e per kWh of electricity
#' Department for Environment, Food & Rural Affairs (DEFRA) is 0.233 kgCO2e per
#' kWh of electricity consumed 2020
#'
#' @param kW eletrical energy (kWh)
#' @param factor conversion factor (kWh to kg) 2021 default
#'
#' @return carbon dioxide (kg)
#'
#' @examples
#' powerCarbon(40)
#' powerCarbon(35, factor = 0.233)
#'
#' @export
powerCarbon <- function (kW, factor = 0.193) {

  kW * factor

}
