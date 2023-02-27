#' Returns CO2 emissions from power (kW)
#'
#' based on 527 tonnes of carbon dioxide per gigawatt hour (GWh) overall in 2021
#' Bulb has a figure 0f 0.193 kg per kWh
#'
#' @param kW eletrical energy (kWh)
#' @param factor conversion factor (kWh to kg) 2021 default
#'
#' @return carbon dioxide (kg)
#'
#' @examples
#' powerCarbon(40)
#' powerCarbon(35, factor = 0.193)
#'
#' @export
powerCarbon <- function (kW, factor = .527) {

  kW * factor

}
