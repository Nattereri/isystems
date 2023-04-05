#' Returns kWh needed to change the temperature of water
#'
#' @param m - the mass of water - tonnes
#' @param T_f - the final temperature
#' @param T_i - the initial temperature
#' @param Eff - Efficiency of the boiler
#'
#'
#' @return kWh for heater water from an initial to final temperature
#'
#' @examples
#' waterHeatingkWh(68, 15, 60, eff = 0.88)
#' waterHeatingkWh(3536, 55, 56, eff = 0.88)
#'
#' @export
waterHeatingkWh <- function (m, T_i, T_f, eff = 0.88) {

  # check data is numeric
  stopifnot("mass must be numeric" =  is.numeric(m))
  stopifnot("Initial temperature must be numeric" =  is.numeric(T_i))
  stopifnot("Final temperature must be numeric" =  is.numeric(T_f))
  stopifnot("Boiler efficiency must be numeric" =  is.numeric(eff))


  c <- 4184
  JkWh <- 2.7777778 * 10^(-7)

  Q_t = (c * m * (T_f - T_i) * 1000) / eff

  JkWh * Q_t

}
