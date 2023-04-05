#' Returns pump power (kW) for a head and discharge
#'
#' @param Hm pumping head (metres)
#' @param Q discharge volume (l/s)
#' @param eff pump efficency (percentage)
#'
#' @return numeric (kW)
#'
#' @examples
#' pumpPower(35, 0.06, eff= 0.55)
#'
#' @export
pumpPower <- function (Hm, Q, eff = 0.7) {

  stopifnot("Pump efficiency must be numeric" =  is.numeric(eff))
  stopifnot("Pump flow must be numeric" =  is.numeric(Q))
  stopifnot("Pump head must be numeric" =  is.numeric(Hm))

  (9.81 * Q * Hm) / eff

}
