#' Returns concentration of pollutant downstream
#'
#' @param Fr - river flow upstream of the discharge
#' @param Cr - concentration of the pollutant in the river upstream of the discharge
#' @param fd - flow of the discharge
#' @param cd - concentration of the pollutant in the discharge
#'
#'
#' @return Concentration of the pollutant downstream of the discharge
#'
#' @examples
#' massBalance(25, 0.06, 2, 5.5)
#'
#' @export
massBalance <- function (Fr, Cr, fd, cd) {

  ((Fr * Cr) + (fd * cd)) / (Fr + fd)

}
