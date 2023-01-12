#' Takes Excel date (numbered from 1899-12-30) returns ISO date
#'
#' @param excelDate number
#'
#' @return Date
#'
#' @examples
#' dateExcel(42705)
#'
#'
#'
#'
#' @export
dateExcel <- function (excelDate) {

  # check data is data.frame
  stopifnot("Excel date must be numeric" =  is.numeric(excelDate))

  as.Date(excelDate, origin = "1899-12-30")

}
