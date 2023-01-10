#' Takes Character and returns if left censored
#'
#' @param Value character string with numbers
#'
#' @return logical
#'
#' @examples
#' leftCen("<100")
#'
#'
#'
#'
#' @export
leftCen <- function (Value) {

  stringr::str_detect(Value, "<")

}
