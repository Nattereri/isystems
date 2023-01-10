#' Takes Character and returns if right censored
#'
#' @param Value character string with numbers
#'
#' @return logical
#'
#' @examples
#' rightCen(">100000")
#'
#'
#'
#'
#' @export
rightCen <- function (Value) {

  stringr::str_detect(Value, ">")

}
