#' Takes Character and returns if censored left or right
#'
#' @param Value character string with numbers
#'
#' @return logical
#'
#' @examples
#' censored("<100")
#' censored(">100")
#'
#'
#'
#'
#' @export
censored <- function (Value) {

  stringr::str_detect(Value, "<|>")

}
