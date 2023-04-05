#' Takes Character and returns Number
#'
#' @param Value character string with numbers
#'
#' @return numeric value
#'
#' @examples
#' makeNum("<100")
#' makeNum(" 5,811.0")
#'
#'
#'
#'
#' @export
makeNum <- function (Value) {

Value <- stringr::str_trim(Value, side = c("both"))
Value <- stringr::str_replace(Value, "[^0-9.-]", "")
Value <- as.numeric(Value)

return(Value)

}
