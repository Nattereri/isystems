#' Takes Character and returns Number
#'
#' @param Value character string with numbers
#'
#' @return numeric value
#'
#' @examples
#' makeNum("<100")
#'
#'
#'
#'
#' @export
makeNum <- function (Value) {

Value <- stringr::str_replace(Value, "[^0-9.-]", "")
Value <- as.numeric(Value)

return(Value)

}
