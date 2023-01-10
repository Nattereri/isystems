#' Takes Character and returns Number
#'
#' @param Value character with numbers
#'
#' @return numeric value
#'
#' @examples
#' MakeNum("<100")
#'
#'
#'
#'
#' @export
MakeNum <- function (Value) {

Value <- stringr::str_replace(Value, "[^0-9.-]", "")
Value <- as.numeric(Value)

return(Value)

}
