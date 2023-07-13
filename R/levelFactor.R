#' Takes column and shows the levels when made a factor
#'
#' @param Value character string
#'
#' @return character vector
#'
#' @examples
#' exampleVector <- c("aa", "bb", "cc", "aa", "dd", "cc", "cc")
#' levelFactor(exampleVector)
#'
#'
#'
#' @export
levelFactor <- function (Value) {

  # check data is data.frame
  stopifnot("data must be a vector" =  is.vector(Value))

  # check data is data.frame
  stopifnot("data must be a character vector" =  is.character(Value))

  levels(factor(Value))

}
