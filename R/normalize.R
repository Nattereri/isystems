#' Normalize a numerical vector between 0-1
#'
#' @param Value numerical vector
#'
#' @return numerical vector with values between 0 and 1.
#'
#' @examples
#' lnormalize(c(1, 2, 3, 4, 5))
#'
#'
#'
#'
#' @export
normalize<-function(x){

  stopifnot("Vector must be numeric" =  is.numeric(x))

  # be careful, the denominator may be trivial!
  return((x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm + T)))

}
