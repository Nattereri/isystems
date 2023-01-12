#' Converts a UNIX timestamp to an ISO date
#'
#' @param value number
#'
#' @return Date
#'
#' @examples
#' dateUnix(1669766400)
#'
#'
#'
#'
#' @export
dateUnix <- function (value) {

  # check data is data.frame
  stopifnot("UNIX Timestamp must be numeric" =  is.numeric(value))

  as.Date(as.POSIXct(value, origin="1970-01-01"))

}
