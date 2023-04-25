#' Returns factor levels for Month (abbrev)Year in order (4 or 2 digit years)
#'
#' @param fromYear integer Year
#' @param toYear integer Year
#' @param abbr_yr logical TRUE 2 digit year FALSE 4 digit year
#' @param abbr_mn logical TRUE abbreviated month FALSE full month
#'
#' @return character vector
#'
#' @examples
#' factorYrMn(2000, 2023)
#' factorYrMn(2020, 2023, abbr_yr = F, abbr_mn = F)
#'
#'
#' @export
factorYrMn <- function(fromYear, toYear, abbr_yr = T, abbr_mn = T) {

  fromYear <- as.integer(fromYear)
  toYear <- as.integer(toYear)

  stopifnot("From Year must be integer" =  is.integer(fromYear))
  stopifnot("To Year must be integer" =  is.integer(toYear))

  #Levels
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  monthsfull <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

  years <- seq(fromYear, toYear, by = 1L)

  month_year <- NULL
  k <- 0

  for(i in 1:(length(years))) {

    for(j in 1:12) {

      k <- k + 1

      if (abbr_mn) {
        if (abbr_yr) {
          year2 <- stringr::str_sub(as.character(years[i]), start = 3L, end = 4L)
          month_year[k] <- glue("{months[j]} {year2}")
        } else {
          month_year[k] <- glue("{months[j]} {years[i]}")
        }
      } else {
        if (abbr_yr) {
          year2 <- stringr::str_sub(as.character(years[i]), start = 3L, end = 4L)
          month_year[k] <- glue("{monthsfull[j]} {year2}")
        } else {
          month_year[k] <- glue("{monthsfull[j]} {years[i]}")
        }
      }
    }
  }

  return(month_year)

}

