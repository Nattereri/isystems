#' Returns factor levels for Month (abbrev)Year in order (4 or 2 digit years)
#'
#' @param fromYear integer Year
#' @param toYear integer Year
#' @param yr_type 2 or 4 digit years
#'
#' @return character vector
#'
#' @examples
#' factorYrMn(2000, 2023)
#'
#'
#' @export
factorYrMn <- function(fromYear, toYear, yr_type = 2) {

  fromYear <- as.integer(fromYear)
  toYear <- as.integer(toYear)

  stopifnot("From Year must be integer" =  is.integer(fromYear))
  stopifnot("To Year must be integer" =  is.integer(toYear))

  #Levels
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  years <- seq(fromYear, toYear, by = 1L)

  month_year <- NULL
  k <- 0

  for(i in 1:(length(years))) {

    for(j in 1:12) {

      k <- k + 1

      if (yr_type == 4) {
        month_year[k] <- glue("{months[j]} {years[i]}")
      } else if (yr_type == 2) {

        year2 <- stringr::str_sub(as.character(years[i]), start = 3L, end = 4L)

        month_year[k] <- glue("{months[j]} {year2}")
      }

    }

  }

  return(month_year)

}

