#' Add date information
#'
#' From the Date column creates useful date information (Month, MonthNr, MonthFull, Year, Week,
#' YearDay, MonthDay, WeekDay, WeekDayFull, Season); as extra columns in the data frame.
#'
#' Season:
#' Meteorological spring March 1 to May 31
#' Meteorological summer June 1 to August 31
#' Meteorological autumn September 1 to November 30
#' Meteorological winter December 1 to February 28
#'
#'
#' @param system_df bat data frame with Date column
#'
#' @return data.frame
#'
#' @examples
#' sys_data_frame <- tibble(Date = Sys.Date())
#' metaDate(sys_data_frame)
#'
#' @export
metaDate <- function(system_df) {

  # check data is data.frame
  stopifnot("input must be data frame/tibble" =  is.data.frame(system_df))

  #check column names have Date column
  system_df_col_names <- colnames(system_df)
  stopifnot("Date column name must be in the data frame" =  ("Date" %in% system_df_col_names))

  #check Date column
  stopifnot("Input must be a Date" =  lubridate::is.Date(system_df$Date))

  Season_levels <- c("Winter", "Spring", "Summer", "Autumn")

  SummerVector <- c("Jun", "Jul", "Aug" )
  AutumnVector <- c("Sep", "Oct", "Nov" )
  WinterVector <- c("Dec", "Jan", "Feb")
  SpringVector <- c("Mar", "Apr", "May" )


  system_df %>%
    dplyr::mutate(Month = lubridate::month(Date, label = T),
                  MonthNr = lubridate::month(Date),
                  MonthFull = lubridate::month(Date, label = T, abbr = F),
                  Year = lubridate::year(Date),
                  Week = lubridate::week(Date),
                  YearDay = lubridate::yday(Date),
                  MonthDay = lubridate::mday(Date),
                  WeekDay = lubridate::wday(Date, label = T),
                  WeekDayFull = lubridate::wday(Date, label = T, abbr = F),
                  Season = case_when(
                    Month %in% WinterVector ~ "Winter",
                    Month %in% SpringVector ~ "Spring",
                    Month %in% SummerVector ~ "Summer",
                    Month %in% AutumnVector ~ "Autumn"),
                  Season = factor(Season, levels = Season_levels))

}
