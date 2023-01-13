#' Add date information
#'
#' Creates useful date information (Month, MonthFull, Year)
#' from the Date column; as extra columns in the data frame
#'
#' @param system_df bat data frame with Date column
#'
#' @return Month, MonthFull, Year
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

  system_df %>%
    dplyr::mutate(Month = lubridate::month(Date, label = T),
                  MonthNr = lubridate::month(Date),
                  MonthFull = lubridate::month(Date, label = T, abbr = F),
                  Year = lubridate::year(Date),
                  Week = lubridate::week(Date),
                  YearDay = lubridate::yday(Date),
                  MonthDay = lubridate::mday(Date),
                  WeekDay = lubridate::wday(Date, label = T),
                  WeekDayFull = lubridate::wday(Date, label = T, abbr = F))

}
