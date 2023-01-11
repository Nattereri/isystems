#' Takes a Latis name and makes a consistent parameter name
#'
#' Latis lab names only
#'
#' @param df data frame with Date, SampleID, Value and Parameter column
#'
#' @return ggplot graph
#'
#' @examples
#' Date <- c("2022-10-3", "2022-11-7", "2022-12-5", "2022-10-3", "2022-11-7", "2022-12-5")
#' Date <- lubridate::ymd(Date)
#' Parameter <- c("pH", "pH", "pH", "pH", "pH", "pH")
#' value <- c(7.5, 8.8, 9.0, 5.0, 6.5, 7.8)
#' SampleID <- c("CHW 1", "CHW 1", "CHW 1", "CHW 2", "CHW 2", "CHW 2")
#' system_df <- tibble(Date, Parameter, value, SampleID)
#' graphParameter(system_df, g_param = "pH", g_facet = T)
#'
#' @export
graphParameter <- function(df,
                           g_param = "pH",
                           g_facet = T) {

  graph_df <- df %>%
    filter(Parameter == g_param)

  p <- ggplot(graph_df, aes(x = Date, y = value)) +
    geom_point(shape = 21) +
    facet_wrap(.~SampleID) +
    theme_bw() +
    theme (legend.position = "none",
                      axis.text.x = element_text(size=10, face="bold", angle = 270),
                      axis.text.y = element_text(size=10, face="bold"),
                      axis.title.x = element_blank(),
                      axis.title.y = element_text(size=10),
                      plot.title = element_text(size=14, face="bold"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.y = element_line(colour = "grey80", linetype = "dotted"),
                      panel.grid.minor.y = element_blank(),
                      panel.grid.major.x = element_line(colour = "grey80", linetype = "dotted"),
                      plot.caption = element_text(),
                      strip.text = element_text(face="bold", colour = "white"),
                      strip.background = element_rect(fill = "black"))

  p

}
