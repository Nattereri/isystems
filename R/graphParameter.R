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
                           g_facet = T,
                           p_size = 3,
                           p_alpha = 0.8,
                           loess_line = T,
                           loess_conf = F,
                           loess_width = 1.5,
                           loess_alpha = 0.8) {

  # check data is data.frame
  stopifnot("df_with_date must be data frame" =  is.data.frame(df))

  #check column names have DateTime column
  system_df_col_names <- colnames(df)
  stopifnot("DateTime column must be data frame" =  ("Date" %in% system_df_col_names))

  #check Date column
  stopifnot("Input must have Date column" =  lubridate::is.Date(df$Date))

  #check parameter can be graphed
  system_df_col_names <- colnames(df)
  stopifnot("Parameter is not in the graph list" =  (g_param %in% paramList()))

  graph_df <- df %>%
    filter(Parameter == g_param)

  if (g_param == "Pseudomonas Species" | g_param == "Total Viable Count"){
    graph_df$value <- log10(1 + graph_df$value)
  }

  p <- ggplot(graph_df, aes(x = Date, y = value)) +
    geom_point(shape = 21, fill = paramColour(g_param), size = p_size, alpha = p_alpha) +
    facet_wrap(.~SampleID)

  if (loess_line){
    p <- p + geom_smooth(se = loess_conf,
                         linetype = "dashed",
                         linewidth = loess_width,
                         alpha = loess_alpha,
                         colour = paramColour(g_param))
  }

  if (g_param == "Pseudomonas Species" | g_param == "Total Viable Count"){
    p <- p +  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                            labels = scales::trans_format("log10", scales::math_format(10^.x))) +
      annotation_logticks(sides = "rl")
  } else {

    p <- p + scale_y_continuous()

  }

  p <- p +
    labs(title = glue("TimeLine of {g_param}"),
                 y = glue("{g_param} {paramUnits(g_param)}"),
                 caption = "") +
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
