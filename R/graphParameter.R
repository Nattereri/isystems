#' Takes a Latis name and makes a consistent parameter name
#'
#' Latis lab names only
#'
#' @param df Data frame with Date, SampleID, Value and Parameter column.
#' @param g_system String. System name blank by default.
#' @param g_facet Logical. If TRUE graphs are faceted. TRUE by default.
#' @param p_size Numeric. Point size on graph.
#' @param p_alpha Numeric. Alpha of point.
#' @param c_alpha Numeric. Alpha of control value annotation.
#' @param loess_line Logical. Display loess curve. TRUE by default.
#' @param loess_conf Logical. Display loess confidence. FALSE by default.
#' @param loess_width Numeric. Loess line width.
#' @param loess_alpha Numeric. Alpha of loess line.
#' @param control_limits Logical. Display control limits. TRUE by default.
#' @param inhibitor String. Inhibitor stratergy; "nitrite", "mixed".
#'
#' @import tidyverse
#' @import ggplot2
#' @import lubridate
#' @import magrittr
#' @import ggtext
#'
#' @return ggplot graph
#'
#' @examples
#' Date <- c("2022-10-3", "2022-11-7", "2022-12-5", "2022-10-3", "2022-11-7", "2022-12-5")
#' Date <- lubridate::ymd(Date)
#' Parameter <- c("pH", "pH", "pH", "pH", "pH", "pH")
#' value <- c(7.5, 8.8, 9.0, 5.0, 6.5, 7.8)
#' SampleID <- c("CHW 1", "CHW 1", "CHW 1", "CHW 2", "CHW 2", "CHW 2")
#' system_df <- tibble::tibble(Date, Parameter, value, SampleID)
#' graphParameter(system_df, g_param = "pH", g_facet = T)
#'
#'
#' Date <- c("2022-07-3", "2022-08-7", "2022-09-5", "2022-10-3", "2022-11-7", "2022-12-5")
#' Date <- lubridate::ymd(Date)
#' Parameter <- c("Total Iron", "Total Iron", "Total Iron", "Total Iron", "Total Iron", "Total Iron")
#' value <- c(2.5, 2.8, 3.5, 4.3, 6.5, 10)
#' SampleID <- c("CHW 1", "CHW 1", "CHW 1", "CHW 1", "CHW 1", "CHW 1")
#' system_df <- tibble::tibble(Date, Parameter, value, SampleID)
#' graphParameter(system_df, g_param = "Total Iron", g_facet = T)
#'
#'
#' Date <- c("2022-07-3", "2022-08-7", "2022-09-5", "2022-10-3", "2022-11-7", "2022-12-5")
#' Date <- lubridate::ymd(Date)
#' Parameter <- c("Total Viable Count", "Total Viable Count", "Total Viable Count", "Total Viable Count", "Total Viable Count", "Total Viable Count")
#' value <- c(100, 1000, 10000, 100000, 1000000, 10000000)
#' SampleID <- c("CHW 1", "CHW 1", "CHW 1", "CHW 1", "CHW 1", "CHW 1")
#' system_df <- tibble::tibble(Date, Parameter, value, SampleID)
#' graphParameter(system_df, g_param = "Total Viable Count")
#'
#' @export
graphParameter <- function(df,
                           g_param = "pH",
                           g_system = "",
                           g_facet = T,
                           p_size = 3,
                           p_alpha = 0.8,
                           c_alpha = 0.4,
                           loess_line = T,
                           loess_conf = F,
                           loess_width = 1.5,
                           loess_alpha = 0.8,
                           control_limits = T,
                           inhibitor = "mixed") {

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


  # Get date range so graph covers all points (for use in control value plotting)
  S_date <-  min(graph_df$Date, na.rm = T)
  E_date <-  max(graph_df$Date, na.rm = T)

  numDays <- as.double(difftime(E_date,
                                S_date,
                                units = "days"))

  numDays <- as.integer(numDays * 0.1)

  S_date <- S_date - ddays(numDays)
  E_date <- E_date + ddays(numDays)

  if (g_param == "Pseudomonas Species" | g_param == "Total Viable Count"){
    #graph_df$value <- log10(1 + graph_df$value)
    graph_df$value <- 1 + graph_df$value
  }

  p <- ggplot(graph_df, aes(x = Date, y = value))

  caption_text <- ""
  if (control_limits){



    if(inhibitor == "mixed"){

      c_val <- cvalues %>%
        filter(Inhibitor != "nitrite") %>%
        filter(Inhibitor != "not mixed")

    }

    if(inhibitor == "nitrite"){

      c_val <- cvalues %>%
        filter(Inhibitor != "mixed")

    }


    c_val <-  c_val%>%
      filter(Parameter == g_param)

    if (!is.na(c_val$c1) & !is.na(c_val$c2)) {

      p <- p +  annotate("rect", xmin = S_date, xmax = E_date,
                         ymin =c_val$c1, ymax = c_val$c2, alpha = c_alpha,  fill = "darkgreen")

    }

    if (is.na(c_val$c2)) {

     ymin_graph_df <- min(graph_df$value, na.rm = T)

     ymin_graph_df <- ymin_graph_df - (ymin_graph_df * 0.1)

     if (g_param == "Pseudomonas Species" | g_param == "Total Viable Count"){

     } else {

       p <- p +  annotate("rect", xmin = S_date, xmax = E_date,
                          ymin =ymin_graph_df, ymax = c_val$c1, alpha = c_alpha,  fill = "darkgreen")

     }

    }

    caption_text <- c_val$Notes

  }

  p <- p + geom_point(shape = 21, fill = paramColour(g_param), size = p_size, alpha = p_alpha) +
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

    if (control_limits){

      p <- p +
        annotate("rect", xmin = S_date, xmax = E_date, ymin = 1, ymax = 10^2, alpha = .4, fill = "darkgreen") +
        annotate("rect", xmin = S_date, xmax = E_date, ymin = 10^2, ymax = 10^4, alpha = .4, fill = "darkseagreen1") +
        annotate("rect", xmin = S_date, xmax = E_date, ymin = 10^4, ymax = 10^6, alpha = .2, fill = "darkred") +
        annotate("rect", xmin = S_date, xmax = E_date, ymin = 10^6, ymax = Inf, alpha = .2, fill = "deeppink3")

    }

  } else {

    p <- p + scale_y_continuous()

  }

  p <- p +
    labs(title = glue("Time Line for {g_system} systems: {g_param}"),
                 y = glue("{g_param} {paramUnits(g_param)}"),
                 caption = glue("{caption_text}")) +
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
                      plot.caption = element_markdown(size=10, face="bold.italic"),
                      strip.text = element_text(size=10, face="bold", colour = "white"),
                      strip.background = element_rect(fill = "black"))

  p

}
