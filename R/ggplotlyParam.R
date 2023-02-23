#' Plot a timeline of closed system parameter option to make a plotly graph
#'
#' Plot a timeline of closed system parameter option to make a plotly graph
#' and option to make a control value graph
#'
#'
#'
#' @param df Data frame with Date, SampleID, value and Parameter column.
#' @param site String site name blank by default
#' @param plot_system  String. System name blank by default.
#' @param plot_parameter String. parameter name
#' @param gtitle String, graph title
#' @param data_from Integer start Year of data
#' @param show_controlvalues Logical. Display control limits. FALSE by default.
#' @param linear_trend Logical. Display linear trend
#' @param loess_trend Logical. Display loess trend
#' @param graph_facet String "portrait" or "landscape", portrait by default.
#' @param graph_wrap Logical. If TRUE graphs are faceted. TRUE by default.
#' @param show_caption Logical FALSE by default.
#' @param caption_text String caption text "" by default
#' @param date_scale Integer number of months/weeks between Date axis values
#' @param date_period String. e.g. "months" or "weeks"
#' @param date_form String. Date format string
#' @param note_label Logical FALSE by default.
#' @param gg_plotly Logical. Make plotly graph FALSE by default.
#' @param inhibitor String. Inhibitor strategy; "nitrite", "mixed"  by default.
#'
#' @import tidyverse
#' @import ggplot2
#' @import scales
#' @import lubridate
#' @import magrittr
#' @import plotly
#'
#' @return ggplot or ggplotly graph
#'
#' @examples
#' Date <- c("2022-10-3", "2022-11-7", "2022-12-5", "2023-01-3", "2023-02-7", "2023-03-5")
#' Date <- lubridate::ymd(Date)
#' Year <- lubridate::year(Date)
#' Parameter <- c("pH", "pH", "pH", "pH", "pH", "pH")
#' value <- c(7.5, 8.8, 9.0, 5.0, 6.5, 7.8)
#' system <- c("LPHW", "LPHW", "LPHW", "LPHW", "LPHW", "LPHW")
#' SampleID <- c("LPHW 1", "LPHW 1", "LPHW 1", "LPHW 1", "LPHW 1", "LPHW 1")
#' system_df <- tibble::tibble(Date, Year, Parameter, value, system, SampleID)
#' ggplotlyParam(system_df, site = "No 10", plot_parameter = "pH")
#'
#' ggplotlyParam(system_df, site = "No 10", plot_parameter = "pH", gg_plotly = T)
#'
#' Date <- c("2022-08-3", "2022-10-7", "2022-12-5", "2023-02-3", "2023-04-7")
#' Date <- lubridate::ymd(Date)
#' Year <- lubridate::year(Date)
#' Parameter <- c("Glycol", "Glycol", "Glycol", "Glycol", "Glycol")
#' value <- c(15, 20, 30, 29, 31)
#' system <- c("CHW", "CHW", "CHW", "CHW", "CHW")
#' SampleID <- c("CHW 1", "CHW 1", "CHW 1", "CHW 1", "CHW 1")
#' system_df <- tibble::tibble(Date, Year, Parameter, value, system, SampleID)
#' ggplotlyParam(system_df,
#'               site = "No 10",
#'               plot_system = "CHW",
#'               plot_parameter = "Glycol",
#'               show_controlvalues = T)
#'
#' @export
ggplotlyParam <- function(df,
                          site = "",
                          plot_system = "LPHW",
                          plot_parameter = "pH",
                          gtitle = "",
                          data_from = 2010,
                          show_controlvalues = F,
                          linear_trend = F,
                          loess_trend = T,
                          graph_facet = "portrait",
                          graph_wrap = T,
                          show_caption = F,
                          caption_text = "",
                          date_scale = 3,
                          date_period = "months",
                          date_form = "%b %y",
                          note_label = F,
                          gg_plotly = F,
                          inhibitor = "mixed") {

  # check data is data.frame
  stopifnot("df_with_date must be data frame" =  is.data.frame(df))

  #check column names have DateTime column
  system_df_col_names <- colnames(df)
  stopifnot("Date column must in the data frame" =  ("Date" %in% system_df_col_names))

  #check Date column
  stopifnot("Input must have Date column" =  lubridate::is.Date(df$Date))

  date_scale <- glue("{date_scale} {date_period}")

  g_data <- df %>%
    filter(Year >= data_from) %>%
    filter(system == plot_system, !is.na(value), Parameter == plot_parameter)

  if (nrow(g_data) > 0) {
    S_date <- min(g_data$Date)
    E_date <- max(g_data$Date)

    if (plot_parameter == "Pseudomonas Species" | plot_parameter == "Total Viable Count") {
      g_data$value <- log10(1 + g_data$value)
    }

    p <- ggplot(g_data, aes(x = Date, y = value))

    if (show_controlvalues) {

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
        filter(Parameter == plot_parameter)

      C_value1 = c_val$c1[1]
      C_value2 = c_val$c2[1]

      # c_v <- c_values(plot_parameter)
      # C_value1 = c_v$C_value1[1]
      # C_value2 = c_v$C_value2[1]

      if (plot_parameter == "pH" |
          plot_parameter == "Molybdate" |
          plot_parameter == "Inhibitor" |
          plot_parameter == "Nitrite") {
        p <- p + # Control values
          annotate("rect",
                   xmin = S_date, xmax = E_date,
                   ymin = C_value1, ymax = C_value2, alpha = .3, fill = "darkgreen"
          ) +
          annotate("rect",
                   xmin = S_date, xmax = E_date,
                   ymin = 0, ymax = C_value1, alpha = .3, fill = "red"
          ) +
          annotate("rect",
                   xmin = S_date, xmax = E_date,
                   ymin = C_value2, ymax = +Inf, alpha = .3, fill = "gold"
          )
      } else if (plot_parameter == "Glycol") {

        p <- p + # Control values
          annotate("rect",
                   xmin = S_date, xmax = E_date,
                   ymin = 0, ymax = C_value1, alpha = .3, fill = "red"
          ) +
          annotate("rect",
                   xmin = S_date, xmax = E_date,
                   ymin = C_value1, ymax = +Inf, alpha = .3, fill = "darkgreen")


      } else {

        # Control value
        #p <- p + geom_hline(yintercept = C_value1, colour = "darkred", linetype = "dashed")

        p <- p + # Control values
          annotate("rect",
            xmin = S_date, xmax = E_date,
            ymin = 0, ymax = C_value1, alpha = .3, fill = "darkgreen"
          ) +
          annotate("rect",
            xmin = S_date, xmax = E_date,
            ymin = C_value1, ymax = +Inf, alpha = .3, fill = "red")

      }
    }

    p <- p + geom_point(
      size = 3,
      colour = "black",
      fill = paramColour(plot_parameter),
      alpha = 0.7,
      shape = 21
    )


    if (note_label) {
      p <- p + geom_label_repel(
        data = g_data, aes(label = Notes),
        max.overlaps = 20,
        #nudge_y = -0.25,
        #nudge_x = ifelse(graph_data$n < 100, 0.33, -0.33),
        alpha = 0.7)
    }


    if (linear_trend) {
      p <- p + geom_smooth(method = "lm", colour = "darkred")
    }

    if (loess_trend) {
      p <- p + geom_line(
        stat = "smooth",
        method = "loess",
        color = "blue",
        formula = y ~ x,
        linewidth = 1,
        alpha = 0.5
      )
    }

    p <- p + scale_x_date(
      breaks = date_breaks(date_scale),
      labels = date_format("%b %y"),
      limits = c(S_date, E_date)
    )

    if (plot_parameter == "Pseudomonas Species" | plot_parameter == "TVC22") {
      p <- p + scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
        annotation_logticks(sides = "rl")

      ano_y_min <- 0
      ano_y_max <- max(g_data$value, na.rm = T) + 2
    } else {
      p <- p + scale_y_continuous()

      ano_y_min <- -Inf
      ano_y_max <- +Inf
    }



    if (graph_facet == "landscape") {
      if (graph_wrap) {
        p <- p + facet_wrap(~SampleID)
      } else {
        p <- p + facet_grid(SampleID ~ .)
      }
    } else if (graph_facet == "portrait") {
      if (graph_wrap) {
        p <- p + facet_wrap(~SampleID)
      } else {
        p <- p + facet_grid(. ~ SampleID)
      }
    }

    if (show_caption) {
      c_txt <- caption_text
    } else {
      c_txt <- ""
    }



    # facet_wrap(~Parameter, scales = "free") +
    if(gtitle == ""){
      gtitle <- glue("{site} - {plot_system}: TimeLine of {altText(plot_parameter)}")
    } else {
      gtitle <- gtitle
    }

    p <- p + labs(
      title = gtitle,
      y = glue("{altText(plot_parameter)} {paramUnits(plot_parameter)}"),
      caption = c_txt
    ) +
    theme_bw() +
    theme (legend.position = "none",
           axis.text.x = element_text(size=10, face="bold", angle = 270),
           axis.text.y = element_text(size=10, face="bold"),
           axis.title.x = element_blank(),
           axis.title.y = element_markdown(size=10),
           plot.title = element_text(size=14, face="bold"),
           panel.grid.minor = element_blank(),
           panel.grid.major.y = element_line(colour = "grey80", linetype = "dotted"),
           panel.grid.minor.y = element_blank(),
           panel.grid.major.x = element_line(colour = "grey80", linetype = "dotted"),
           plot.caption = element_markdown(),
           strip.text = element_text(face="bold", colour = "white"),
           strip.background = element_rect(fill ="black"))


  } else {

    df <- data.frame(
      x = c(1, 1, 2, 2, 1.5),
      y = c(1, 2, 1, 2, 1.5),
      text = c("", "", "", "", glue("No {site} - {plot_system} - {plot_parameter} Data"))
    )

    p <- ggplot(df, aes(x, y)) +
      geom_text(aes(label = text)) +
      theme_bw() +
      theme (#text = element_text(family = font),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=14),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80", linetype = "dotted", size = 0.1),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80", linetype = "dotted", size = 0.1),
        plot.caption = element_text(size=8, face="italic"),
        strip.text = element_text(size=6, colour = "white"),
        strip.background = element_rect(fill = "black"))
  }

  if (gg_plotly) {
    ggplotly(p)
  } else {
    p
  }
}
