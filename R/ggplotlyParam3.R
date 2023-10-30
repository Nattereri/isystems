#' Plot a timeline of a location parameter option to make a plotly graph
#'
#' Plot a timeline of a location parameter option to make a plotly graph
#' and option to make a control value graph
#'
#'
#'
#' @param df Data frame with Date, SampleID, value and Parameter column.
#' @param site String site name blank by default
#' @param PlotLocation  String. Location name blank by default.
#' @param PlotParm String. parameter name
#' @param gtitle String, graph title
#' @param axsisTxt String, axsis title
#' @param figTxt String, fig title
#' @param ylim1 numeric lower limit y axsis
#' @param ylim2 numeric upper limit y axsis
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
#' @param Soluble Logical. If TRUE soluble values shown as a different shape.
#' @param Caption String, text for the graph caption
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
#' Location <- c("LPHW", "LPHW", "LPHW", "LPHW", "LPHW", "LPHW")
#' SampleID <- c("LPHW 1", "LPHW 1", "LPHW 1", "LPHW 1", "LPHW 1", "LPHW 1")
#' system_df <- tibble::tibble(Date, Year, Parameter, value, Location, SampleID)
#' ggplotlyParam3(system_df, site = "No 10", PlotParm = "pH")
#'
#' ggplotlyParam3(system_df, site = "No 10", PlotParm = "pH", gg_plotly = T)
#'
#' Date <- c("2022-08-3", "2022-10-7", "2022-12-5", "2023-02-3", "2023-04-7")
#' Date <- lubridate::ymd(Date)
#' Year <- lubridate::year(Date)
#' Parameter <- c("Glycol", "Glycol", "Glycol", "Glycol", "Glycol")
#' value <- c(15, 20, 30, 29, 31)
#' Location <- c("CHW", "CHW", "CHW", "CHW", "CHW")
#' SampleID <- c("CHW 1", "CHW 1", "CHW 1", "CHW 1", "CHW 1")
#' system_df <- tibble::tibble(Date, Year, Parameter, value, Location, SampleID)
#' ggplotlyParam3(system_df,
#'               site = "No 10",
#'               PlotLocation = "CHW",
#'               PlotParm = "Glycol",
#'               show_controlvalues = T)
#'
#' Date <- c("2022-08-3", "2022-10-7", "2022-12-5", "2023-02-3", "2023-04-7")
#' Date <- lubridate::ymd(Date)
#' Year <- lubridate::year(Date)
#' Parameter <- c("Pseudomonas Species", "Pseudomonas Species", "Pseudomonas Species", "Pseudomonas Species", "Pseudomonas Species")
#' value <- c(100, 2000, 50000, 1000000, 300000)
#' Location <- c("CHW", "CHW", "CHW", "CHW", "CHW")
#' SampleID <- c("CHW 1", "CHW 1", "CHW 1", "CHW 1", "CHW 1")
#' system_df <- tibble::tibble(Date, Year, Parameter, value, Location, SampleID)
#' ggplotlyParam3(system_df,
#'               site = "No 10",
#'               PlotLocation = "CHW",
#'               PlotParm = "Pseudomonas Species",
#'               show_controlvalues = F,
#'               loess_trend = F)
#'
#' @export
ggplotlyParam3 <- function(df,
                           site = "",
                           PlotLocation = "Polishing Pond",
                           PlotParm = "",
                           gtitle = "",
                           axsisTxt = "",
                           figTxt = "",
                           ylim1 = 0,
                           ylim2 = 600,
                           show_controlvalues = F,
                           linear_trend = F,
                           loess_trend = T,
                           loess_se = T,
                           graph_facet = "portrait",
                           graph_wrap = T,
                           show_caption = F,
                           caption_text = "",
                           date_scale = 4,
                           date_period = "months",
                           date_form = "%b %y",
                           note_label = F,
                           gg_plotly = F,
                           inhibitor = "mixed",
                           Soluble = F,
                           Caption = "") {

  date_scale <- glue("{date_scale} {date_period}")

  gtitle <- glue("{PlotLocation}: TimeLine of {figTxt}")

  g_data <- df %>%
    filter(Location == PlotLocation, !is.na(value), Parameter == PlotParm)


  S_date <- min(g_data$Date)
  E_date <- max(g_data$Date)

  if (PlotParm == "Pseudomonas Species"| PlotParm == "TVC22" | PlotParm == "TVC37" | PlotParm == "SRB" | PlotParm == "NRB"){

    #g_data$value <- log10(1 + g_data$value)

  }

  if (Soluble) {
    p <- ggplot(g_data) +
      geom_point(aes(x = Date, y = value, shape = Soluble), size = 5, fill = paramColour(PlotParm), alpha=0.7) +
      #geom_smooth() +
      scale_x_date(
        breaks = date_breaks(date_scale),
        labels = date_format("%b %y"),
        limits = c(S_date, E_date)
      )

  } else {
    p <- ggplot(g_data,  aes(x = Date, y = value)) +
      geom_point(size = 5, fill = paramColour(PlotParm), alpha=0.7, shape = 21) +
      #geom_smooth() +
      scale_x_date(
        breaks = date_breaks(date_scale),
        labels = date_format("%b %y"),
        limits = c(S_date, E_date)
      )
  }
  if (linear_trend) {
    p <- p + geom_smooth(method = "lm", colour = "darkred")
  }

  if (loess_trend) {
    p <- p + geom_line(data = g_data,
                       aes(x = Date, y = value),
                       stat = "smooth",
                       method = "loess",
                       color = "blue",
                       formula = y ~ x,
                       linewidth = 1,
                       alpha = 0.5
    )
  }



  if (PlotParm == "Pseudomonas Species" | PlotParm == "TVC22" | PlotParm == "TVC37"| PlotParm == "SRB" | PlotParm == "NRB"){


    p <- p + scale_y_continuous(
      trans = "log10",
      breaks = breaks_log(),
      labels = label_number(),
      limits = c(ylim1, ylim2)) +
      annotation_logticks(sides = "rl")

    ano_y_min <- 0
    ano_y_max <- max(g_data$value, na.rm = T) + 2

  } else {

    p <- p + scale_y_continuous(limits = c(ylim1, ylim2))

    ano_y_min <- -Inf
    ano_y_max <- +Inf

  }



  if (show_controlvalues) {

    gtitle <- glue("{PlotLocation}: {figTxt} Control Values ")

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
      filter(Parameter == PlotParm)

    C_value1 = c_val$c1[1]
    C_value2 = c_val$c2[1]

    # c_v <- c_values(plot_parameter)
    # C_value1 = c_v$C_value1[1]
    # C_value2 = c_v$C_value2[1]

    if (PlotParm == "pH" |
        PlotParm == "Molybdate" |
        PlotParm == "Inhibitor" |
        PlotParm == "Nitrite"   |
        PlotParm == "Sulphate") {
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
                 ymin = C_value2, ymax = +Inf, alpha = .3, fill = "red"
        )
    } else if (PlotParm == "Glycol") {

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

      if (PlotParm == "Pseudomonas Species" | PlotParm == "TVC22" | PlotParm == "TVC37"){

        p <- p +
          annotate("rect", xmin = S_date, xmax = E_date, ymin = 1, ymax = 10^2, alpha = .4, fill = "darkgreen") +
          annotate("rect", xmin = S_date, xmax = E_date, ymin = 10^2, ymax = 10^4, alpha = .4, fill = "darkseagreen1") +
          annotate("rect", xmin = S_date, xmax = E_date, ymin = 10^4, ymax = 10^6, alpha = .2, fill = "darkred") +
          annotate("rect", xmin = S_date, xmax = E_date, ymin = 10^6, ymax = Inf, alpha = .2, fill = "deeppink3")

      } else {

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
  }

  if (Soluble) {

    p <- p + scale_shape_manual(values = c(21, 24)) +
      labs(title = gtitle,
           y = axsisTxt,
           x = "Triangular points are soluble values",
           shape = "Soluble") +
      theme_bw() +
      theme (legend.position = "none",
             axis.text.x = element_text(size=10, face="bold", angle = 270),
             axis.text.y = element_text(size=10, face="bold"),
             axis.title.x = element_text(size=10),
             axis.title.y = element_text(size=10),
             plot.title = element_text(size=14, face="bold"),
             panel.grid.minor = element_blank(),
             panel.grid.major.y = element_line(colour = "grey80", linetype = "dotted"),
             panel.grid.minor.y = element_blank(),
             panel.grid.major.x = element_line(colour = "grey80", linetype = "dotted"),
             plot.caption = element_text(),
             strip.text = element_text(face="bold", colour = "white"),
             strip.background = element_rect(fill = "black"))

  } else {

    p <- p + labs(title = gtitle,
                  y = axsisTxt,
                  caption = Caption) +
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

  }

  if (gg_plotly) {
    ggplotly(p)
  } else {
    p
  }

}
