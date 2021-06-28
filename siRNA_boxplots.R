library(readr)

#import siRNA data from text file (tidy format) - this code was used to generate fig. 2
sRNAnrpe1 <- read_delim("polV.txt", "\t", escape_double = FALSE, trim_ws = TRUE, col_types = readr::cols(
                                                                          genotype = readr::col_character(),
                                                                          cpm = readr::col_double(),
                                                                          size = readr::col_character()
                                                                        ))
#boxplot 

plot_data_final <-
  ggplot2::ggplot(sRNAnrpe1,
                  ggplot2::aes(x = factor(
                    genotype,
                    levels = c(
                      "WTxsulf",
                      "nrpe1xsulf",
                      "sulf",
                      "nrpe1",
                      "WT"
                    )
                  ), y = cpm, fill = size)) +
  ggplot2::geom_boxplot(position = 'dodge', outlier.shape = NA) + ggplot2::expand_limits(y = 1) +
  ggplot2::geom_point(shape=16, position=ggplot2::position_jitterdodge(0.1)) +
  ggplot2::labs(x = "Genotype", y = "CPM", title = "") +
  ggplot2::theme(
    title            = ggplot2::element_text(size = 24, face = "bold"),
    legend.title     = ggplot2::element_text(size = 24, face = "bold"),
    legend.text      = ggplot2::element_text(size = 24, face = "bold"),
    axis.title       = ggplot2::element_text(size = 24, face = "bold"),
    axis.text.y      = ggplot2::element_text(size = 18, face = "bold"),
    axis.text.x      = ggplot2::element_text(size = 12, face = "bold"),
    panel.background = ggplot2::element_blank(),
    strip.text.x     = ggplot2::element_text(
      size           = 24,
      colour         = "black",
      face           = "bold"
    )
  ) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) 

plot_data_final


cowplot::save_plot("plot_data_finalpolV.pdf", plot_data_final, base_height = 6, base_width = 10)
