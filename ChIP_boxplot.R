
#Script used to generate H3K9me2 plot (identical to H3K4me3 script)
#importing ChIP data from text file - tidy format

chip <- read_delim("chip_tidy.txt", "\t", escape_double = FALSE, trim_ws = TRUE, col_types = readr::cols(
                                                                          Genotype = readr::col_character(),
                                                                          Enrichment = readr::col_double(),
                                                                          Region = readr::col_character()
                                                                        ))
#generating boxplot

plot_data_final <-
  ggplot2::ggplot(chip,
                  ggplot2::aes(x = factor(
                    Region,
                    levels = c(
                      "A",
                      "B",
                      "C",
                      "D",
                      "E",
                      "F",
                      "CAC3",
                      "TE"
                    )
                  ), y = Enrichment, fill = Genotype)) +
  ggplot2::geom_boxplot(position = 'dodge', outlier.shape = NA) + ggplot2::expand_limits(y = 1) +
  ggplot2::geom_point(shape=16, position=ggplot2::position_jitterdodge(0.1)) +
  ggplot2::labs(x = "Region", y = "% Input", title = "") +
  ggplot2::theme(
    title            = ggplot2::element_text(size = 24, face = "bold"),
    legend.title     = ggplot2::element_text(size = 24, face = "bold"),
    legend.text      = ggplot2::element_text(size = 24, face = "bold"),
    axis.title       = ggplot2::element_text(size = 24, face = "bold"),
    axis.text.y      = ggplot2::element_text(size = 20, face = "bold"),
    axis.text.x      = ggplot2::element_text(size = 24, face = "bold"),
    panel.background = ggplot2::element_blank(),
    strip.text.x     = ggplot2::element_text(
      size           = 24,
      colour         = "black",
      face           = "bold"
    )
  ) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ggplot2::scale_fill_manual(values = c("#00b159", "#ffc425")) 

#saving
cowplot::save_plot("ChIP_H3k9m2_final.png", plot_data_final, base_height = 6, base_width = 10)
