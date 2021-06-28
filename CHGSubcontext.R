# install.packages(c("readxl", "ggsci", "cowplot", "readr", "tidyr"))

options(tibble.print_max = Inf)
data_all <- dplyr::bind_rows(lapply(file.path("data", list.files("data")), readr::read_csv))[-1]

# Combine and parse First 30 files
data_subcontext <- tibble::as_tibble(do.call("rbind", lapply(stringr::str_split(list.files("data")[1:30], "_"), unlist)))
data_subcontext <- dplyr::mutate(data_subcontext, V4 = unlist(lapply(stringr::str_split(unlist(data_subcontext[ , 4]), "[.]"), function(x) return(x[1]))))
data_subcontext <- dplyr::mutate(data_subcontext, regions = paste0(V2, "_", V3))
data_subcontext <- dplyr::select(data_subcontext, -V2,-V3)
names(data_subcontext) <- c("name", "subcontext", "region")

# Combine and parse Last 30 - 36 files
data_subcontext_31_36 <- tibble::as_tibble(do.call("rbind", lapply(stringr::str_split(list.files("data")[31:36], "_"), unlist)))
data_subcontext_31_36 <- dplyr::mutate(data_subcontext_31_36, V3 = unlist(lapply(stringr::str_split(unlist(data_subcontext_31_36[ , 3]), "[.]"), function(x) return(x[1]))))
data_subcontext_31_36 <- dplyr::select(data_subcontext_31_36, V1,V3,V2)

names(data_subcontext_31_36) <- c("name", "subcontext", "region")

data_subcontext_all <- dplyr::bind_rows(data_subcontext, data_subcontext_31_36)

data_DMRs <- dplyr::bind_cols(data_all, data_subcontext_all)


data_final <- dplyr::bind_rows(
  dplyr::tibble(
    Genotype = rep("WT", nrow(data_DMRs)),
    value = data_DMRs$WT,
    subcontext = data_DMRs$subcontext,
    region = data_DMRs$region
  ),
  dplyr::tibble(
    Genotype = rep("sulf", nrow(data_DMRs)),
    value = data_DMRs$sulf,
    subcontext = data_DMRs$subcontext,
    region = data_DMRs$region
  )
)

#Bar plot for final figure

readr::write_tsv(data_final, "WT_sulf_CHG_methylation_subcontext.tsv", col_names = TRUE)


plot_data_final <-
  ggplot2::ggplot(dplyr::filter(data_final, !region %in% c("control_6",
                                                           "control_7",
                                                           "control_8",
                                                           "control_9")),
                  ggplot2::aes(x = subcontext, y = value * 100, fill = Genotype)) +
  ggplot2::geom_col(position = 'dodge') + ggplot2::expand_limits(y = 1) +
  ggplot2::labs(x = "Subcontext", y = "% Methylation", title = "") +
  ggplot2::theme(
    title            = ggplot2::element_text(size = 24, face = "bold"),
    legend.title     = ggplot2::element_text(size = 24, face = "bold"),
    legend.text      = ggplot2::element_text(size = 24, face = "bold"),
    axis.title       = ggplot2::element_text(size = 24, face = "bold"),
    axis.text.y      = ggplot2::element_text(size = 24, face = "bold"),
    axis.text.x      = ggplot2::element_text(size = 18, face = "bold"),
    panel.background = ggplot2::element_blank(),
    strip.text.x     = ggplot2::element_text(
      size           = 24,
      colour         = "black",
      face           = "bold"
    )
  ) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  ggplot2::scale_fill_manual(values = c("#ffc425", "#00b159")) +
  ggplot2::facet_grid(cols = ggplot2::vars(factor(
    region,
    levels = c(
      "DMR1",
      "Gene_body",
      "control_1",
      "control_2",
      "control_3",
      "control_4",
      "control_5"
    )
  )))

cowplot::save_plot("plot_data_final.png", plot_data_final, base_height = 8, base_width = 20)

