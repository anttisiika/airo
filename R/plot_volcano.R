#' Plot volcano
#'
#' Takes a df which is output from limma (should have cols for fold_change and p_value,
#' and label)).
#'
#' @param dataframe results form limma (or equivalent with col for fc and p_value/adj_p_value and label)
#' @param adjusted adjusted or unadjusted p values
#' @param nudge_y if labels should be nudged.
#' @param legend.position where the legend should be
#'
#' @import ggplot2
#' @return ggplot2 plot/object
#' @export
#'
#' @examples volcanoplot(limma_results)
volcano_plot <-
  function(dataframe, fold_change, pvalue, label, adjusted = TRUE, nudge_y = 0.5,
           legend.position = 'bottom_left') {

    # Legend justiction ------------------------------------------------------------
    if (legend.position == 'top_left') {
      legend.justification = c(0, 1)
      legend.position = c(0, 1)
      title.position = 'bottom'
    } else if (legend.position == 'bottom_right') {
      legend.justification = c(1, 0)
      legend.position = c(1, 0)
      title.position = 'top'
    } else if (legend.position == 'bottom_left') {
      legend.justification = c(0, 0)
      legend.position = c(0, 0)
      title.position = 'top'
    } else if (legend.position == 'top_right') {
      legend.justification = c(1, 1)
      legend.position = c(1, 1)
      title.position = 'bottom'
    } else {
      return(
        print('choose one of: top_left, bottom_right, bottom_left or top_right'))
    }

    # plot adjusted or unadjusted p values and change labels -----------------------
    if (!adjusted) {
      p <-
        ggplot(dataframe, aes({{ fold_change }}, -log10({{ p_value }}), fill = sign_direction_unadj)) +
        labs(y = bquote( ~ 'Unadjusted P-value' ~ (-Log[10])),
             x = bquote( ~ 'Fold-change' ~ (Log[2])))
    } else {
      p <-
        ggplot(dataframe, aes({{ fold_change }}, -log10({{ p_value }}), fill = sign_direction_adj)) +
        labs(y = bquote( ~ 'FDR-corrected P-value' ~ (-Log[10])),
             x = bquote( ~ 'Fold-change' ~ (Log[2])))
    }

    # make the rest of the plot ----------------------------------------------------
    p <- p + geom_hline(yintercept = -log10(0.05), lty = 2, alpha = 0.7, size = 0.6) +

      geom_point(aes(shape = is.na({{label}})), fill = NA, size = 3) +
      scale_shape_manual(values = c(21, NA))+
      geom_point(shape = 21) +
      guides(shape = 'none') +

      ggrepel::geom_text_repel(aes(label = {{label}}),
                               min.segment.length = 0,
                               box.padding = 0.5,
                               point.padding = 0.5,
                               nudge_y = nudge_y,
                               force = 0.5) +


      #scale_x_continuous(breaks = c(-3:3), limits = c(-3,3)) +
      # scale_y_continuous(breaks = c(0:8), limits = c(0,5))+
      scale_fill_manual(values = c('grey34', darj1[5], darj1[4])) +
      scale_color_manual(values = c('grey34', darj1[5], darj1[4])) +


      theme(line = element_blank()) +
      theme(panel.border = element_blank(), axis.line = element_line())+

      theme(
        legend.justification = legend.justification,
        legend.position = legend.position,
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      ) +
      #theme(text = element_text(family = "Roboto Condensed")) +
      guides(fill =
               guide_legend(
                 title = '', title.position = title.position,
                 override.aes = list(shape = 22, size = 5)),
             col = 'none')

    # return ---------------------------------------------------------------------

    return(p)
  }
