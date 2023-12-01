
#' Compact pipe to gt table
#' also makes font roboto..
#' @param data
#'
#' @return gt
#' @export
#'
#'
gt_compact_pipe <- function(data){
data %>%
gt::tab_options(table.font.names = "Roboto Condensed") %>%

  gt::tab_options(
    data_row.padding = gt::px(1),
    summary_row.padding = gt::px(1),
    grand_summary_row.padding = gt::px(1),
    footnotes.padding = gt::px(1),
    source_notes.padding = gt::px(1),
    row_group.padding = gt::px(1)
  )
}
