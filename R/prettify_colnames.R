#'
#' Prettifies colnames
#'
#' This function prettifies colnames for quick plotting/tables
#' removes _ and makes space instead, makes titlecase and removes
#' an optional prefix.
#'
#'  @param df input dataframe
#'  @param prefix prefix to remove is set to remove processed 'pr_'
#'
#'  @return df
#'
#'
#'  @examples
#'  prettify_colnames(df, prefix = 'hej)
#'
#' @export
#'
prettify_colnames <-
  function(df, prefix = 'pr_'){

  df %>%
    dplyr::rename_with(., ~stringr::str_replace_all(., prefix, ' ')) %>%
    dplyr::rename_with(., ~stringr::str_replace_all(., '_', ' ')) %>%
    dplyr::rename_all(stringr::str_to_title)
}
