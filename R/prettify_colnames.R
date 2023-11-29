
prettify_colnames <- function(df, prefix = 'pr_'){
  df %>%
    dplyr::rename_with(., ~stringr::str_replace_all(., prefix, ' ')) %>%
    dplyr::rename_with(., ~stringr::str_replace_all(., '_', ' ')) %>%
    dplyr::rename_all(stringr::str_to_title)
}


