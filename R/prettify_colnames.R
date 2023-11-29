
prettify_colnames <- function(df, prefix = 'pr_'){
  df %>%
    rename_with(., ~str_replace_all(., prefix, ' ')) %>%
    rename_with(., ~str_replace_all(., '_', ' ')) %>%
    rename_all(stringr::str_to_title)
}


