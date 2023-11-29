
#' Convience wrapper for actually running limma, not exported.
#'
#' @param exprs_values temp
#' @param design temp
#' @param genelist temp
#'
#' @return tibble
#'
run_limma <- function(exprs_values, design, genelist ){
  fit <- limma::lmFit(exprs_values, design)
  fit <- limma::eBayes(fit)
  limma::topTable(fit, coef=2, number=Inf, genelist = genelist) %>%
    as_tibble()
}

#' Run Limma Analysis
#'
#' Convinece wrapper for running limma. Takes a long tibble
#' with assay, npx and d_aid. Additional can join a column
#' with coefs to join to the analysis
#' Needs a phenotype string for the analysis.
#' @param df long df
#' @param phenotype_col what to test
#' @param coefs additional df that is joined for phenotype (optional)
#'
#' @return data frame with results_
#' @import dplyr
#' @export
#'
#' @examples check 100pats project..
run_limma_analysis <- function(df, phenotype_col, coefs = NULL) {
  # Filter out rows with NAs in the specified phenotype column
  filtered_data <- df %>%
    { if(!is.null(coefs)) left_join(., coefs, by = 'id') else . } %>%
    filter(complete.cases(select(., {{ phenotype_col }})))

  # Prepare mtx_form
  mtx_form <- filtered_data %>%
    select(d_aid, assay, npx) %>%
    pivot_wider(names_from = d_aid, values_from = npx)

  # Prepare phenotype data
  phenotype <- filtered_data %>%
    select(id, d_aid, all_of(phenotype_col)) %>%
    unique()

  # Prepare genelist
  genelist <- mtx_form[,1]

  # Prepare design matrix
  design_formula <- as.formula(paste("~", phenotype_col))
  design_matrix <- model.matrix(design_formula, data = phenotype)

  # Extract expression values
  exprs_values <- as.matrix(mtx_form[, -1])  # Assuming 1st column is sample_id

  # Run limma analysis
  limma_results <- run_limma(exprs_values, design_matrix, genelist)

  return(limma_results %>% mutate(test_design = !!(phenotype_col)))
}


