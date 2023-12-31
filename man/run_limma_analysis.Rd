% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_limma.R
\name{run_limma_analysis}
\alias{run_limma_analysis}
\title{Run Limma Analysis}
\usage{
run_limma_analysis(df, phenotype_col, coefs = NULL)
}
\arguments{
\item{df}{long df}

\item{phenotype_col}{what to test}

\item{coefs}{additional df that is joined for phenotype (optional)}
}
\value{
data frame with results_
}
\description{
Convinece wrapper for running limma. Takes a long tibble
with assay, npx and d_aid. Additional can join a column
with coefs to join to the analysis
Needs a phenotype string for the analysis.
}
\examples{
check 100pats project..
}
