% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wgnca_ID.R
\name{wgcna_ID}
\alias{wgcna_ID}
\title{ID data frame from}
\usage{
wgcna_ID(object, condition_under = TRUE, annot = NULL)
}
\arguments{
\item{object}{data frame containing ID information}

\item{condition_under}{condition has embedded "_" if `TRUE`}

\item{annot}{annotation file (ignored if `NULL`)}
}
\value{
data frame with column `ID` and optional column `animal`
}
\description{
ID data frame from
}
