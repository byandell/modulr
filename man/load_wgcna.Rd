% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_wgcna.R
\name{load_wgcna}
\alias{load_wgcna}
\title{Load WGCNA object}
\usage{
load_wgcna(
  moddir,
  modobj = "WGCNA_objects_ms10.Rdata",
  params = list(signType = "unsigned", power = 12, minSize = 4),
  listof = TRUE,
  annot = NULL
)
}
\arguments{
\item{moddir}{directory name containing module object}

\item{modobj}{name of module object, ending in `.Rdata` or `.RData`}

\item{params}{non-default parameters for WGCNA (see `wgcna_params`)}

\item{listof}{embed object as `listof_wgcnaModules` if `TRUE`}

\item{annot}{annotation file (ignored if `NULL`)}
}
\value{
list object that has all WGCNA components
}
\description{
Load WGCNA object
}
