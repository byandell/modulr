#' Load WGCNA object
#'
#' @param moddir directory name containing module object
#' @param modobj name of module object, ending in `.Rdata` or `.RData`
#' @param params non-default parameters for WGCNA (see `wgcna_params`)
#' @param listof embed object as `listof_wgcnaModules` if `TRUE`
#' @param annot annotation file (ignored if `NULL`)
#'
#' @return list object that has all WGCNA components
#' @export
#'
load_wgcna <- function(moddir, modobj = "WGCNA_objects_ms10.Rdata",
                       params = list(signType = "unsigned",
                                     power = 12, 
                                     minSize = 4),
                       listof = TRUE,
                       annot = NULL) {
  
  dropME <- function(x) {
    names(x) <- stringr::str_remove(names(x), "^ME")
    x
  }
  # local() keeps loaded objects local.
  out <- local({
    load(file.path(moddir, modobj))
    # operations to create object, which is returned.
    
    # Add cutHeight to parameters.
    params$cutHeight <- merge$cutHeight
    
    list(
      ID = wgcna_ID(merge$newMEs, annot = annot),
      dendro = merge$dendro,
      eigen = dropME(merge$newMEs),
      modules = module_factors(kMEs, merge$colors),
      params = wgcna_params(params))
  })
  
  # Put harmonized rownames in place.
  if(is.null(annot)) {
    rownames(out$eigen) <- tidyr::unite(out$ID, ID, ID, animal)$ID
  } else {
    rownames(out$eigen) <- tidyr::unite(out$ID, ID, strain, sex, diet, animal)$ID
  }
  
  class(out) <- c("wgcnaModules", class(out))
  attr(out, "params") <- out$params
  
  if(listof) {
    out <- list(value = out)
    class(out) <- c("listof_wgcnaModules", class(out))
    attr(out, "params") <- attr(out[[1]], "params")
  }
  out
}
