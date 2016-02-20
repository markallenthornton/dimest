#' Estimate dimensionality by R-squared
#' 
#' @description Estimate dimensionality by change in multdimensional scaling R-squared 
#' @usage dimest.R2(dmat,epsratio=.1,mds.control=list(type="ordinal",itmax=1000))
#' @param dmat vectorized dissimilarity matrix
#' epsratio convergence ratio, defaults to .1
#' mds.control list of smacofSym parameters: type, defaults to "ordinal"; itmax, defaults to 1000
#' @return ndim estimated number of dimensions
#' r2 R-squared of configuration with ndim dimensions
#' @details Defines identity matrix of dimensions n x n. Reproduces functionality of identically named MATLAB function.
#' @seealso \code{\link{smacofSym}} \code{\link{dimest}} \code{\link{dimest.cv}}
#' @examples 
#' set.seed(1)
#' dat <- matrix(rnorm(200),100,2)
#' dmat <- as.vector(dist(dat))
#' dimR2fit <- dimest.R2(dmat)
dimest.R2 <- function(dmat,epsratio=.1,mds.control=list(type="ordinal",itmax=1000)){
  ndim <- 1
  converged = FALSE
  r2last <- 0
  while (!converged){
    mdfit <- smacof::smacofSym(squareform(dmat),ndim,type=mds.control$type,itmax=mds.control$itmax)
    r2 <- cor(mdfit$confdiss,dmat)^2
    if (ndim == 1){
      r2eps <- epsratio*r2
    }
    if (r2-r2last < r2eps){
      converged=TRUE
    } else {
      ndim <- ndim + 1
      r2last <- r2
    }
  }
  result <- list(r2=r2,ndim=ndim-1)
  return(result)
}