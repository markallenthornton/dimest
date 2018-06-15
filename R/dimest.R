#' Estimate dimensionality of dissimilarity matrix
#' 
#' @description Wrapper function for dimensionality estimation methods
#' @usage dimest(dmat,epsratio=NULL,mds.control=list(type="ordinal",itmax=1000))
#' @param dmat vectorized dissimilarity matrix, or m x n matrix of n vectorized dissimilarity matrices
#' epsratio convergence criterion. Leave default = NULL for method specific defaults: dimest.cv = .001, dimest.R2 = .1
#' mds.control list of smacofSym parameters: type, defaults to "ordinal"; itmax, defaults to 1000
#' @return ndim estimated number of dimensions
#' cv.cor for dimest.cv, vector of mean crossvalidated correlations between mds configurations and left out data
#' r2 for dimest.R2, R-squared of configuration with ndim dimensions
#' @details Wrapper function for dimest.cv and dimest.R2. Detects which method to use based on whether dmat argument is
#' a matrix, for dimest.cv, or a vector, for dimest.R2. The epsratio is set automatically to the default of whichever
#' method is chosen, but can be manually overridden. The smacof multidimensional scaling type defaults to ordinal for
#' dealing with ordinal human ratings or neural pattern dissimilarity matrices with global components.
#' @seealso \code{\link{smacofSym}} \code{\link{dimest.cv}} \code{\link{dimest.R2}}
#' @examples 
#' set.seed(1)
#' dat <- matrix(rnorm(200),100,2)
#' dmat <- as.vector(dist(dat))
#' dmat2 <- replicate(2,dmat+rnorm(4950))
#' dimR2fit <- dimest(dmat)
#' dimcvfit <- dimest(dmat2)
#' @export dimest
dimest <- function(dmat,epsratio=NULL,mds.control=list(type="ordinal",itmax=1000)){
  dmatdim <- dim(dmat)
  if(is.null(dmatdim)){
    if (is.null(epsratio)){
      espratio = .1
    }
    dimres <- dimest.R2(dmat,epsratio,mds.control)
  } else {
    if (is.null(epsratio)){
      espratio = .001
    }
    dimres <- dimest.cv(dmat,epsratio,mds.control)
  }
  return(dimres)
}
