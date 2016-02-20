#' Estimate dimensionality by crossvalidation
#' 
#' @description Estimate dimensionality by change in crossvalidated multidimensional scaling fit
#' @usage dimest.cv(dmat,epsratio=.001,mds.control=list(type="ordinal",itmax=1000))
#' @param dmat m x n matrix of n vectorized dissimilarity matrices
#' epsratio convergence criterion. Defaults to .001
#' mds.control list of smacofSym parameters: type, defaults to "ordinal"; itmax, defaults to 1000
#' @return ndim estimated number of dimensions
#' cv.cor vector of mean crossvalidated correlations between mds configurations and left out data
#' @details Defines identity matrix of dimensions n x n. Reproduces functionality of identically named MATLAB function.
#' @seealso \code{\link{smacofSym}} \code{\link{dimest}} \code{\link{dimest.R2}}
#' @examples 
#' set.seed(1)
#' dat <- matrix(rnorm(200),100,2)
#' dmat <- as.vector(dist(dat))
#' dmat2 <- replicate(2,dmat+rnorm(4950))
#' dimcvfit <- dimest.cv(dmat2)
dimest.cv <- function(dmat,epsratio=.001,mds.control=list(type="ordinal",itmax=1000)){
  if (class(dmat)!="matrix"){
      stop("The argument 'dmat' must be a m x n matrix composed of (n) vectorized distance matrices.")
  }
  ddims <- dim(dmat)
  nobj <- dim(squareform(dmat[,1]))[1]
  if (ddims[2]<2){
    stop("Crossvalidation requires at least two distance matrices")
  }
  dmeans <- lapply(1:ddims[2],function(z) apply(dmat,1,function(x,out) mean(x[-out]),out=z))
  avgdimcor<-c()
  i <- 0
  lessthaneps <- 0
  converged <- FALSE
  while (!converged){
    i <- i + 1
    curdimcor <- rep(NA,ddims[2])
    for (n in 1:ddims[2]){
      mdfit <- smacof::smacofSym(squareform(dmat[,n]),i,type=mds.control$type,itmax = mds.control$itmax)
      curdimcor[n]<-cor(mdfit$confdiss,dmeans[[n]])
    }
    avgdimcor[i]<-mean(curdimcor)
    if (i > 1){
      delta <- avgdimcor[i]-avgdimcor[i-1]
      if (eps > delta){
        lessthaneps <- lessthaneps + 1
        if (lessthaneps > 1){
          converged <- TRUE
        }
      } else {
        if (lessthaneps == 1){
          lessthaneps <- 0
        }
      }
    } else {
      eps <- epsratio*avgdimcor[i]
    }
  }
  result <- list(cv.cor=avgdimcor,ndim=i-2)
  return(result)
}