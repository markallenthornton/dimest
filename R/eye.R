#' Identity matrix
#' 
#' @description Define identity matrix
#' @usage eye(n)
#' @param n size of matrix
#' @return m an n x n identity matrix
#' @details Defines identity matrix of dimensions n x n. Reproduces functionality of identically named MATLAB function.
#' @examples 
#' eye(5)
#' @export eye
eye <- function(n){
  mat <- matrix(0,n,n)
  diag(mat)<-1
  return(mat)
}

