#' Lower triangle of matrix
#' 
#' @description Extract and vectorize lower triangular portion of square matrix
#' @usage lowerTriangle(m)
#' @param m a square matrix
#' @return v a vector containing the lower triangular 
#' @examples
#' mat <- as.matrix(dist(1:10))
#' lowerTriangle(mat)
lowerTriangle <- function(m){
  return(m[lower.tri(m)])
}