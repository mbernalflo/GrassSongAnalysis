#' Identification of local maxima and minima.
#' 
#' @description Identifies (local) maxima and minima in a vector of numbers.
#'
#' @param x A vector to be analyzed
#' @param tol A tolerance level to be ignored.
#' @param partial Boolean. If TRUE, local extremes will be identified.
#' @param decreasing Boolean, if TRUE minima are identified, if FALSE maxima 
#'   are identified.
#' 
#' @return 
#' Returns a vector of minima/maxima locations within \code{x}.
#' 
#' @references 
#' By jamie.f.olson on stackoverflow 
#' 
#' @author Holger Schielzeth (holger.schielzeth@@uni-bielefeld.de)
#' 
#' @examples   
#' # My examples
#' 
#' @export
#' 

identifyExtremes <- function(x, tol=1e-6, partial=TRUE, decreasing=FALSE){
  if (decreasing){
    if (partial){
      which(diff(c(FALSE,diff(x)>tol,TRUE))>0)
    }else {
      which(diff(diff(x)>tol)>0)+1
    }
  }else {
    if (partial){
      which(diff(c(TRUE,diff(x)>=tol,FALSE))<0)
    }else {
      which(diff(diff(x)>=tol)<0)+1
    }
  }
}