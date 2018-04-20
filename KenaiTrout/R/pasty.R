#' Create a character vector of capture histories
#'
#' Copied from somewhere
#'
#' @param x A dataframe with rows of individuals and columns of capture events 
#'
#' @return A character vector
#'
#' @export
pasty<-function(x) {
  k<-ncol(x)
  n<-nrow(x)
  out<-array(dim=n)
  for (i in 1:n){
    out[i]<-paste(x[i,],collapse="")
  }
  return(out)
}