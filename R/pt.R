#' pt function
#'
#' Gives orderly and limited head of the dataframe or matrix.
#'
#' This function gives a subsample of the data for visual inspection. The 
#' number of columns is limited to 10, the number of rows is limited to 5.
#'
#'
#'
#' @param data A dataframe or matrix
#'
#' @author Yves R. Sagaert
#'
#'
#' @return A data frame or matrix
#' @export
#'
#' @examples
#' bigdataframe <- matrix(ncol=100,nrow=100,data=rnorm(10000,100,10))
#' pt(data = bigdataframe)
#'

pt <- function(data) {
  # show head of data to 5 row and 10 col
  return(data[1:min(nrow(data),5),1:min(ncol(data),10)])
}
