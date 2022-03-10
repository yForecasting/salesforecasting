#' Copy to clipboard function
#'
#' Copies data object to clipboard
#'
#' This function copies data object to clipboard for easy paste in Word, Excel
#'  or email.
#'
#'
#'
#' @param x A dataframe, matrix or other data object
#' @param rn Rownames are not copied when argument is set to FALSE (default)
#'
#' @author Yves R. Sagaert
#'
#'
#' @export
#'
#' @examples
#' copydata <- matrix(ncol=10,nrow=10,data=rnorm(100,100,10))
#' cp(copydata)
#'

cp <- function(x,rn=FALSE){
  # Fast command to copy to clipboard
  utils::write.table(x,"clipboard",sep="\t",row.names=rn)
}