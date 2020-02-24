#' Standardized Regression
#'
#' This function computee standardized regression model.
#' @param y series name,
#' @param x series name
#' @keywords Standardized Regression
#' @export
#' @importFrom stats lm sd 
#' @examples IHR = REcoData$IHR
#' @examples ITH = REcoData$ITH
#' @examples stdreg(IHR,ITH)
#'

stdreg <- function(y,x){
  yy = (y-mean(y))/sd(y)
  xx = (x-mean(x))/sd(x)
  model = lm(yy~xx-1)
  print(summary(model))

}

