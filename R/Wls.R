#' Weighted Least Square
#'
#' This Function makes Weighted Least Square estimation.
#' @param y series name,
#' @param x series name
#' @keywords Weighted Least Square estimation
#' @export
#' @importFrom stats lm fitted  
#' @examples IHR = REcoData$IHR
#' @examples ITH = REcoData$ITH
#' @examples Wls(ITH,IHR)
#'


Wls <- function (y,x) {
  model1 = lm(y~x)
  fit1 = fitted(model1)
  sabit = rep(1,length(y))
  model2 = lm(I(y/fit1) ~ 0 + I(x/fit1) + I(sabit/fit1))
  print(model2)

}
