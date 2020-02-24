#' Durbin two stage method
#'
#' This function makes Durbin two stage method for autocorrelation.
#' @param x series name,
#' @param y series name
#' @keywords Autocorrelation
#' @export
#' @importFrom stats embed lm coefficients 
#' @examples IHR = REcoData$IHR
#' @examples ITH = REcoData$ITH
#' @examples Durbin2(ITH,IHR)
#'

Durbin2 <- function(y,x){
  
  model2 = lm(embed(y,2)[,1]~embed(y,2)[,2]+embed(x,2)[,1]+embed(x,2)[,2])
  p = coefficients(model2)[2]
  yp = embed(y,2)[,1] - embed(p*y,2)[,2]
  x = as.matrix(x)
  sayx = dim(x)[2]
  xp = embed(x,2)[,1] - embed(p*x,2)[,-c(1:sayx)]
  mod2 = lm(yp~xp)
  res2 = residuals(mod2)
  ee2 = sum(res2*res2)
  print(summary(mod2))
  
}


