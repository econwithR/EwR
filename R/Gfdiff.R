#' Generalized differencing methods
#'
#' This function uses generalized differencing method for correction autocorrelation.
#' @param x series name,
#' @param y series name
#' @keywords Autocorrelation
#' @export
#' @importFrom stats embed lm residuals 
#' @examples IHR = REcoData$IHR
#' @examples ITH = REcoData$ITH
#' @examples Gfdiff(IHR, ITH)
#'


Gfdiff <- function(y, x){
  mod1 = lm(y~x)
  res1 = residuals(mod1)
  ee1 = sum(res1*res1)
  tart = sum(embed(res1,2)[,2]*embed(res1,2)[,1])
  p = tart/ee1
  yp = embed(y,2)[,1] - embed(p*y,2)[,2]
  x = as.matrix(x)
  sayx = dim(x)[2]
  xp = embed(x,2)[,1] - embed(p*x,2)[,-c(1:sayx)]
  mod2 = lm(yp~xp)
  res2 = residuals(mod2)
  ee2 = sum(res2*res2)
  print(summary(mod2))
}

