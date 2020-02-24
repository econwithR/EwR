#' Hausmann Test for identification
#'
#' This function allows you to make Hausman Test for identification
#' @param x series name,
#' @param y series name
#' @param z series name
#' @keywords restriction
#' @export
#' @importFrom stats coefficients lm cor pchisq 
#' @examples IHR = REcoData$IHR
#' @examples ITH = REcoData$ITH
#' @examples DK =REcoData$DK
#' @examples HausmanTest(IHR,ITH,DK)
#'

HausmanTest <- function(y,x,z){

  model1 = lm(y~x)
  model2 = lm(y~z)
  b1 = coefficients(model1)[2]
  b0 = coefficients(model2)[2]
  q2 = (b1-b0)^2
  kor2 = (cor(x,z))^2
  v1 = (summary(model1)$coefficient[,'Std. Error'][2])^2
  m = (q2*kor2)/((1-kor2)*v1)
  pvalue = pchisq(m,1, lower.tail = FALSE)
  print(c("Test Stat",m))
  print(c("p-value",pvalue))
}


