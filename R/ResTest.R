#' Restriction Tests
#'
#' This function computes LM, LR and Wald test statistics for redundant variable.
#' @param y series name,
#' @param x1 series name
#' @param x2 series name
#' @keywords Restriction Tests
#' @export
#' @importFrom stats lm 
#' @examples IHR = REcoData$IHR
#' @examples ITH = REcoData$ITH
#' @examples DK =REcoData$DK
#' @examples ResTest(IHR,ITH,DK)
#'

ResTest <- function(y,x1,x2){

  model1 = lm(y~x1)
  model2 = lm(y~x2)

  Ftest = ((summary(model1)$r.squared - summary(model2)$r.squared) / (dim(x1)[2] - dim(x2)[2])) / ((1-summary(model1)$r.squared) / (dim(x1)[1] - dim(x1)[2]))
  print(c("Ftest=",Ftest))
  pvalueFtest = pf(Ftest, (dim(x1)[2]-dim(x2)[2]), (dim(x1)[1]-dim(x1)[2]), lower.tail = FALSE)
  print(c("Ftest p-value",pvalueFtest))


  LRtest = (dim(x1)[1])*log((1-summary(model2)$r.squared)/(1-summary(model1)$r.squared))
  print(c("LRtest=",LRtest))
  pvalueLRtest = pchisq(LRtest, df = (dim(x1)[2]-dim(x2)[2]), lower.tail = FALSE)
  print(c("LRtest p-value=",pvalueLRtest))


  LMTEST =  (summary(model1)$r.squared - summary(model2)$r.squared) / ((1-summary(model2)$r.squared) / dim(x1)[1])
  print(c("LMTEST",LMTEST))
  pvalueLMTEST = pchisq(LMTEST, df = (dim(x1)[2]-dim(x2)[2]), lower.tail = FALSE)
  print(c("LMTEST p-value",pvalueLMTEST))

  Waldtest = (summary(model1)$r.squared - summary(model2)$r.squared) / ((1-summary(model1)$r.squared) / dim(x1)[1])
  print(c("WaldTest",Waldtest))

  pvalueWald = pchisq(Waldtest, df = (dim(x1)[2]-dim(x2)[2]), lower.tail = FALSE)
  print(c("WaldTest",pvalueWald))

}




