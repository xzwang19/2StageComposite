func_fcs <- function(data, m, method){
  
  if (method=="lfcs"){
    ini <- mice(data, maxit = 0, pri = FALSE)
    pred <- ini$predictorMatrix
    
    pred[paste0("CURRSMK", 5:9), c(paste0("BMI", 5:9), paste0("SBP", 5:9), paste0("DBP", 5:9), paste0("TC", 5:9), paste0("FASTING_BG", 5:9))] <- 0
    pred[paste0("BMI", 5:9), c(paste0("CURRSMK", 5:9), paste0("SBP", 5:9), paste0("DBP", 5:9), paste0("TC", 5:9), paste0("FASTING_BG", 5:9))] <- 0
    pred[paste0("SBP", 5:9), c(paste0("CURRSMK", 5:9), paste0("BMI", 5:9), paste0("DBP", 5:9), paste0("TC", 5:9), paste0("FASTING_BG", 5:9))] <- 0
    pred[paste0("DBP", 5:9), c(paste0("CURRSMK", 5:9), paste0("SBP", 5:9), paste0("BMI", 5:9), paste0("TC", 5:9), paste0("FASTING_BG", 5:9))] <- 0
    pred[paste0("TC", 5:9), c(paste0("CURRSMK", 5:9), paste0("SBP", 5:9), paste0("DBP", 5:9), paste0("BMI", 5:9), paste0("FASTING_BG", 5:9))] <- 0
    pred[paste0("FASTING_BG", 5:9), c(paste0("CURRSMK", 5:9), paste0("SBP", 5:9), paste0("DBP", 5:9), paste0("BMI", 5:9), paste0("TC", 5:9))] <- 0
    
    meth <- c("","logreg","logreg","logreg","logreg", rep(c("", "norm.boot", "norm.boot", "norm.boot", "norm.boot"), 5), "", "")
    
    imp <- mice(data, pred = pred, method = meth, pri = FALSE, m = m, maxit = 20)
  }
  
  if (method=="afcs"){
    ini <- mice(data, maxit = 0, pri = FALSE)
    #pred <- quickpred(data)
    pred <- ini$predictorMatrix
    
    
    meth <- c("","logreg","logreg","logreg","logreg", rep(c("", "norm.boot", "norm.boot", "norm.boot", "norm.boot"), 5), "", "")
    
    
    imp <- mice(data, pred = pred, method = meth, pri = FALSE, m = m, maxit = 20)
  }
  
  if (method=="xfcs"){
    ini <- mice(data, maxit = 0, pri = FALSE)
    pred <- ini$predictorMatrix
    
    meth <- c("","logreg","logreg","logreg","logreg", rep(c("", "norm.boot", "norm.boot", "norm.boot", "norm.boot"), 5), "", "")
    
    trait <- c("CURRSMK", "BMI", "SBP", "DBP", "TC", "FASTING_BG")
    pred[pred>0] <- 0
    
    for (i in 1:6){
      for (j in 1:6){
        diag(pred[paste0(trait[i], 5:9), paste0(trait[j], 5:9)]) <- 1
      }
    }
    
    for (i in 1:6){
      diag(pred[paste0(trait[i], 5:9), paste0(trait[i], 5:9)]) <- 0
    }
    
    pred[,"AGE5"] <- 1
    pred[, "SEX"] <- 1
    
    imp <- mice(data, pred = pred, method = meth, pri = FALSE, m = m, maxit = 20)
    
  }
  
  return (imp)
  
  
  
}