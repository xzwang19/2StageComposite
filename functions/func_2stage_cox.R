func_2stage_cox <- function(mnar_data, mar_data, method, m, n, k, bk){
  
  #### stage 1: impute MNAR for m imputations
  mnar <- subset(mnar_data, select=-c(AGE6, AGE7, AGE8, AGE9, framid, exam))
  mar <- subset(mar_data, select=-c(AGE6, AGE7, AGE8, AGE9, framid, exam))
  
  mnar[, c("CURRSMK5", "CURRSMK6", "CURRSMK7", "CURRSMK8", "CURRSMK9", "SEX")] <- lapply(mnar[, c("CURRSMK5", "CURRSMK6", "CURRSMK7", "CURRSMK8", "CURRSMK9", "SEX")], as.factor)
  mar[, c("CURRSMK5", "CURRSMK6", "CURRSMK7", "CURRSMK8", "CURRSMK9", "SEX")] <- lapply(mar[, c("CURRSMK5", "CURRSMK6", "CURRSMK7", "CURRSMK8", "CURRSMK9", "SEX")], as.factor)
  
  alldata <- rbind(mnar, mar)
  imp_alldata <- func_fcs_cox(data=alldata, m=m, method = method)
  
  pred <- imp_alldata$predictorMatrix
  meth <- imp_alldata$method
  
  cols <- colnames(mice::complete(imp_alldata,1))
  IMP_ALL = data.frame(matrix(vector(), 0, 37,
                              dimnames=list(c(), c(cols, "imp1", "imp2", "framid"))),
                       stringsAsFactors=F)
  
  for (i in 1:imp_alldata$m) {
    
    imp_alldata_long <- mice::complete(imp_alldata, i)
    imp_mnar_sub <- imp_alldata_long[1:nrow(mnar), ]
    imp_mnar_sub_cont <- imp_mnar_sub[,!grepl("CURRSMK", names(imp_mnar_sub))]
    imp_mnar_sub_cont <- subset(imp_mnar_sub_cont, select = -c(AGE5, SEX, event, ch0))
    imp_mnar_sub_bin <- imp_mnar_sub[,grepl("CURRSMK", names(imp_mnar_sub))]
    imp_mnar_sub_bin <- cbind(imp_mnar_sub_bin, imp_mnar_sub[,c("SEX", "AGE5", "event", "ch0")])
    
    setDF(mnar)
    mnar_cont <- mnar[,!grepl("CURRSMK", names(mnar))]
    mnar_cont <- subset(mnar_cont, select = -c(AGE5, SEX, event, ch0))
    mnar_bin <- mnar[,grepl("CURRSMK", names(mnar))]
    mnar_bin <- cbind(mnar_bin, mnar[,c("SEX", "AGE5", "event", "ch0")])
    
    ##### modification of continuous MNAR values
    imp_mnar_sub_cont[is.na(mnar_cont)] <- imp_mnar_sub_cont[is.na(mnar_cont)]*k
    
    ##### modification of binary MNAR values
    logistic6 <- glm(CURRSMK6~CURRSMK5+CURRSMK7+CURRSMK8+CURRSMK9+AGE5+SEX+event+ch0
                     +BMI5+BMI6+BMI7+BMI8+BMI9+SBP5+SBP6+SBP7+SBP8+SBP9
                     +DBP5+DBP6+DBP7+DBP8+DBP9+TC5+TC6+TC7+TC8+TC9
                     +FASTING_BG5+FASTING_BG6+FASTING_BG7+FASTING_BG8+FASTING_BG9, family="binomial", data=imp_mnar_sub)
    logit6 <- predict(logistic6)
    logit6 <- as.matrix(logit6)
    nonig_logit6 <- logit6 + log(bk)
    nonig_prob6 <- exp(nonig_logit6)/(1+exp(nonig_logit6))
    nonig_smoke6 <- rbinom(length(nonig_prob6), 1, nonig_prob6)
    
    logistic7 <- glm(CURRSMK7~CURRSMK5+CURRSMK6+CURRSMK8+CURRSMK9+AGE5+SEX+event+ch0
                     +BMI5+BMI6+BMI7+BMI8+BMI9+SBP5+SBP6+SBP7+SBP8+SBP9
                     +DBP5+DBP6+DBP7+DBP8+DBP9+TC5+TC6+TC7+TC8+TC9
                     +FASTING_BG5+FASTING_BG6+FASTING_BG7+FASTING_BG8+FASTING_BG9, family="binomial", data=imp_mnar_sub)
    logit7 <- predict(logistic7)
    logit7 <- as.matrix(logit7)
    nonig_logit7 <- logit7 + log(bk)
    nonig_prob7 <- exp(nonig_logit7)/(1+exp(nonig_logit7))
    nonig_smoke7 <- rbinom(length(nonig_prob7), 1, nonig_prob7)
    
    logistic8 <- glm(CURRSMK8~CURRSMK5+CURRSMK6+CURRSMK7+CURRSMK9+AGE5+SEX+event+ch0
                     +BMI5+BMI6+BMI7+BMI8+BMI9+SBP5+SBP6+SBP7+SBP8+SBP9
                     +DBP5+DBP6+DBP7+DBP8+DBP9+TC5+TC6+TC7+TC8+TC9
                     +FASTING_BG5+FASTING_BG6+FASTING_BG7+FASTING_BG8+FASTING_BG9, family="binomial", data=imp_mnar_sub)
    logit8 <- predict(logistic8)
    logit8 <- as.matrix(logit8)
    nonig_logit8 <- logit8 + log(bk)
    nonig_prob8 <- exp(nonig_logit8)/(1+exp(nonig_logit8))
    nonig_smoke8 <- rbinom(length(nonig_prob8), 1, nonig_prob8)
    
    logistic9 <- glm(CURRSMK9~CURRSMK5+CURRSMK6+CURRSMK7+CURRSMK8+AGE5+SEX+event+ch0
                     +BMI5+BMI6+BMI7+BMI8+BMI9+SBP5+SBP6+SBP7+SBP8+SBP9
                     +DBP5+DBP6+DBP7+DBP8+DBP9+TC5+TC6+TC7+TC8+TC9
                     +FASTING_BG5+FASTING_BG6+FASTING_BG7+FASTING_BG8+FASTING_BG9, family="binomial", data=imp_mnar_sub)
    logit9 <- predict(logistic9)
    logit9 <- as.matrix(logit9)
    nonig_logit9 <- logit9 + log(bk)
    nonig_prob9 <- exp(nonig_logit9)/(1+exp(nonig_logit9))
    nonig_smoke9 <- rbinom(length(nonig_prob9), 1, nonig_prob9)
    
    imp_mnar_sub_bin[is.na(mnar_bin$CURRSMK6), "CURRSMK6"]<-nonig_smoke6[is.na(mnar_bin$CURRSMK6)]
    imp_mnar_sub_bin[is.na(mnar_bin$CURRSMK7), "CURRSMK7"]<-nonig_smoke7[is.na(mnar_bin$CURRSMK7)]
    imp_mnar_sub_bin[is.na(mnar_bin$CURRSMK8), "CURRSMK8"]<-nonig_smoke8[is.na(mnar_bin$CURRSMK8)]
    imp_mnar_sub_bin[is.na(mnar_bin$CURRSMK9), "CURRSMK9"]<-nonig_smoke9[is.na(mnar_bin$CURRSMK9)]
    
    imp_mnar_sub <- cbind(imp_mnar_sub_bin[,-c(6:9)], imp_mnar_sub_cont)
    setDT(imp_mnar_sub)
    imp_mnar_sub <- cbind(imp_mnar_sub, mnar$SEX, mnar$AGE5, mnar$event, mnar$ch0) ### same order as in pred
    colnames(imp_mnar_sub) <- c(paste0("CURRSMK", 5:9), paste0("BMI", 5:9), paste0("SBP", 5:9), paste0("DBP", 5:9),
                                paste0("TC", 5:9), paste0("FASTING_BG", 5:9), "SEX", "AGE5", "event", "ch0")
    
    s2_full <- rbind(imp_mnar_sub, mar)
    
    #### stage 2: impute MAR for n imputations
    
    imp_mar <- mice(s2_full,
                    m = n,
                    method = meth,
                    pred = pred,
                    maxit = 20,
                    pri = FALSE)
    
    imp_all <- mice::complete(imp_mar, "long")
    setDT(imp_all)
    
    imp_all$imp1 <- i
    imp_all$imp2 <- imp_all$.imp
    alldata2 <- rbind(mnar_data, mar_data)
    imp_all$framid <- rep(alldata2$framid, n)
    imp_all <- imp_all[, -c(1:2)]
    IMP_ALL <- rbind(IMP_ALL, imp_all)
    
  }
  
  IMP_ALL[, c("CURRSMK5", "CURRSMK6", "CURRSMK7", "CURRSMK8", "CURRSMK9", "SEX")] <- lapply(IMP_ALL[, c("CURRSMK5", "CURRSMK6", "CURRSMK7", "CURRSMK8", "CURRSMK9", "SEX")], function(x) as.numeric(x) - 1)
  return(IMP_ALL)
}


