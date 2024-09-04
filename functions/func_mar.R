func_mar <- function(N, simdata, simdata_comp5){

  
  ################# mask missing SBP and DBP #################
  setDF(simdata)
  simdata_ind <- simdata[1:ceiling(N/2),]
  simdata_noa <- simdata[(ceiling(N/2)+1):N,]
  simdata_comp5 <- simdata_comp5[framid %in% simdata_noa$framid, ]
  
  coef_sbp6 <- c(-3.2435, 0.0029, 0.014, -0.0854) # 0.1
  coef_sbp7 <- c(-4.1538, 0.0060, 0.0289, -0.2885) # 0.13
  coef_sbp8 <- c(-5.3353, 0.0081, 0.06, -0.322) # 0.26
  coef_sbp9 <- c(-5.9439, 0.0097, 0.0795, -0.2528) # 0.42
  coef_sbp <- data.frame(sbp6 = coef_sbp6, sbp7 = coef_sbp7, sbp8 = coef_sbp8, sbp9 = coef_sbp9)
  
  SBP_MAR <- c() ### create missing indicators in sim data
  for (i in 1:4){
    coefs <- coef_sbp[i]
    sbp_logodds <- coefs[1,] + coefs[2,] *simdata_ind$SBP5 + coefs[3,]*simdata_ind$AGE5 + coefs[4,]*simdata_ind$SEX
    sbp_rp <- exp(sbp_logodds) / (exp(sbp_logodds) + 1) # Suppress values between 0 and 1 via inverse-logit
    sbp_mar <- rbinom(nrow(simdata_ind), 1, sbp_rp) # indicator of missing
    SBP_MAR <- cbind(SBP_MAR, sbp_mar)
  }
  
  SBP_MAR <- as.data.frame(SBP_MAR)
  names(SBP_MAR) <- paste0("sbp_mar_", 6:9)
  var_sbp_mar <-c(names(SBP_MAR))
  var_sbp <- c(paste0("SBP", 6:9))
  var_dbp <- c(paste0("DBP", 6:9))

  for (i in 1:4){
    simdata_ind[SBP_MAR[,var_sbp_mar[i]]==1, var_sbp[i]] <- NA ### create missingness in the sim data
  }
  for (i in 1:4){
    simdata_ind[SBP_MAR[,var_sbp_mar[i]]==1, var_dbp[i]] <- NA # if a SBP is missing, DBP is missing too
  } # create missing SBP and DBP
  
  
  
######################## masking MAR non-attendees ##########################
  
  coef_comp6 <- c(-2.6521, -0.0815, 0.0137, -0.0644) # 0.1
  coef_comp7 <- c(-3.0566, -0.1368, 0.0287, -0.2563) # 0.13
  coef_comp8 <- c(-3.8709, -0.1973, 0.0608, -0.2854) # 0.25
  coef_comp9 <- c(-4.1916, -0.2292, 0.0802, -0.2029) # 0.42
  
  nonatt_mar6 <- coef_comp6[1] + coef_comp6[2] *simdata_comp5$composite5_5 + coef_comp6[3]*simdata_comp5$AGE5 + coef_comp6[4]*simdata_comp5$SEX    
  nonatt_mar7 <- coef_comp7[1] + coef_comp7[2] *simdata_comp5$composite5_5 + coef_comp7[3]*simdata_comp5$AGE5 + coef_comp7[4]*simdata_comp5$SEX
  nonatt_mar8 <- coef_comp8[1] + coef_comp8[2] *simdata_comp5$composite5_5 + coef_comp8[3]*simdata_comp5$AGE5 + coef_comp8[4]*simdata_comp5$SEX
  nonatt_mar9 <- coef_comp9[1] + coef_comp9[2] *simdata_comp5$composite5_5 + coef_comp9[3]*simdata_comp5$AGE5 + coef_comp9[4]*simdata_comp5$SEX
  
  rp6 <- exp(nonatt_mar6) / (exp(nonatt_mar6) + 1) # Suppress values between 0 and 1 via inverse-logit
  rp7 <- exp(nonatt_mar7) / (exp(nonatt_mar7) + 1) # Suppress values between 0 and 1 via inverse-logit
  rp8 <- exp(nonatt_mar8) / (exp(nonatt_mar8) + 1) # Suppress values between 0 and 1 via inverse-logit
  rp9 <- exp(nonatt_mar9) / (exp(nonatt_mar9) + 1) # Suppress values between 0 and 1 via inverse-logit
  
  nonatt6 <- rbinom(nrow(simdata_comp5), 1, rp6) # indicator of missing
  nonatt7 <- rbinom(nrow(simdata_comp5), 1, rp7) # indicator of missing
  nonatt8 <- rbinom(nrow(simdata_comp5), 1, rp8) # indicator of missing
  nonatt9 <- rbinom(nrow(simdata_comp5), 1, rp9) # indicator of missing
  
  setDF(simdata_noa)
  
  simdata_noa[nonatt6==1, grepl("6", names(simdata_noa))]<-NA
  simdata_noa[nonatt7==1, grepl("7", names(simdata_noa))]<-NA
  simdata_noa[nonatt8==1, grepl("8", names(simdata_noa))]<-NA
  simdata_noa[nonatt9==1, grepl("9", names(simdata_noa))]<-NA
  
  simdata <- rbind(simdata_ind, simdata_noa)
  setDT(simdata)
  
  return(simdata)

  
}