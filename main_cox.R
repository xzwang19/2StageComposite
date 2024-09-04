library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
library(mice)
library(nlme)
library(lme4)
library(reshape2)
library(mvtnorm)
library(JM)

######## load functions ########

source("functions/func_generate.R") ## generate full simulated data
source("functions/func_comp5.R") ## generate Composite-5 score using simulated data
source("functions/func_mar.R") ### generate MAR
source("functions/func_mnar.R") ### generate MNAR
source("functions/func_cox_generate.R") ### generate survival times
source("functions/func_2stage_cox.R") ### implement two-stage MI in order to perform Cox models
source("functions/func_fcs_cox.R") ### FCS methods for Cox models
source("functions/func_comp5_imp.R") ### generate Composite-5 after imputation
source("functions/func_combine.R") ### two-stage combining rule, see Siddique J, Harel O, Crespi CM. 
                                   ### Addressing Missing Data Mechanism Uncertainty using Multiple-Model Multiple Imputation: Application to a Longitudinal Clinical Trial. Ann Appl Stat. 2012 Dec 1;6(4):1814-1837. 

########## create simulated data ###########
n=3700
load("data/corrsim.RData") ### load correlation structure
load("data/meansdsim.RData") ### load mean and sd

random <- 1
set.seed(random)
simdata <- func_generate(n = n, random = random, corr = corrsim, meansd = meansdsim)
setDT(simdata)

################### Create Composite-5  #################

simdata_comp5 <- func_comp5(simdata)
simdata_comp5$random <- random

################## Divide into two groups #############

'%!in%' <- function(x,y)!('%in%'(x,y))
setDT(simdata)
setDT(simdata_comp5)
simdata1<-simdata[framid %in% sample(framid, round(n*0.5))]
simdata2<-simdata[framid %!in% simdata1$framid]

simdata_comp5_1 <- simdata_comp5[framid %in% simdata1$framid]
simdata_comp5_2 <- simdata_comp5[framid %in% simdata2$framid]

COX_RESULTS <- c()
counts = 0

################# masking MAR non-attendees and missing components ##########################

simdata_mar <- func_mar(N = nrow(simdata1), simdata = simdata1, simdata_comp5 = simdata_comp5_1)

###################### Masking NMAR non-attendees #################

for (l in c("lower", "upper", "both")){
  
  simdata_mnar <- func_mnar(N = nrow(simdata2), simdata = simdata2, simdata_comp5 = simdata_comp5_2, mask = l)
  
  setDT(simdata_mar)
  setDT(simdata_mnar)
  
  simmiss <- rbind(simdata_mnar, simdata_mar)
  
  print(paste0("MAR: ", (nrow(simdata_mar)-nrow(na.omit(simdata_mar)))/nrow(simmiss), 
               " NMAR: ", (nrow(simdata_mnar)-nrow(na.omit(simdata_mnar)))/nrow(simmiss)))
  
  ############## add mortality data #################
  mortdata <- func_cox_generate(simdata_comp5) #### generate survival times
  mortdata1 <- mortdata[!duplicated(framid, fromLast=T)]
  ch0 <- nelsonaalen(mortdata1, fup, event)
  mortdata1$ch0 <- ch0
  simmiss <- inner_join(simmiss, mortdata1[,.(framid, event, ch0, exam)], by="framid")
  
  ################ remove observations after event time ###################
  
  simmiss_mort <- data.frame(matrix(vector(), 0, dim(simmiss)[2]))
  colnames(simmiss_mort) <- colnames(simmiss)
  
  for (i in 1:3700){
    
    simmiss_ind <- simmiss[framid==i]
    setDF(simmiss_ind)
    
    if (simmiss_ind$exam==5){
      simmiss_ind[,grepl("6", names(simmiss_ind))] <- NA
      simmiss_ind[,grepl("7", names(simmiss_ind))] <- NA
      simmiss_ind[,grepl("8", names(simmiss_ind))] <- NA
      simmiss_ind[,grepl("9", names(simmiss_ind))] <- NA
    } else if (simmiss_ind$exam==6){
      simmiss_ind[,grepl("7", names(simmiss_ind))] <- NA
      simmiss_ind[,grepl("8", names(simmiss_ind))] <- NA
      simmiss_ind[,grepl("9", names(simmiss_ind))] <- NA
    } else if (simmiss_ind$exam==7){
      simmiss_ind[,grepl("8", names(simmiss_ind))] <- NA
      simmiss_ind[,grepl("9", names(simmiss_ind))] <- NA
    } else if (simmiss_ind$exam==8){
      simmiss_ind[,grepl("9", names(simmiss_ind))] <- NA
    } else {
      simmiss_ind <- simmiss_ind
    }
    
    simmiss_mort <- rbind(simmiss_mort, simmiss_ind)
    
  }
  
  setDT(simmiss_mort)
  
  ########################## two-stage imputation ############################
  
  for ( k in c(0.8, 0.9, 1, 1.1, 1.2)){
    for ( m in c(5)){
      
      bk <- ifelse(k<1, 0.5, 
                   ifelse(k==1, 1, 2))
      
      for (met in c("lfcs", "afcs", "xfcs")){
        
        print(paste0("analyzing ",counts+1, " model(s)"))
        
        simdata_mnar_mort <- simmiss_mort[framid %in% simdata_mnar$framid]
        simdata_mar_mort <- simmiss_mort[framid %in% simdata_mar$framid]
        
        IMP_ALL <- func_2stage_cox(mnar_data = simdata_mnar_mort, mar_data = simdata_mar_mort, method = met, m=m, n=5, k=k, bk=bk)
        
        ####### generate imputed composite-5 #########
        
        imp_comp5 <- func_comp5_imp(IMP_ALL)
        
        ###################### ANALYZE COX MODEL ######################
        COX_BETA <- c()
        COX_VAR <- c()
        
        for (data_imp1 in 1:max(imp_comp5$imp1)){
          for (data_imp2 in 1:max(imp_comp5$imp2)){
            cox_data <- imp_comp5[imp1==data_imp1 & imp2==data_imp2]
            mortdata$exam <- as.character(mortdata$exam)
            cox_mort <- inner_join(cox_data, mortdata, by=c("framid", "exam"))
            setDT(cox_mort)
            cox_mort <- cox_mort[order(framid, exam)]
            
            ##### run Cox model #####
            imp.cox <- coxph(Surv(start, stop, event2) ~ AGE5 + SEX + comp5, data=cox_mort)
            cox_beta <- coef(imp.cox)[3]
            cox_var <- diag(vcov(imp.cox))[3]
            COX_BETA <- c(COX_BETA, cox_beta)
            COX_VAR <- c(COX_VAR, cox_var)
          }
        }
        
        cox_results <- func_combine(Q=COX_BETA, U=COX_VAR, n.nested = 5, n.model = m, alpha = 0.05)
        cox_results$k <- k
        cox_results$bk <- bk
        cox_results$m <- m
        cox_results$random <- random
        cox_results$missing <- l
        cox_results$method <- met
        
        COX_RESULTS <- rbind(COX_RESULTS, cox_results)
        
      
        counts = counts+1
      }
    }
  }
  
}

###### save COX results ########
saveRDS(COX_RESULTS,paste0("output/COX_RESULTS_",group,".rds"))





