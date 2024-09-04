library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
library(mice)
library(nlme)
library(lme4)
library(reshape2)
library(mvtnorm)

######## load functions ########

source("functions/func_generate.R") ## generate full simulated data
source("functions/func_comp5.R") ## generate Composite-5 score using simulated data
source("functions/func_mar.R") ### generate MAR
source("functions/func_mnar.R") ### generate MNAR
source("functions/func_fcs.R") ### implement three FCS methods
source("functions/func_2stage.R") ### two-stage MI for individual components of Composite-5
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

MEAN_RESULTS <- c()
SLOPE_RESULTS <- c()
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
  
  ########################## two-stage imputation ############################
  
  for ( k in c(0.8, 0.9, 1, 1.1, 1.2)){
    for ( m in c(5)){
      
      bk <- ifelse(k<1, 0.5, 
                   ifelse(k==1, 1, 2))
      
      for (met in c("lfcs", "afcs", "xfcs")){
        
        print(paste0("analyzing ",counts+1, " model(s)"))
        
        IMP_ALL <- func_2stage(mnar_data = simdata_mnar, mar_data = simdata_mar, method = met, m=m, n=5, k=k, bk=bk)
        
        ####### generate imputed composite-5 #########
        
        imp_comp5 <- func_comp5_imp(IMP_ALL)
        
        ###################### ANALYZE SLOPES ######################
        SLOPE_BETA <- c()
        SLOPE_VAR <- c()
        
        for (data_imp1 in 1:max(imp_comp5$imp1)){
          for (data_imp2 in 1:max(imp_comp5$imp2)){
            slope_data <- imp_comp5[imp1==data_imp1 & imp2==data_imp2]
            slope_data$exam <- as.numeric(slope_data$exam)
            slope_data$exam <- slope_data$exam-4
            sum_slope <- lmer(comp5 ~ AGE5 + SEX + exam + (1|framid), data = slope_data)
            slope_beta <- fixef(sum_slope)[4]
            slope_var <- diag(vcov(sum_slope))[4]
            SLOPE_BETA <- c(SLOPE_BETA, slope_beta)
            SLOPE_VAR <- c(SLOPE_VAR, slope_var)
          }
        }
        
        slope_results <- func_combine(Q=SLOPE_BETA, U=SLOPE_VAR, n.nested = 5, n.model = m, alpha = 0.05)
        slope_results$k <- k
        slope_results$bk <- bk
        slope_results$m <- m
        slope_results$random <- random
        slope_results$missing <- l
        slope_results$method <- met
        SLOPE_RESULTS <- rbind(SLOPE_RESULTS, slope_results)
        
        ############### MEAN: combine results to generate estimates and variances ###################
        
        imp_est <- imp_comp5[, .(mean_comp5 = mean(comp5), var_comp5 = var(comp5)/length(unique(imp_comp5$framid))), .(imp1, imp2, exam)]
        
        q6 <- imp_est[exam==6]$mean_comp5
        u6 <- imp_est[exam==6]$var_comp5
        
        q7 <- imp_est[exam==7]$mean_comp5
        u7 <- imp_est[exam==7]$var_comp5
        
        q8 <- imp_est[exam==8]$mean_comp5
        u8 <- imp_est[exam==8]$var_comp5
        
        q9 <- imp_est[exam==9]$mean_comp5
        u9 <- imp_est[exam==9]$var_comp5
        
        ex6 <- func_combine(Q=q6, U=u6, n.nested = 5, n.model = m, alpha = 0.05)
        ex7 <- func_combine(Q=q7, U=u7, n.nested = 5, n.model = m, alpha = 0.05)
        ex8 <- func_combine(Q=q8, U=u8, n.nested = 5, n.model = m, alpha = 0.05)
        ex9 <- func_combine(Q=q9, U=u9, n.nested = 5, n.model = m, alpha = 0.05)
        
        final_results <- rbind(ex6, ex7, ex8, ex9)
        final_results$exams <- seq(6,9)
        
        final_results$k <- k
        final_results$bk <- bk
        final_results$m <- m
        final_results$random <- random
        final_results$missing <- l
        final_results$method <- met
        
        MEAN_RESULTS <- rbind(MEAN_RESULTS, final_results)
        
        counts = counts+1
      }
    }
  }
  
}

###### save slopes ########
saveRDS(SLOPE_RESULTS,paste0("output/SLOPE_RESULTS_",group,".rds"))

###### save means ########

MEAN_RESULTS <- as.data.frame(MEAN_RESULTS)
saveRDS(MEAN_RESULTS,paste0("output/MEAN_RESULTS_",group,".rds"))




