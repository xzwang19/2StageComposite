func_comp5_imp <- function(data){
  IMP_ALL <- data
  smk_list <- names(IMP_ALL)[grep('CURRSMK', names(IMP_ALL))]
  bmi_list <- names(IMP_ALL)[grep('BMI', colnames(IMP_ALL))]
  sbp_list <- names(IMP_ALL)[grep('SBP', colnames(IMP_ALL))]
  dbp_list <- names(IMP_ALL)[grep('DBP', colnames(IMP_ALL))]
  tc_list <- names(IMP_ALL)[grep('TC', colnames(IMP_ALL))]
  fasting_list <- names(IMP_ALL)[grep('FASTING', colnames(IMP_ALL))]
  
  var_list <- list(smk_list, bmi_list, sbp_list, dbp_list, tc_list, fasting_list)
  
  setDF(IMP_ALL)
  #### from wide format to long format ####
  
  s11 <- IMP_ALL[, c("imp1","imp2","framid", unlist(var_list[1]))]
  s22 <- melt(s11, id.vars = c("imp1","imp2", "framid"))
  s33 <- separate(s22, variable, sep="(?<=[A-Za-z])(?=[0-9])", into = c("comp", "exam"))
  comp <- unique(s33$comp)
  s33 <- s33[,-4]
  colnames(s33) <- c("imp1","imp2", "framid", "exam", comp)
  s33 <- s33[!duplicated(s33[,c(1,2,3,4)]),]
  s2<-s33
  
  for (i in 2:length(var_list)){
    s11 <- IMP_ALL[, c("imp1","imp2","framid", unlist(var_list[i]))]
    s22 <- melt(s11, id.vars = c("imp1","imp2", "framid"))
    s33 <- separate(s22, variable, sep="(?<=[A-Za-z])(?=[0-9])", into = c("comp", "exam"))
    comp <- unique(s33$comp)
    s33 <- s33[,-4]
    colnames(s33) <- c("imp1","imp2", "framid", "exam", comp)
    s33 <- s33[!duplicated(s33[,c(1,2,3,4)]),]
    s2 <-inner_join(s2, s33, by=c("imp1","imp2", "framid", "exam"))
  }
  
  setDT(s2)
  
  s2$FHS_smk <- with(s2, ifelse(CURRSMK>0, 0, 1))
  s2$FHS_obe <- with(s2, ifelse(BMI>=25, 0, 1))
  s2$FHS_htn <- with(s2, ifelse(SBP >=120 | DBP >=80, 0, 1))
  s2$FHS_lipid <- with(s2 , ifelse(TC >=200 , 0, 1))
  s2$FHS_dia <- with(s2, ifelse(FASTING_BG>=100 , 0, 1))
  
  s2$comp5 <- s2$FHS_smk + s2$FHS_obe + s2$FHS_htn + s2$FHS_lipid + s2$FHS_dia
  
  ##### add age and sex back to data #####
  setDT(s2)
  setDT(IMP_ALL)
  s2 <- inner_join(s2, IMP_ALL[imp1==1&imp2==1, .(framid, SEX, AGE5)], by="framid")
  return(s2)
}



