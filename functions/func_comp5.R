func_comp5 <- function(data){
r1 <- data
setDF(r1)

age_list <- names(r1)[grep('AGE', names(r1))]
smk_list <- names(r1)[grep('CURRSMK', colnames(r1))]
bmi_list <- names(r1)[grep('BMI', colnames(r1))]
sbp_list <- names(r1)[grep('SBP', colnames(r1))]
dbp_list <- names(r1)[grep('DBP', colnames(r1))]
tc_list <- names(r1)[grep('TC', colnames(r1))]
fasting_list <- names(r1)[grep('FASTING', colnames(r1))]

var_list <- list(age_list, smk_list, bmi_list, sbp_list, dbp_list, tc_list, fasting_list)

r2 <- data.frame(framid=unique(r1$framid), exam = c(rep(5,nrow(r1)), rep(6,nrow(r1)), rep(7,nrow(r1)), rep(8,nrow(r1)), rep(9,nrow(r1))))

for (i in 1:length(var_list)){
  
  r_var1 <- r1[, c("framid", unlist(var_list[i]))]
  r_var2 <- melt(r_var1, id.vars=c("framid"))
  r_var3 <- separate(r_var2, variable, sep="(?<=[A-Za-z])(?=[0-9])", into = c("comp", "exam"))
  colnames(r_var3) <- c("framid", "comp", "exam", unique(r_var3$comp))
  r_var3 <- r_var3[,-2]
  r_var3 <- r_var3[!duplicated(r_var3[,c(1,2)]),]
  r_var3$exam <- as.numeric(r_var3$exam)
  r2 <-left_join(r2, r_var3, by=c("framid", "exam"))
  
}

setDT(r2)

r2$FHS_smk <- with(r2, ifelse(CURRSMK==0, 1, 0))
r2$FHS_obe <- with(r2, ifelse(BMI>=25, 0, 1))
r2$FHS_htn <- with(r2, ifelse(SBP >=120 | DBP >=80, 0, 1))
r2$FHS_lipid <- with(r2 , ifelse(TC >=200, 0, 1))
r2$FHS_dia <- with(r2, ifelse(FASTING_BG>=100, 0, 1))

r2$comp5 <- r2$FHS_smk + r2$FHS_obe + r2$FHS_htn + r2$FHS_lipid + r2$FHS_dia

r2_simp7_wide <- spread(r2[,.(framid, exam, comp5)], exam, comp5)
colnames(r2_simp7_wide) <- c("framid", paste0("composite5_", 5:9))

setDT(r1)
r2_wide <- inner_join(r2_simp7_wide, r1[,.(framid, SEX, AGE5)], by="framid")
return(r2_wide)
}



