
func_cox_generate <- function(simdata_comp5){
  ##################### generate time to event data ########################
  
  pass01<-rep(0,n) 
  
  obs<-c(1:n);time0<-rep(0,n);dtime<-rep(0,n)
  
  database<-data.frame(obs,fup=time0,event=pass01,DTIME=dtime)
  
  j <- 0
  for (k in 1:n){
    j <- j+1
    
    years1 <- 5
    
    rate1 = 0.000015*exp(0.11*unique(simdata_comp5[framid==j]$AGE5)[1] + (-0.54)*unique(simdata_comp5[framid==j]$SEX)[1] + 
                           (-0.22)*unique(simdata_comp5[framid==j]$composite5_5)[1]) # only the 1st random intercept included
    t1 = rexp(1, rate = rate1)
    
    if(t1<=years1){
      bh<-c(j, t1, 1, 5)
    }
    
    if(years1 < t1){
      years2 = 5
      rate2 = 0.000015*exp(0.11*unique(simdata_comp5[framid==j]$AGE5)[1] + (-0.54)*unique(simdata_comp5[framid==j]$SEX)[1] + 
                             (-0.22)*unique(simdata_comp5[framid==j]$composite5_6)[1])
      t2 = rexp(1, rate = rate2)
      
      if(t2<=years2){
        bh<-c(j, years1+t2, 1, 6)
      }
      
      if(years2 < t2){
        years3 = 5
        rate3 = 0.00004*exp(0.11*unique(simdata_comp5[framid==j]$AGE5)[1] + (-0.54)*unique(simdata_comp5[framid==j]$SEX)[1] + 
                              (-0.22)*unique(simdata_comp5[framid==j]$composite5_7)[1])
        t3 = rexp(1, rate = rate3)
        
        if(t3<=years3){
          bh<-c(j, years1+years2+t3, 1, 7)
        }
        
        if(years3 < t3){
          years4 = 5
          rate4 = 0.00006*exp(0.11*unique(simdata_comp5[framid==j]$AGE5)[1] + (-0.54)*unique(simdata_comp5[framid==j]$SEX)[1] + 
                                (-0.22)*unique(simdata_comp5[framid==j]$composite5_8)[1])
          t4 = rexp(1, rate = rate4)
          
          if(t4<=years4){
            bh<-c(j, years1+years2+years3+t4, 1, 8)
          }
          
          if(years4<t4){
            years5 = 5
            rate5 = 0.0003*exp(0.11*unique(simdata_comp5[framid==j]$AGE5)[1] + (-0.54)*unique(simdata_comp5[framid==j]$SEX)[1] + 
                                 (-0.22)*unique(simdata_comp5[framid==j]$composite5_9)[1])
            t5 = rexp(1, rate = rate5)
            
            if(t5<=years5){
              bh<-c(j, years1+years2+years3+years4+t5, 1, 9)
            }
            
            if(years5<t5){
              bh <- c(j, years1+years2+years3+years4+years5, 0, 10)
            }
          }
          
        }
      }
    }
    database[j,]<-bh
  }
  
  setDT(database)
  database$ID <- database$obs
  SURVTTE <- database
  #mean(SURVTTE$event)
  #prop.table(table(SURVTTE$DTIME))
  
  SURVTTE$tsb5 <- 0
  SURVTTE$tsb6 <- 5
  SURVTTE$tsb7 <- 10
  SURVTTE$tsb8 <- 15
  SURVTTE$tsb9 <- 20
  
  SURVTTE <- SURVTTE[,.(ID, tsb5, tsb6, tsb7, tsb8, tsb9, fup, event)]
  colnames(SURVTTE) <- c("framid", "tsb5", "tsb6", "tsb7", "tsb8", "tsb9", "fup", "event")
  
  ############ create start and stop date ################
  
  rmwide <- melt(SURVTTE, id.vars=c("framid", "fup", "event"))
  setDT(rmwide)
  rmwide <- rmwide[value<=fup]
  framids <- unique(rmwide$framid)
  
  RMWIDE <- c()
  for (i in 1:length(framids)){
    
    id <- framids[i]
    indiwide <- rmwide[framid %in% id]
    indiwide$start <- indiwide$value
    indiwide$stop <- c(indiwide$start[-1],unique(indiwide$fup))
    indiwide$event2 <- with(indiwide, ifelse(stop==fup, event, 0))
    RMWIDE <- rbind(RMWIDE, indiwide)
    
  }
  RMWIDE$exam <- with(RMWIDE, ifelse(variable=="tsb5", 5, 
                                     ifelse(variable=="tsb6", 6, 
                                            ifelse(variable=="tsb7", 7, 
                                                   ifelse(variable=="tsb8", 8, 9)))))
  RMWIDE$exam <- as.character(RMWIDE$exam)
  return(RMWIDE)
  
}
