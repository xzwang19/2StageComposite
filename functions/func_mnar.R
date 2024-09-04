func_mnar <-function(N, simdata, simdata_comp5, mask){
  
  setDT(simdata_comp5)
  setDT(simdata)
  percent <- 0.25
  
  if (mask == "lower"){
    
    simdata_comp5 <- simdata_comp5[order(composite5_6)]
    simdata_comp5$composite5_6[sample(1:ceiling((percent*N)), ceiling((percent*N*0.8)))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(composite5_7)]
    simdata_comp5$composite5_7[sample(1:ceiling((percent+0.05)*N), ceiling((percent+0.05)*N*0.8))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(composite5_8)]
    simdata_comp5$composite5_8[sample(1:ceiling((percent+0.1)*N), ceiling((percent+0.1)*N*0.8))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(composite5_9)]
    simdata_comp5$composite5_9[sample(1:ceiling((percent+0.15)*N), ceiling((percent+0.15)*N*0.8))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(framid)]
    simdata <- simdata[order(framid)]
    
    setDF(simdata)
    setDF(simdata_comp5)
    
    simdata[is.na(simdata_comp5$composite5_6), grepl("6", names(simdata))]<-NA
    simdata[is.na(simdata_comp5$composite5_7), grepl("7", names(simdata))]<-NA
    simdata[is.na(simdata_comp5$composite5_8), grepl("8", names(simdata))]<-NA
    simdata[is.na(simdata_comp5$composite5_9), grepl("9", names(simdata))]<-NA
  }
  
  if (mask=="upper"){
    simdata_comp5 <- simdata_comp5[order(composite5_6, decreasing = T)]
    simdata_comp5$composite5_6[sample(1:ceiling((percent*N)), ceiling((percent*N*0.8)))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(composite5_7, decreasing = T)]
    simdata_comp5$composite5_7[sample(1:ceiling((percent+0.05)*N), ceiling((percent+0.05)*N*0.8))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(composite5_8, decreasing = T)]
    simdata_comp5$composite5_8[sample(1:ceiling((percent+0.1)*N), ceiling((percent+0.1)*N*0.8))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(composite5_9, decreasing = T)]
    simdata_comp5$composite5_9[sample(1:ceiling((percent+0.15)*N), ceiling((percent+0.15)*N*0.8))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(framid)]
    simdata <- simdata[order(framid)]
    
    setDF(simdata)
    setDF(simdata_comp5)
    
    simdata[is.na(simdata_comp5$composite5_6), grepl("6", names(simdata))]<-NA
    simdata[is.na(simdata_comp5$composite5_7), grepl("7", names(simdata))]<-NA
    simdata[is.na(simdata_comp5$composite5_8), grepl("8", names(simdata))]<-NA
    simdata[is.na(simdata_comp5$composite5_9), grepl("9", names(simdata))]<-NA
  }
  
  if (mask == "both"){
    simdata_comp5 <- simdata_comp5[order(composite5_6, decreasing = T)]
    simdata_comp5$composite5_6[sample(1:ceiling((percent/2*N)), ceiling((percent/2*N*0.8)))] <- NA
    simdata_comp5$composite5_6[sample((N-ceiling((percent/2*N))):N, ceiling((percent/2*N*0.8)))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(composite5_7, decreasing = T)]
    simdata_comp5$composite5_7[sample(1:ceiling(((percent+0.05)/2*N)), ceiling(((percent+0.05)/2*N*0.8)))] <- NA
    simdata_comp5$composite5_7[sample((N-ceiling(((percent+0.05)/2*N))):N, ceiling(((percent+0.05)/2*N*0.8)))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(composite5_8, decreasing = T)]
    simdata_comp5$composite5_8[sample(1:ceiling(((percent+0.1)/2*N)), ceiling(((percent+0.1)/2*N*0.8)))] <- NA
    simdata_comp5$composite5_8[sample((N-ceiling(((percent+0.1)/2*N))):N, ceiling(((percent+0.1)/2*N*0.8)))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(composite5_9, decreasing = T)]
    simdata_comp5$composite5_9[sample(1:ceiling(((percent+0.15)/2*N)), ceiling(((percent+0.15)/2*N*0.8)))] <- NA
    simdata_comp5$composite5_9[sample((N-ceiling(((percent+0.15)/2*N))):N, ceiling(((percent+0.15)/2*N*0.8)))] <- NA
    
    simdata_comp5 <- simdata_comp5[order(framid)]
    simdata <- simdata[order(framid)]
    
    # all.equal(simdata_simp7$framid, simdata$framid)
    
    setDF(simdata)
    setDF(simdata_comp5)
    
    simdata[is.na(simdata_comp5$composite5_6), grepl("6", names(simdata))]<-NA
    simdata[is.na(simdata_comp5$composite5_7), grepl("7", names(simdata))]<-NA
    simdata[is.na(simdata_comp5$composite5_8), grepl("8", names(simdata))]<-NA
    simdata[is.na(simdata_comp5$composite5_9), grepl("9", names(simdata))]<-NA
  }
  
  return(simdata)
  
  
}