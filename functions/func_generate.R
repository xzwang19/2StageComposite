

func_generate <- function(n, random, corr, meansd){
  
  cor2cov <- function(R, S) {
    sweep(sweep(R, 1, S, "*"), 2, S, "*")
  }
  ## convert from correlation to covariance
  
  covs <- cor2cov(corr, meansd[,2])
  
  set.seed(random)
  
  simdata=rmvnorm(n,mean=meansd[,1],sigma=covs)
  
  colnames(simdata) <- colnames(corr)
  
  for (i in c(1:5, 31)) {
    threshold <- quantile(simdata[,i], (1-mean(simdata[,i])))
    simdata[,i] =  ifelse(simdata[,i] > threshold, 1, 0)
  }
  
  
  simdata <- as.data.frame(simdata)
  setDT(simdata)
  
  ###### generate other ages and id variables #######
  
  simdata$AGE6<-simdata$AGE5+5
  simdata$AGE7<-simdata$AGE5+10
  simdata$AGE8<-simdata$AGE5+15
  simdata$AGE9<-simdata$AGE5+20
  simdata$framid <- seq(1,nrow(simdata))
  
  return(simdata)
  
  }


# as.vector(colMeans(simdata))
# as.vector(apply(simdata, 2, sd))




