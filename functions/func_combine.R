func_combine<-function(Q, U, n.nested, n.model, alpha = 0.05) {
  # Calculates post-imputation mean and variance of point estimates
  # and fractions of missing information for nested imputations
  
  # Args:
  #   Q: vector of point estimates across imputations
  #      should be sorted by nest
  #   U: vector of variances of point estimates across imputations
  #      should be sorted by nest
  #   n.nested: number of imputations within each nest, n
  #   n.model: number of models (nests), m
  
  # Reconfigure mi estimates for nested imputation
  # so that columns represent models (nests)
  Q <- matrix(data=Q,nrow=n.nested,ncol=n.model)
  U <- matrix(data=U,nrow=n.nested,ncol=n.model)
  
  qbar <- mean(Q)  # overall mean
  
  ubar <- mean(U)  # overal variance
  
  b.mn <- var(apply(Q, 2, mean))  # between nest variance
  
  w.mn <- mean(apply(Q, 2, var))  # within nest variance
  
  # total variance
  t.mn <- ubar + ((1 - (1/n.nested)) * w.mn) + ((1 + (1/n.model)) * b.mn)
  
  # total standard deviation
  st.mn <- sqrt(t.mn)
  
  # df for qbar
  df <- ((1/(n.model - 1))*((((1 + (1/n.model)) * b.mn)/t.mn)^2) + 
           (1/(n.model * (n.nested - 1))) * ((((1 - (1/n.nested)) * w.mn)/t.mn)^2))^(-1)
  
  # lower CI for q
  lower.CI <- qbar - qt(1 - alpha/2, df) * sqrt(t.mn)
  
  # upper CI for q
  upper.CI <- qbar + qt(1 - alpha/2, df) * sqrt(t.mn)
  
  
  result <- data.frame(qbar = qbar, lower.CI = lower.CI, upper.CI = upper.CI)
  return(result)
}
