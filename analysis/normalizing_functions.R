ridit <- function(x){
  tab <- table(x)
  tot <- sum(tab)
  prop <- tab/tot
  
  cum <- list()
  sigma <- 0
  for (n in rownames(prop)){
    cum[n] <- prop[n]*.5 + sigma
    sigma <- sigma + prop[n]
  }
  
  out <- c()
  
  for (val in x){
    out <- c(out, cum[[val]])
  }
  
  return(out)
}

zscore <- function(x){
  mean.x <- mean(x)
  sd.x <- sd(x)
  
  out <- (x - mean.x) / sd.x
  
  return(out)
}