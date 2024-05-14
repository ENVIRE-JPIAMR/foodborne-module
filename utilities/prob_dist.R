#########################################################################################################################################
# FSA project FS307037 
# Development of a modular assessment framework to quantify the risk of AMR exposure via food products: example of chicken and lettuce
##########################################################################################################################################
# File Name:     Function_ProbaDistribution.R
# Description:   Set of functions to generate truncated probabilities distributions

# Company:       Ausvet Eu
# Author:        Céline Faverjon
# Role:          Epidemiologist
# Email:         celine@ausvet.eu
# Date:          03.02.21
##########################################################################################################################################

#truncated normal distributions
rnormt <- function(n, min, max, mu, s = 1) {
  
  # range is a vector of two values
  
  F.a <- pnorm(min , mean = mu, sd = s)
  F.b <- pnorm(max , mean = mu, sd = s)
  
  u <- runif(n, min = F.a, max = F.b)
  
  qnorm(u, mean = mu, sd = s)
  
}

#truncated gamma distribution
rgammat <- function(n, min, max, shape, scale = 1) {
  
  F.a <- pgamma(min, shape = shape, scale = scale)
  F.b <- pgamma(max, shape = shape, scale = scale)
  
  u <- runif(n, min = F.a, max = F.b)
  qgamma(u, shape = shape, scale = scale)
}

#truncated poisson distribution
rpoist <- function(n, min, max, lambda) {
  
  F.a <- ppois(min, lambda = lambda)
  F.b <- ppois(max, lambda= lambda)
  
  u <- runif(n, min = F.a, max = F.b)
  qpois(u, lambda = lambda)
}

#truncated beta distribution
rbetat <- function(n, min, max, shape1,shape2) {
  
  F.a <- pbeta(min, shape1 = shape1, shape2=shape2)
  F.b <- pbeta(max, shape1 = shape1, shape2=shape2)
  
  u <- runif(n, min = F.a, max = F.b)
  qbeta(u, shape1 = shape1, shape2=shape2)
}

#truncated log logistic distribution
rllogt <- function(n, min, max, shape,scale) {
  
  F.a <- pllog(min, shape = shape, scale=scale)
  F.b <- pllog(max, shape = shape, scale=scale)
  
  u <- runif(n, min = F.a, max = F.b)
  qllog(u, shape = shape, scale=scale)
}

#truncated shifted log logistic distribution
rllog3t <- function(n, min, max, shape,scale, thres) {
  
  F.a <- pllog3(min, shape = shape, scale=scale, thres=thres)
  F.b <- pllog3(max, shape = shape, scale=scale, thres=thres)
  
  u <- runif(n, min = F.a, max = F.b)
  qllog3(u, shape = shape, scale=scale, thres=thres)
}

#truncated laplace distribution
rtrunclaplace <- function(n, mean, scale, lower, upper) {
  r.out <- numeric(n)
  nrem <- n
  begin <- 1
  
  repeat {
    ntake <- max(nrem, 100)
    r <- rlaplace(ntake, mean, scale)
    ii <- r > lower & r <= upper
    
    nr <- sum(ii)
    end <- min(n, begin + nr - 1)
    r.out[begin:end] <- r[ii][1:min(nr, nrem)]
    
    nrem <- n - end
    if (nrem <= 0)
      break
    else {
      begin <- end + 1
    }
  }
  
  r.out
}
