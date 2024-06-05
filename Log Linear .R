rm(list=ls());
library('rjags')
library('R2jags')
library(ggplot2)
library(tidyr)
library(plotly)
library(gridExtra)
library(grid)
z1 <- read.csv("data_ODAS_SC.csv", header = TRUE)
z2 <- read.csv("data_ODAS_RC.csv", header = TRUE)
z1 <- z1[,-1]
z2 <- z2[,-1]
zS <- array(as.matrix(z1), dim = c(18, 4, 4, 2, 7))
#dim(aperm(zS, c(5, 3, 2, 1, 4)))

zR <- array(as.matrix(z2), dim = c(18, 4, 4, 2, 7))
#dim(aperm(zR, c(5, 3, 2, 1, 4))) #test

# 0.1 denotes missing observation    
#zS <- extend_dim(zS,1, 3, 0.1)
#zR <- extend_dim(zR, 1, 2, 0.1)

#for (i in 1:32) {
# zS[, i, i, , ] <- 0.1
# zR[, i, i, , ] <- 0.1
#}

# Transform size to (TODAS) array
zS <- aperm(zS, c(5, 3, 2, 1, 4))
zR <- aperm(zR, c(5, 3, 2, 1, 4))


#zS[zS==0.1] <- NA
#zR[zR==0.1] <- NA

data_list <- list(
  zR = round(zR),
  zS = round(zS),
  nT = 7,
  nC = 4,
  #nC = 32,
  nA = 18,
  nS = 2 #,
  #totalO = 1, # index of total in origin
  #totalD = 1 # index of total in origin
)
model_jags_original_simplified <- 
  "model {
  
  # Likelihood function 
  for (t in 1:nT) {
    for (a in 1:nA) {
      for (s in 1:nS) {
        for (o in 1:(nC-1)) {
          for (d in (o+1):nC) { #upper-right triangle
            zR[t, o, d, a, s] ~ dpois(lambda.R[t, o, d, a, s])   #lambda = v
            zS[t, o, d, a, s] ~ dpois(lambda.S[t, o, d, a, s])
          }
          for(d in 1:o){ #bottom-left triangle
            zR[t, o+1, d, a, s] ~ dpois(lambda.R[t, o+1, d, a, s])   #lambda = v
            zS[t, o+1, d, a, s] ~ dpois(lambda.S[t, o+1, d, a, s])
          }
        }
      }
    }
  }
  
  # Model
  for (t in 1:nT) {
    for (a in 1:nA) {
      for (s in 1:nS) {
        for (i in 1:nC){
           m[t, i, i, a, s] <- 0
           pi.prop[t, i, i, a, s]<-0
        }
        
        for (o in 1:(nC-1)) {
          for (d in (o+1):nC) { #upper-right triangle
            m[t, o, d, a, s] ~ dlnorm(alpha.da[d,a] +
                                      alpha.ds[d,s] +
                                      alpha.oa[o,a] +
                                      alpha.os[o,s], tau.m)
          
                                      
            lambda.R[t, o, d, a, s] ~ dlnorm(log(m[t, o, d, a, s]) + alpha.R[t, o, d], tau.R)
            lambda.S[t, o, d, a, s] ~ dlnorm(log(m[t, o, d, a, s]) + alpha.S[t, o, d], tau.S)
          }
          for(d in 1:o){  #bottom-left triangle
            m[t, o + 1, d, a, s] ~ dlnorm(alpha.da[d,a] +
                                          alpha.ds[d,s] +
                                          alpha.oa[o+1,a] +
                                          alpha.os[o+1,s] , tau.m)
                                      
            lambda.R[t, o+1, d, a, s] ~ dlnorm(log(m[t, o+1, d, a, s]) + alpha.R[t, o+1, d], tau.R)
            lambda.S[t, o+1, d, a, s] ~ dlnorm(log(m[t, o+1, d, a, s]) + alpha.S[t, o+1, d], tau.S)
          }
        }
      }
    }
  }
  
  # Calculate m.sum and pi
  
  for (t in 1:nT) {
    for (o in 1:(nC-1)) {
      for (d in (o+1):nC) { #upper-right triangle
        m.sum[t, o, d] <- sum(m[t, o, d, 1:nA, 1:nS]) + 0.0001
        for (a in 1:nA) {
          for (s in 1:nS) {
            pi.prop[t, o, d, a, s] <- m[t, o, d, a, s] / m.sum[t, o, d]
          }
        }
      }
      for(d in 1:o){  #bottom-left triangle
        m.sum[t, o+1, d] <- sum(m[t, o+1, d, 1:nA, 1:nS]) + 0.0001
        for (a in 1:nA) {
          for (s in 1:nS) {
            pi.prop[t, o+1, d, a, s] <- m[t, o+1, d, a, s] / m.sum[t, o+1, d]
          }
        }
      }
    }
  }
  
  # Priors
  
  alpha.a[1] <- 0
  for (a in 2:nA) { 
    alpha.a[a] ~ dnorm(0, 1);
  }
  
  alpha.s[1] <- 0
  alpha.s[2] ~ dnorm(0, 1);

  tau.S ~ dgamma(0.001, 0.001)T(0.0001,20.00)
  tau.R ~ dgamma(0.001, 0.001)T(0.0001,20.00)
  tau.m ~ dgamma(0.001, 0.001)T(0.0001,20.00)
  
  ## interaction
  for (t in 1:nT) {
    for (o in 1:(nC-1)) {
      for (d in (o+1):nC) { #upper-right triangle
        alpha.R[t,o,d] ~ dnorm(0, 0.01)
        alpha.S[t,o,d] ~ dnorm(0, 0.01)
      }
      for(d in 1:o){  #bottom-left triangle
        alpha.R[t,o+1,d] ~ dnorm(0, 0.01)
        alpha.S[t,o+1,d] ~ dnorm(0, 0.01)
      }
    }
  }
  
  tau.alpha.da ~ dgamma(0.01, 0.01)T(0.0001,20.00)
  for (d in 1:nC) {
    for (a in 1:nA) {
      alpha.da[d,a] ~ dnorm(alpha.a[a], tau.alpha.da)
    }
  }
  
  tau.alpha.ds ~ dgamma(0.01, 0.01)T(0.0001,20.00)
  for (d in 1:nC) {
    for (s in 1:nS) {
      alpha.ds[d,s] ~ dnorm(alpha.s[s], tau.alpha.ds)
    }
  }
  
  tau.alpha.oa ~ dgamma(0.01, 0.01)T(0.0001,20.00)
  for (o in 1:nC) {
    for (a in 1:nA) {
      alpha.oa[o,a] ~ dnorm(alpha.a[a], tau.alpha.oa)
    }
  }
  
  tau.alpha.os ~ dgamma(0.01, 0.01)T(0.0001,20.00)
  for (o in 1:nC) {
    for (s in 1:nS) {
      alpha.os[o,s] ~ dnorm(alpha.s[s], tau.alpha.os)
    }
  }
}
"

# jags_model <- jags.model(textConnection(model_jags_original_simplified), data = data_list, n.chains = 1)
# jags_samples <- coda.samples(jags_model, n.iter = 1000)
writeLines(model_jags_original_simplified, 'model_jags_original_simplified.txt', useBytes = TRUE)

par_to_collect<-c("m","pi.prop",
                  "alpha.R","alpha.S",
                  'alpha.da','alpha.ds','alpha.oa','alpha.os',
                  "alpha.s","alpha.a",
                  "tau.R","tau.S","tau.m",
                  "τau.alpha.ds","τau.alpha.da","τau.alpha.os","τau.alpha.oa")

#mcmc_results <- res_jags_original_simplified$BUGSoutput$sims.list$pi.prop

#mcmc_results[,1,1,1,,]
##iter, time, orig, des, age, sex, value


mc.cores <- 4
a<-Sys.time()
res_jags_original_simplified <- jags.parallel(
  data=data_list,
  parameters.to.save = par_to_collect,
  DIC = TRUE,
  model.file="model_jags_original_simplified.txt",
  n.iter = 2000,
  n.thin = 5,
  jags.seed = 123,
  n.burnin = 500,
  n.chains = mc.cores,
  n.cluster = mc.cores)
print(Sys.time()-a) # 2h on servers - n.iter = 2000.  n.burnin = 500, n.thin = 5 

save(file="res_jags_original_simplified.rds",list="res_jags_original_simplified")
