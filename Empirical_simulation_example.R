################################################################################################
#### Empirical Simulation 
#### R version: 4.1.2
#### Author: Yuxin Ding
#### Peer Reviewer: Yushi Liu
#### Last update: 11/28/2024
################################################################################################

# rm(list=ls())

#---------------------------------------------------------------------#
#-------------------------- Library Calls ----------------------------#
#---------------------------------------------------------------------#
library(tidyverse)
library(haven)
library(reshape2)
library(table1)
#---------------------------------------------------------------------#
#---------------------------- Data input -----------------------------#
#---------------------------------------------------------------------#
# Load the dataset with complete records
dat.c <- read.csv("example.csv", header = TRUE)

# Dataset by treatment arm
dat.c.ly <- dat.c %>% filter(TRT01P=="LY") %>% select(-TRT01P)
dat.c.gl <- dat.c %>% filter(TRT01P=="GL") %>% select(-TRT01P)

#---------------------------------------------------------------------------#
#------------------------ Summary table by treatment -----------------------#
#---------------------------------------------------------------------------#

#### Table 1 #####
table1(~ AGE+factor(SEX)+HBA1CBL+HBA1C_wk4+HBA1C_wk12+HBA1C_wk26+HBA1C_wk39+HBA1C_wk52+BFSGMGDL+FSG_wk4+FSG_wk12+FSG_wk26+FSG_wk39+FSG_wk52+BTRGMGDL+
         TRG_wk4+TRG_wk12+TRG_wk26+TRG_wk39+TRG_wk52+NHypoe_BL+NHypoe_wk12+NHypoe_wk26+NHypoe_wk52+THypoe_BL+THypoe_wk12+THypoe_wk26+THypoe_wk52| TRT01P, data=dat.c, overall = FALSE)

t.sum <- rbind(apply(dat.c.gl,2,mean),
               apply(dat.c.gl,2,sd),
               apply(dat.c.gl,2,max),
               apply(dat.c.gl,2,min),
               apply(dat.c.ly,2,mean),
               apply(dat.c.ly,2,sd),
               apply(dat.c.ly,2,max),
               apply(dat.c.ly,2,min))
rownames(t.sum) <- c("mean.r", "sd.r", "min.r", "max.r","mean.t", "sd.t", "min.t", "max.t")

#-------------------------------------------------------------------------------------------#
#----------------------------------- Empirical Simulation ----------------------------------#
# Use dat.c.gl as historical data,  and the summary statistics of dat.c.ly as the targets,
# to generate the new dataset. Comparing the new dataset with dat.c.ly.
#-------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------#
#   Step1: Get the values of tau and delta                            #
#---------------------------------------------------------------------#
t <- c()
d <- c()
for (i in 1:ncol(dat.c.ly)){
  t[i] = (max(dat.c.ly[,i])-min(dat.c.ly[,i]))/(max(dat.c.gl[,i])-min(dat.c.gl[,i]))
  d[i] = min(dat.c.ly[,i])/t[i] - min(dat.c.gl[,i])
}

#---------------------------------------------------------------------#
#   Step2: Step-wise searching for alpha and beta                     #
#---------------------------------------------------------------------#
swgs <- function(a1,a2,as,b1,b2,bs,tau,delta,datref,tarmean,tarsd,type){
  var=names(datref)
  datk <- datref 
  n0 <- length(datk)
  
  F_inv_c <- function(u, alpha, beta){
    x <- u^{alpha}*(1-u)^{beta}
    t1 <- floor(n0*(x))
    t1 <- ifelse(t1==0, 1, t1)
    r <- which(base::rank(datk,ties.method= "first")==as.numeric(t1))
    res <- datk[r]
    return(res)
  }
  
  F_inv_b <- function(u, alpha, beta){
    x <- u^{alpha}*(1-u)^{beta}
    p <- sum(datk)/n0
    res <- ifelse(x<p, 1, 0)
    return(res)
  }
  
  F_inv_o <- function(u, alpha, beta){
    x <- u^{alpha}*(1-u)^{beta}
    p <- summary(as.factor(datk))/n0
    temp <- summary(as.factor(datk))
    cutpoint <- as.numeric(names(temp))
    K <- length(p)
    p.cumulative <- p
    for(k in 2:K) p.cumulative[k] <- p.cumulative[k-1] + p[k]
    res <- ifelse(x<p.cumulative[1], cutpoint[1], 0)
    for(k in 2:K){
      res <- ifelse((x>=p.cumulative[k-1]) & (x<p.cumulative[k]), cutpoint[k], res)
    }
    return(res)
  }
  
  F_est <- function(a,b){
    temp <- c()
    res <- list()
    for (k in 1:2000){
      w <- seq(0,1,by=0.0005)
      if (type=="continuous") {
        temp[k] <- F_inv_c(u=w[k], alpha=a, beta=b)
      }
      else if (type=="binary"){
        temp[k] <- F_inv_b(u=w[k], alpha=a, beta=b)
      }
      else if (type=="ordinal"){
        temp[k] <- F_inv_o(u=w[k], alpha=a, beta=b)
      }
    }
    m_t <- sum(temp)/2000 * tau + delta
    sd_t <- sqrt((sum(temp^2)/2000 - (sum(temp)/2000)^2)) * tau
    res$mean <- m_t
    res$sd <- sd_t
    return(res)
  }
  
  al1 = seq(a1,a2, by=as)
  bl1 = seq(b1,b2, by=bs)
  gs <- expand.grid(al1,bl1)
  
  for (j in 1:nrow(gs)){
    gs[j,3] <- F_est(a=gs[j,1], b=gs[j,2])$mean
    gs[j,4] <- F_est(a=gs[j,1], b=gs[j,2])$sd
  }
  colnames(gs) <- c("Alpha", "Beta", "mean.e", "sd.e")
  
  input1 <- c('tau'=tau,'delta'=delta,'var'=var)
  input2 <- c('alpha'=paste0("[",a1,",",a2,"], by=",as),'beta'=paste0("[",b1,",",b2,"], by=",bs))
  output <- gs
  output2 <- gs %>% mutate(diff_m=abs(mean.e-tarmean),diff_s=abs(sd.e-tarsd)) %>% arrange(diff_m) %>% filter(row_number() %in% 1:3)
  hissum <- c(mean.r=mean(datref), sd.r=sd(datref))
  
  resl <- list(Historical.summary=hissum, Target.summary=c(mean.t=tarmean,sd.t=tarsd), Input1=input1,Input2=input2, `Searching Results`=output, `Top 3 Searching Results`=output2)
  return(resl)
}

# Get the optimal Alpha and Beta values for each variable, may start with a wider, reasonable range as the example below:
# swgs(a1=0.5,a2=5,as=0.5,b1=0,b2=5,bs=0.5,tau=t[1],delta=d[1],datref=dat.c.gl %>% pull(1),tarmean=t.sum[5,1],tarsd=t.sum[6,1])


#---------------------------------------------------------------------#
#  Step 3: Data Generation                                            #
#---------------------------------------------------------------------#
# optimal alpha and beta were obtained from the step-wise grid searching
# a <- c(x,xx,xxx,...,xxxx)
# b <- c(rep(0,28))

# Data generation function

simu.dat <- function(dat0, N, method, tau, delta, alpha, beta){
  n0 <- nrow(dat0)
  
  F_u <- (apply(dat0, 2, rank)-0.5)/n0
  Phi_inv <- qnorm(F_u)
  CV <- cov(Phi_inv)
  ##
  Mu1 <- apply(Phi_inv,2,mean)  
  Mu2 <- rep(0, ncol(dat0))
  dat.ge <- MASS::mvrnorm(n = N, mu=Mu2, Sigma=CV, tol = 1e-6, empirical = FALSE) 
  Phi <- pnorm(dat.ge)
  Phi_f <- matrix(NA, nrow = nrow(Phi), ncol=ncol(Phi))
  for (i in 1:ncol(Phi)){
    Phi_f[,i] <- Phi[,i]^{alpha[i]}*(1-Phi[,i])^{beta[i]}
  }
  
  dat.f <- matrix(NA, nrow = N, ncol = ncol(dat0))
  if(method == "simple"){
    t1 <- floor(n0*(Phi_f))
    t1[which(t1==0)]=1           ############## set 0 to 1
    # t1 <- floor(n0*(Phi_f))+1
    # t1[which(t1==n0+1)]=n0           ############## set n0+1 to n0
    for (i in c(1, 3:5, 8:22)){
      a1 <- pull(dat0, colnames(dat0)[i])
      a2 <- (sapply(t1[,i], function(x) a1[which(rank(a1,ties.method= "first")==x)]) * tau[i]) + delta[i]
      dat.f[,i] <- a2
    }
    
    for (i in c(2)){
      a1 <- pull(dat0, colnames(dat0)[i])
      p <- sum(a1)/n0
      dat.f[,i] <- ifelse(Phi_f[,i]<p, 1, 0)
    }
    
    for (i in c(6:7, 23:28)){
      a1 <- pull(dat0, colnames(dat0)[i])
      p <- summary(as.factor(a1))/n0
      temp <- summary(as.factor(a1))
      cutpoint <- as.numeric(names(temp))
      K <- length(p)
      p.cumulative <- p
      for(k in 2:K) p.cumulative[k] <- p.cumulative[k-1] + p[k]
      res <- ifelse(Phi_f[,i]<p.cumulative[1], cutpoint[1], 0)
      for(k in 2:K){
        res <- ifelse((Phi_f[,i]>=p.cumulative[k-1]) & (Phi_f[,i]<p.cumulative[k]), cutpoint[k], res)
      }
      dat.f[,i] <- res
    }
  }
  if(method == "continuity correction"){
    f_k_c <- function(u){
      j = floor(n0*(u)+0.5)
      r = rank(a1,ties.method= "first")
      if (n0*u<=0.5){
        f_c <- a1[which(r==1)]-(u-0.5/n0)*(a1[which(r==2)]-a1[which(r==1)])
      }
      else if(n0*u>0.5 & n0*u+0.5<n0){
        f_c <- ((j+0.5)-n0*u)*a1[which(r==(j-1))]+(n0*u-(j-0.5))*a1[which(r==j)]
      }
      else{
        f_c <- a1[which(r==n0)]+(u-1+0.5/n0)*(a1[which(r==n0)]-a1[which(r==(n0-1))])
      }
      return(f_c)
    }
    for (i in 1:ncol(dat0)){
      a1 <- pull(dat0, colnames(dat0)[i])
      a2 <- sapply(Phi[,i], f_k_c)
      dat.f <- cbind(dat.f,(unlist(a2)* tau[i]) + delta[i])
    }
  }
  return(dat.f)
}

res1 <- simu.dat(dat0=dat.c.gl, N=5000, method="simple", tau=t, delta=d, alpha=a, beta=b)
colnames(res1) <- colnames(dat.c.gl)

