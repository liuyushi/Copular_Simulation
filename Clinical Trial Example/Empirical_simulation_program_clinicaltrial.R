################################################################################################
#### Empirical Simulation - for the paper resubmission
#### Data input: /lillyce/prd/ly2605541/i2r_mc_bidj/final/data/analysis/shared/adam
#### Author: Yuxin Ding
#### Peer reviwer: 
#### Last update: 11/28/2024
################################################################################################

rm(list=ls())

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
data.read.path <- "/lillyce/prd/ly2605541/i2r_mc_bidj/final/data/analysis/shared/adam"
adsl.dat <- read_sas(paste(data.read.path,"/adsl.sas7bdat",sep=""))
ada1chyp.dat <- read_sas(paste(data.read.path,"/ada1chyp.sas7bdat",sep=""))
adlb.dat <- read_sas(paste(data.read.path,"/adlb.sas7bdat",sep=""))
adhypsum.dat <- read_sas(paste(data.read.path,"/adhypsum.sas7bdat",sep=""))


#---------------------------------------------------------------------#
#------------------------ Dataset preparation ------------------------#
#---------------------------------------------------------------------#
# Selected variables: HbA1c (adlb), fasting serum glucose (adlb), triglycerides, nocturnal hypoglycemia (adhypsum), total hypoglycemia (adhypsum)

adsl.dat.sub <- adsl.dat %>% filter(TRT01P=="LY"|TRT01P=="GL") %>% dplyr::select(USUBJID, TRT01P, AGE, SEX, HBA1CBL, BFSGMGDL, BTRGMGDL) 

# HBA1C
HBA1C.dat <- adlb.dat %>% filter(PARAM=="Hemoglobin A1C (%) - CN", VISANLFL=="Y", AVISIT %in% c("Visit 7","Visit 13","Endpoint (Wk26)","Visit 18", "Endpoint (Wk52)")) %>% 
  dplyr::select(USUBJID,AVISIT,AVAL) %>% dcast(USUBJID ~ AVISIT, value.var = "AVAL") %>% rename(HBA1C_wk4=`Visit 7`, HBA1C_wk12=`Visit 13`, HBA1C_wk26=`Endpoint (Wk26)`,HBA1C_wk39=`Visit 18`,HBA1C_wk52=`Endpoint (Wk52)`)
# FSG
FSG.dat <- adlb.dat %>% filter(PARAM=="Glucose (mg/dL) - CN", VISANLFL=="Y", AVISIT %in% c("Visit 7","Visit 13","Endpoint (Wk26)","Visit 18", "Endpoint (Wk52)")) %>% 
  dplyr::select(USUBJID,AVISIT,AVAL) %>% dcast(USUBJID ~ AVISIT, value.var = "AVAL") %>% rename(FSG_wk4=`Visit 7`, FSG_wk12=`Visit 13`,FSG_wk26=`Endpoint (Wk26)`,FSG_wk39=`Visit 18`,FSG_wk52=`Endpoint (Wk52)`)
# triglycerides
TRG.dat <- adlb.dat %>% filter(PARAM=="Triglycerides (mg/dL) - CN", VISANLFL=="Y", AVISIT %in% c("Visit 7","Visit 13","Endpoint (Wk26)","Visit 18", "Endpoint (Wk52)")) %>% 
  dplyr::select(USUBJID,AVISIT,AVAL) %>% dcast(USUBJID ~ AVISIT, value.var = "AVAL") %>% rename(TRG_wk4=`Visit 7`,TRG_wk12=`Visit 13`,TRG_wk26=`Endpoint (Wk26)`,TRG_wk39=`Visit 18`,TRG_wk52=`Endpoint (Wk52)`)
# nocturnal hypoglycemia episode
NHypoE.dat <- adhypsum.dat %>% filter(PARAM=="Primary Analysis Nocturnal Hypoglycemia <=70 mg/dL Episodes", AVISGR1=="0<wk<=12"|AVISGR1=="0<wk<=26"|AVISGR1=="0<wk<=52") %>% 
  dplyr::select(USUBJID,AVISGR1,AVAL) %>% dcast(USUBJID ~ AVISGR1, value.var = "AVAL") %>% rename(NHypoe_wk12=`0<wk<=12`,NHypoe_wk26=`0<wk<=26`,NHypoe_wk52=`0<wk<=52`)
# total hypoglycemia episode
THypoE.dat <- adhypsum.dat %>% filter(PARAM=="Total Hypoglycemia <=70 mg/dL Episodes", AVISGR1=="0<wk<=12"|AVISGR1=="0<wk<=26"|AVISGR1=="0<wk<=52") %>% 
  dplyr::select(USUBJID,AVISGR1,AVAL) %>% dcast(USUBJID ~ AVISGR1, value.var = "AVAL") %>% rename(THypoe_wk12=`0<wk<=12`,THypoe_wk26=`0<wk<=26`,THypoe_wk52=`0<wk<=52`)
# nocturnal hypoglycemia episode - baseline
NHypoE.bl <- adhypsum.dat %>% filter(PARAM=="Primary Analysis Nocturnal Hypoglycemia <=70 mg/dL Episodes",  ABLFL=="Y") %>% 
  dplyr::select(USUBJID,AVAL) %>% rename(NHypoe_BL=AVAL)
# total hypoglycemia episode - baseline
THypoE.bl <- adhypsum.dat %>% filter(PARAM=="Total Hypoglycemia <=70 mg/dL Episodes",  ABLFL=="Y") %>% 
  dplyr::select(USUBJID,AVAL) %>% rename(THypoe_BL=AVAL)

epts1 <- list(HBA1C.dat, FSG.dat, TRG.dat, NHypoE.dat,THypoE.dat,NHypoE.bl,THypoE.bl) %>%
  Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by=c("USUBJID")), .)

epts <- adsl.dat.sub %>% left_join(epts1, by = "USUBJID") 

epts$SEX <- ifelse(epts$SEX=="M", 1,0 )

# Completed records only
dat.c <- epts[complete.cases(epts),]
# names(dat.c)
dat.c <- dat.c[,c(1:7,29:30,12,10,8,11,9,17,15,13,16,14,22,20,18,21,19,23:28)] # reorder the columns

dat.c <- dat.c %>% filter(USUBJID != "I2R-MC-BIDJ-423-04708")%>% select(-USUBJID)

dat.c.ly <- dat.c %>% filter(TRT01P=="LY") %>% select(-TRT01P)
dat.c.gl <- dat.c %>% filter(TRT01P=="GL") %>% select(-TRT01P)

dat.c %>%
  ggplot( aes(x=TRT01P, y=FSG_wk4, fill=TRT01P)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) 
# dat.c %>% filter(FSG_wk4>500)
# dat.c %>% filter(FSG_wk4>500) %>% select(USUBJID)


#---------------------------------------------------------------------#
#------------------------ Summary table by trt -----------------------#
#---------------------------------------------------------------------#

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
    # t1 <- floor(n0*(x))+1
    # t1 <- ifelse(t1==n0+1, n0, t1) 
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
  
  # Sys.time()
  for (j in 1:nrow(gs)){
    gs[j,3] <- F_est(a=gs[j,1], b=gs[j,2])$mean
    gs[j,4] <- F_est(a=gs[j,1], b=gs[j,2])$sd
  }
  # Sys.time()
  colnames(gs) <- c("Alpha", "Beta", "mean.e", "sd.e")
  
  input1 <- c('tau'=tau,'delta'=delta,'var'=var)
  input2 <- c('alpha'=paste0("[",a1,",",a2,"], by=",as),'beta'=paste0("[",b1,",",b2,"], by=",bs))
  output <- gs
  output2 <- gs %>% mutate(diff_m=abs(mean.e-tarmean),diff_s=abs(sd.e-tarsd)) %>% arrange(diff_m) %>% filter(row_number() %in% 1:3)
  # output2 <- gs %>% mutate(diff_m=abs(mean.e-tarmean),diff_s=abs(sd.e-tarsd)) %>% arrange(diff_m+diff_s) %>% filter(row_number() %in% 1:10)
  # output2 <- gs %>% mutate(diff_m=abs(mean.e-tarmean),diff_s=abs(sd.e-tarsd)) %>% arrange(diff_s) %>% filter(row_number() %in% 1:3)
  
  hissum <- c(mean.r=mean(datref), sd.r=sd(datref))
  
  resl <- list(Historical.summary=hissum, Target.summary=c(mean.t=tarmean,sd.t=tarsd), Input1=input1,Input2=input2, `Searching Results`=output, `Top 3 Searching Results`=output2)
  return(resl)
}
names(dat.c.gl)

start.time <- Sys.time()
swgs(a1=1.18,a2=1.2,as=0.001,b1=0,b2=.5,bs=0.5,tau=t[1],delta=d[1],datref=dat.c.gl %>% pull(1),tarmean=t.sum[5,1],tarsd=t.sum[6,1], type="continuous")
end.time1 <- Sys.time()
swgs(a1=1.05,a2=1.15,as=0.01,b1=0,b2=0,bs=0.5,tau=t[2],delta=d[2],datref=dat.c.gl %>% pull(2),tarmean=t.sum[5,2],tarsd=t.sum[6,2], type="binary")
swgs(a1=0.8,a2=1.0,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[3],delta=d[3],datref=dat.c.gl %>% pull(3),tarmean=t.sum[5,3],tarsd=t.sum[6,3], type="continuous")
end.time2 <- Sys.time()
swgs(a1=0.99,a2=1.02,as=0.001,b1=0,b2=.5,bs=0.5,tau=t[4],delta=d[4],datref=dat.c.gl %>% pull(4),tarmean=t.sum[5,4],tarsd=t.sum[6,4], type="continuous")
swgs(a1=2.03,a2=2.05,as=0.001,b1=0,b2=0.5,bs=0.5,tau=t[5],delta=d[5],datref=dat.c.gl %>% pull(5),tarmean=t.sum[5,5],tarsd=t.sum[6,5], type="continuous")
end.time3 <- Sys.time()
swgs(a1=1,a2=1.2,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[6],delta=d[6],datref=dat.c.gl %>% pull(6),tarmean=t.sum[5,6],tarsd=t.sum[6,6], type="ordinal")
swgs(a1=0.8,a2=0.9,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[7],delta=d[7],datref=dat.c.gl %>% pull(7),tarmean=t.sum[5,7],tarsd=t.sum[6,7], type="ordinal")
swgs(a1=1.6,a2=1.8,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[8],delta=d[8],datref=dat.c.gl %>% pull(8),tarmean=t.sum[5,8],tarsd=t.sum[6,8], type="continuous")
swgs(a1=0.7,a2=0.75,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[9],delta=d[9],datref=dat.c.gl %>% pull(9),tarmean=t.sum[5,9],tarsd=t.sum[6,9], type="continuous")
swgs(a1=1.09,a2=1.11,as=0.001,b1=0,b2=.5,bs=0.5,tau=t[10],delta=d[10],datref=dat.c.gl %>% pull(10),tarmean=t.sum[5,10],tarsd=t.sum[6,10], type="continuous")
end.time4 <- Sys.time()
swgs(a1=2,a2=2.1,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[11],delta=d[11],datref=dat.c.gl %>% pull(11),tarmean=t.sum[5,11],tarsd=t.sum[6,11], type="continuous")
swgs(a1=1.20,a2=1.22,as=0.001,b1=0,b2=.5,bs=0.5,tau=t[12],delta=d[12],datref=dat.c.gl %>% pull(12),tarmean=t.sum[5,12],tarsd=t.sum[6,12], type="continuous")
swgs(a1=1.16,a2=1.17,as=0.001,b1=0,b2=.5,bs=0.5,tau=t[13],delta=d[13],datref=dat.c.gl %>% pull(13),tarmean=t.sum[5,13],tarsd=t.sum[6,13], type="continuous") ##### outlier
swgs(a1=2.4,a2=2.6,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[14],delta=d[14],datref=dat.c.gl %>% pull(14),tarmean=t.sum[5,14],tarsd=t.sum[6,14], type="continuous")
swgs(a1=0.7,a2=0.9,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[15],delta=d[15],datref=dat.c.gl %>% pull(15),tarmean=t.sum[5,15],tarsd=t.sum[6,15], type="continuous")
end.time5 <- Sys.time()
swgs(a1=1.8,a2=2,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[16],delta=d[16],datref=dat.c.gl %>% pull(16),tarmean=t.sum[5,16],tarsd=t.sum[6,16], type="continuous")
swgs(a1=2.3,a2=2.5,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[17],delta=d[17],datref=dat.c.gl %>% pull(17),tarmean=t.sum[5,17],tarsd=t.sum[6,17], type="continuous")
swgs(a1=0.19,a2=0.21,as=0.001,b1=0,b2=.5,bs=0.5,tau=t[18],delta=d[18],datref=dat.c.gl %>% pull(18),tarmean=t.sum[5,18],tarsd=t.sum[6,18], type="continuous")
swgs(a1=0.4,a2=0.6,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[19],delta=d[19],datref=dat.c.gl %>% pull(19),tarmean=t.sum[5,19],tarsd=t.sum[6,19], type="continuous")
swgs(a1=0.4,a2=0.6,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[20],delta=d[20],datref=dat.c.gl %>% pull(20),tarmean=t.sum[5,20],tarsd=t.sum[6,20], type="continuous")
end.time6 <- Sys.time()
swgs(a1=0.4,a2=0.6,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[21],delta=d[21],datref=dat.c.gl %>% pull(21),tarmean=t.sum[5,21],tarsd=t.sum[6,21], type="continuous")
swgs(a1=0.5,a2=0.7,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[22],delta=d[22],datref=dat.c.gl %>% pull(22),tarmean=t.sum[5,22],tarsd=t.sum[6,22], type="continuous")
swgs(a1=1.5,a2=2.5,as=0.1,b1=0,b2=.5,bs=0.5,tau=t[23],delta=d[23],datref=dat.c.gl %>% pull(23),tarmean=t.sum[5,23],tarsd=t.sum[6,23], type="continuous")
swgs(a1=2.1,a2=2.3,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[24],delta=d[24],datref=dat.c.gl %>% pull(24),tarmean=t.sum[5,24],tarsd=t.sum[6,24], type="continuous") ##
swgs(a1=2.6,a2=2.8,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[25],delta=d[25],datref=dat.c.gl %>% pull(25),tarmean=t.sum[5,25],tarsd=t.sum[6,25], type="continuous")
swgs(a1=1.4,a2=1.6,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[26],delta=d[26],datref=dat.c.gl %>% pull(26),tarmean=t.sum[5,26],tarsd=t.sum[6,26], type="continuous")
swgs(a1=1.6,a2=1.8,as=0.01,b1=0,b2=.5,bs=0.5,tau=t[27],delta=d[27],datref=dat.c.gl %>% pull(27),tarmean=t.sum[5,27],tarsd=t.sum[6,27], type="continuous")
swgs(a1=1.57,a2=1.59,as=0.001,b1=0,b2=.5,bs=0.5,tau=t[28],delta=d[28],datref=dat.c.gl %>% pull(28),tarmean=t.sum[5,28],tarsd=t.sum[6,28], type="continuous")
end.time7 <- Sys.time()


a <- c(1.188,1.10,0.85,1.005,2.041,1.18,0.86,1.63,0.75,1.105, 2.06,1.21,1.165,2.5,0.83,1.89,2.38,0.202,0.5,0.49, 0.47,0.6,1.9,2.16,2.66,1.42,1.63,1.578)
b <- c(rep(0,28))

# Notes: treat count data as continuous is better than ordinal

####### Computational cost
end.time1 - start.time
end.time2 - start.time
end.time3 - start.time
end.time4 - start.time
end.time5 - start.time
end.time6 - start.time
end.time7 - start.time

# > end.time7 <- Sys.time()
# > ####### Computational Cost
#   > end.time1 - start.time
# Time difference of 32.04709 secs
# > end.time2 - start.time
# Time difference of 1.118852 mins
# > end.time3 - start.time
# Time difference of 2.456597 mins
# > end.time4 - start.time
# Time difference of 6.729852 mins
# > end.time5 - start.time
# Time difference of 8.943685 mins
# > end.time6 - start.time
# Time difference of 11.66584 mins
# > end.time7 - start.time
# Time difference of 15.81689 mins

dat.time <- data.frame(
  k = c(0,1,3,5,10,15,20,28,0,1,3,5,10,15,20,28),
  t = c(0,0.5341182,1.118852,2.456597,6.729852,8.943685,11.66584,15.81689,
        0,0.5324268,1.102756,2.421782,5.947926,8.055736,10.66102,14.55309),
  m = c(rep("138",8), rep("50",8)))

ggplot(data=dat.time, aes(x=k, y=t, group=m)) +
  geom_line(aes(linetype = m))+
  scale_x_continuous(breaks=c(0,1,3,5,10,15,20,28), labels = c("0","1","3","5","10","15","20","28")) +
  geom_point()+
  labs(y="time cost(min)", x="k")+
  theme_classic()

#---------------------------------------------------------------------#
#   Explore beta and alpha for HbA1c at week52                       #
#---------------------------------------------------------------------#
ex1 <- swgs(a1=0.5,a2=5,as=0.1,b1=0,b2=0.5,bs=0.5,tau=t[12],delta=d[12],datref=dat.c.gl %>% pull(12),tarmean=t.sum[5,12],tarsd=t.sum[6,12], type="continuous")

ex1.res <- ex1$`Searching Results` 
# ex1.res$Alpha <- as.factor(ex1.res$Alpha)

ex1.res <- ex1.res %>% mutate(mean_diff=mean.e-t.sum[5,12], sd_diff=sd.e-t.sum[6,12])

ggplot(ex1.res %>% filter(Beta==0), aes(x=Alpha, y=mean.e)) + 
  geom_point(color = "deepskyblue")+
  ggtitle("")+ labs(x=~ paste(alpha[k]), y = "Estimated mean") +
  geom_hline(yintercept = 6.72, linetype = "dashed", color = "darkgray")+geom_text(aes(4.5,6.72,label = "Target Mean: 6.72", vjust = -1))+
  theme_minimal()

ggplot(ex1.res %>% filter(Beta==0), aes(x=Alpha, y=sd.e)) + 
  geom_point(color = "deepskyblue")+
  ggtitle("")+ labs(x=~ paste(alpha[k]), y = "Estimated standard deviation") +
  geom_hline(yintercept = 0.814, linetype = "dashed", color = "darkgray")+geom_text(aes(3.9,0.814,,label = "Target Standard Deviation: 0.814", vjust = -1))+
  theme_minimal()

ggplot(ex1.res %>% filter(Beta==0), aes(x=Alpha, y=mean_diff)) + 
  geom_point()+
  ggtitle("")+ labs(x=expression(alpha), y = "Estimated mean - target mean") +
  theme_minimal()

ggplot(ex1.res %>% filter(Beta==0), aes(x=Alpha, y=sd_diff)) + 
  geom_point()+
  ggtitle("")+ labs(x=expression(alpha), y = "Estimated standard deviation - target standard deviation") +
  theme_minimal()

#---------------------------------------------------------------------#
#   Data Generation and visualizations                                #
#---------------------------------------------------------------------#

# Data generation function

dat0=dat.c.gl
N=5000
# method="simple"
tau=t
delta=d
alpha=a
beta=b


simu.dat <- function(dat0, N, method, tau, delta, alpha, beta){
  n0 <- nrow(dat0)
  
  F_u <- (apply(dat0, 2, rank)-0.5)/n0
  Phi_inv <- qnorm(F_u)
  CV <- cov(Phi_inv)
  ##
  Mu1 <- apply(Phi_inv,2,mean)  
  Mu2 <- rep(0, ncol(dat0))
  dat.ge <- MASS::mvrnorm(n = N, mu=Mu2, Sigma=CV, tol = 1e-6, empirical = FALSE) 
  # hist(dat.ge[,2])
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
    for (i in c(1, 3:28)){
      a1 <- pull(dat0, colnames(dat0)[i])
      a2 <- (sapply(t1[,i], function(x) a1[which(rank(a1,ties.method= "first")==x)]) * tau[i]) + delta[i]
      dat.f[,i] <- a2
    }
    
    for (i in c(2)){
      a1 <- pull(dat0, colnames(dat0)[i])
      p <- sum(a1)/n0
      dat.f[,i] <- ifelse(Phi_f[,i]<p, 1, 0)
    }
    
    # for (i in c(6:7, 23:28)){
    #   a1 <- pull(dat0, colnames(dat0)[i])
    #   p <- summary(as.factor(a1))/n0
    #   temp <- summary(as.factor(a1))
    #   cutpoint <- as.numeric(names(temp))
    #   K <- length(p)
    #   p.cumulative <- p
    #   for(k in 2:K) p.cumulative[k] <- p.cumulative[k-1] + p[k]
    #   res <- ifelse(Phi_f[,i]<p.cumulative[1], cutpoint[1], 0)
    #   for(k in 2:K){
    #     res <- ifelse((Phi_f[,i]>=p.cumulative[k-1]) & (Phi_f[,i]<p.cumulative[k]), cutpoint[k], res)
    #   }
    #   dat.f[,i] <- res
    # }
  }
  # if(method == "continuity correction"){
  #   f_k_c <- function(u){
  #     j = floor(n0*(u)+0.5)
  #     r = rank(a1,ties.method= "first")
  #     if (n0*u<=0.5){
  #       f_c <- a1[which(r==1)]-(u-0.5/n0)*(a1[which(r==2)]-a1[which(r==1)])
  #     }
  #     else if(n0*u>0.5 & n0*u+0.5<n0){
  #       f_c <- ((j+0.5)-n0*u)*a1[which(r==(j-1))]+(n0*u-(j-0.5))*a1[which(r==j)]
  #     }
  #     else{
  #       f_c <- a1[which(r==n0)]+(u-1+0.5/n0)*(a1[which(r==n0)]-a1[which(r==(n0-1))])
  #     }
  #     return(f_c)
  #   }
  #   for (i in 1:ncol(dat0)){
  #     a1 <- pull(dat0, colnames(dat0)[i])
  #     a2 <- sapply(Phi[,i], f_k_c)
  #     dat.f <- cbind(dat.f,(unlist(a2)* tau[i]) + delta[i])
  #   }
  # }
  return(dat.f)
}


res1 <- dat.f
colnames(res1) <- colnames(dat.c.gl)
res1 <- as.data.frame(res1)
table1(~AGE+factor(SEX)+HBA1CBL+HBA1C_wk4+HBA1C_wk12+HBA1C_wk26+HBA1C_wk39+HBA1C_wk52+BFSGMGDL+FSG_wk4+FSG_wk12+FSG_wk26+FSG_wk39+FSG_wk52+BTRGMGDL+
         TRG_wk4+TRG_wk12+TRG_wk26+TRG_wk39+TRG_wk52+NHypoe_BL+NHypoe_wk12+NHypoe_wk26+NHypoe_wk52+THypoe_BL+THypoe_wk12+THypoe_wk26+THypoe_wk52,data=res1)

test.ly <- res1%>% mutate(var="Simulated BIL")


ori.gl <- dat.c.gl %>% mutate(var="Observed GL")
ori.ly <- dat.c.ly %>% mutate(var="Observed BIL")
dp.ly <- rbind(ori.gl, test.ly, ori.ly)
dp.ly$var <- factor(dp.ly$var,
                    levels = c("Observed GL", "Simulated BIL", "Observed BIL"))

# Note: Load the following file for figure revision, in order to have the same example in the paper
# load(file="program/simu1.RData")

dp.ly$var <- recode_factor(dp.ly$var, `Historical-GL` = "Observed GL", 
                           `Simulated New Treatment Arm` = "Simulated BIL",
                           `Historical-LY` = "Observed BIL")


##################### Summary Statistics##################### 

table1(~ HBA1C_wk52 | var, data=dp.ly, overall = FALSE)
table1(~ factor(SEX) | var, data=dp.ly, overall = FALSE)
table1(~ SEX | var, data=dp.ly, overall = FALSE)
table1(~ THypoe_wk52| var, data=dp.ly, overall = FALSE)


##################### Figures##################### 

library(EnvStats)
dp.ly %>% 
  # filter(var=="Historical-GL"|var=="Simulated New Treatment Arm") %>%
  ggplot( aes(x=var, y=HBA1C_wk52, fill=var)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_brewer() +
  # geom_jitter(aes(x=var, y=HBA1C_wk52), size=0.4, alpha=0.9) +
  ylab("HbA1c at week 52")+xlab("")+
  stat_mean_sd_text(digits=2)+
  # theme(legend.position = "none")+
  theme_light()

# ggsave("output/hba1c_1n.png", width = 8.5, height = 5)

table(dp.ly$var)

## Q-Q plot
ori.gl <- dp.ly %>% filter(var=="Observed GL")
ori.ly <- dp.ly %>% filter(var=="Observed BIL")
test.ly <- dp.ly %>% filter(var=="Simulated BIL")

qqplot(x=ori.ly$HBA1C_wk52, y=test.ly$HBA1C_wk52, xlab = "Observed BIL", ylab = "Simulated BIL")
ks.test(x=ori.ly$HBA1C_wk52, y=test.ly$HBA1C_wk52)

require(scales)
dp.ly %>% 
  # filter(var=="Historical-GL"|var=="Simulated New Treatment Arm") %>% 
  ggplot( aes(x=THypoe_wk52, fill=var)) +
  geom_histogram(binwidth = 10,aes(y = ..density..),show.legend = FALSE) +
  # geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent_format())+
  scale_fill_brewer() +
  theme_light() +
  ylab("Percentage")+xlab("Total Hypoglycemia <=70 mg/dL Episodes")+facet_wrap(~var)
# ggsave("output/TH_1n.png", width = 8.5, height = 5)

qqplot(x=ori.ly$THypoe_wk52, y=test.ly$THypoe_wk52, xlab = "Observed BIL", ylab = "Simulated BIL")
ks.test(x=ori.ly$THypoe_wk52, y=test.ly$THypoe_wk52)


#---------------------------------------------------------------------#
#                      Plot the heatmap                               #
#---------------------------------------------------------------------#
# res2.a <- res1[[1]]
res2.a <- dp.ly %>% filter(var=="Simulated BIL") %>% select(-var)
dat.pbo.a <- dat.c.gl
dat.4mg.a <- dat.c.ly

colnames(res2.a) <- c( "AGE", "SEX", "HBA1C_BL", "FSG_BL", "TRG_BL", "NHypoe_BL", "THypoe_BL", "HBA1C_wk4", "HBA1C_wk12", 
                       "HBA1C_wk26", "HBA1C_wk39", "HBA1C_wk52", "FSG_wk4", "FSG_wk12", "FSG_wk26", "FSG_wk39", "FSG_wk52", 
                       "TRG_wk4", "TRG_wk12", "TRG_wk26", "TRG_wk39", "TRG_wk52", "NHypoe_wk12", "NHypoe_wk26", "NHypoe_wk52", 
                       "THypoe_wk12", "THypoe_wk26", "THypoe_wk52")
colnames(dat.pbo.a) <- c( "AGE", "SEX", "HBA1C_BL", "FSG_BL", "TRG_BL", "NHypoe_BL", "THypoe_BL", "HBA1C_wk4", "HBA1C_wk12", 
                          "HBA1C_wk26", "HBA1C_wk39", "HBA1C_wk52", "FSG_wk4", "FSG_wk12", "FSG_wk26", "FSG_wk39", "FSG_wk52", 
                          "TRG_wk4", "TRG_wk12", "TRG_wk26", "TRG_wk39", "TRG_wk52", "NHypoe_wk12", "NHypoe_wk26", "NHypoe_wk52", 
                          "THypoe_wk12", "THypoe_wk26", "THypoe_wk52")
colnames(dat.4mg.a) <- c( "AGE", "SEX", "HBA1C_BL", "FSG_BL", "TRG_BL", "NHypoe_BL", "THypoe_BL", "HBA1C_wk4", "HBA1C_wk12", 
                          "HBA1C_wk26", "HBA1C_wk39", "HBA1C_wk52", "FSG_wk4", "FSG_wk12", "FSG_wk26", "FSG_wk39", "FSG_wk52", 
                          "TRG_wk4", "TRG_wk12", "TRG_wk26", "TRG_wk39", "TRG_wk52", "NHypoe_wk12", "NHypoe_wk26", "NHypoe_wk52", 
                          "THypoe_wk12", "THypoe_wk26", "THypoe_wk52")

cormat.simu <- round(cor(res2.a),2)
cormat.gl <- round(cor(dat.pbo.a),2)
cormat.ly <- round(cor(dat.4mg.a),2)


# cormat.gl <- round(cor(ori.gl[,c(1:20,24:26,33:35,41:42)]),2)

### Heatmap
# Melt the correlation matrix
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat.simu)
upper_tri <- get_upper_tri(cormat.gl)
upper_tri <- get_upper_tri(cormat.ly)

upper_tri
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_fixed()
# ggsave("output/Corr_simu.png", width = 7.5, height = 7.5)
# ggsave("output/Corr_gl.png", width = 7.5, height = 7.5)
# ggsave("output/Corr_ly.png", width = 7.5, height = 7.5)

#---------------------------------------------------------------------#
#   Repeat the generation for 200 times                               #
#---------------------------------------------------------------------#
#### multiple samples
## Test 1: no change on the order stats, but use Mu1-empirical instead of Mu2

res1 <- list()
for (ll in 1:100){
  res1[[ll]] <- simu.dat(dat0=dat.c.gl, N=5000, method="simple", tau=t, delta=d, alpha=a, beta=b)
  colnames(res1[[ll]]) <- colnames(dat.c.gl)
  res1[[ll]] <- as.data.frame(res1[[ll]])
  print(ll)
}

# Note: load the following two datasets to retrieve the data presented in the paper
load(file="RCT_simulation/paper/res_newcode_part1.RData")
res_part1=res1
load(file="RCT_simulation/paper/res_newcode_part2.RData")
res_part2=res1

library(matrixStats)
res.200rep=c(res_part1[1:100],res_part2[1:100])
mean.200rep=t(matrix(unlist(lapply(res.200rep, colMeans)), nrow=28))
sdf <- function(x){colSds(as.matrix(x))}
sd.200rep=t(matrix(unlist(lapply(res.200rep, sdf)), nrow=28))

colnames(mean.200rep)<- colnames(dat.c.gl)
colnames(sd.200rep)<- colnames(dat.c.gl)
mean.200rep <- as.data.frame(mean.200rep)
sd.200rep <- as.data.frame(sd.200rep)

msef <- function(v, x){
  sum((v-x)^2)/length(v)
}
msef(v=mean.200rep$HBA1C_wk52, 6.72)
msef(v=sd.200rep$HBA1C_wk52, 0.81)
sqrt(msef(v=mean.200rep$HBA1C_wk52, 6.72))
sqrt(msef(v=sd.200rep$HBA1C_wk52, 0.81))

msef(v=mean.200rep$SEX, 0.5746269)
msef(v=sd.200rep$SEX, 0.4953244)
sqrt(msef(v=mean.200rep$SEX, 0.5746269))
sqrt(msef(v=sd.200rep$SEX, 0.4953244))

sqrt(msef(v=mean.200rep$THypoe_wk52, 14.2))
sqrt(msef(v=sd.200rep$THypoe_wk52, 20.8))



