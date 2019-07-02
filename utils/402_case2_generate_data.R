##############################################################################
## Case example 2 Data generation
##############################################################################


library(ggplot2)
library(dplyr)
library(RxODE)
library(caTools)

## PK model

ode <- "
Concentration = centr/V2;
C3 = peri/V3;
C4 = peri2/V4;
d/dt(depot) = -KA*depot;
d/dt(centr) = KA*depot - (CL+Q+Q2)*Concentration + Q*C3 + Q2*C4;
d/dt(peri) =                    Q*Concentration - Q*C3;
d/dt(peri2) =                   Q2*Concentration - Q2*C4;
"

work <- tempfile("Rx_intro-")
mod1 <- RxODE(model = ode, modName = "mod1")


set.seed(12345666)
ndose <- 1
nsub <- 30 # number of subjects per dose
nstud <- 1 # number of studies

# Introduce covariate effects
ETHN <- rep(rep(c(rep("Caucasian",ceiling(nsub/2)),rep("Japanese",floor(nsub/2))),ndose),nstud)
WT0 <- (ETHN=="Caucasian")*runif(length(ETHN),60,110) + (ETHN=="Japanese")*runif(length(ETHN),50,100)
VCOV <- (WT0/70)
CLCOV <- (WT0/70)^0.75*(1 + 1*(ETHN=="Japanese"))

CL <- 0.02*exp(rnorm(nsub*ndose*nstud,0,.3))*CLCOV
KA <- 0.5*exp(rnorm(nsub*ndose*nstud,0,.3))
V2 <- 0.5*exp(rnorm(nsub*ndose*nstud,0,.3))*VCOV

DOSE <- rep(sort(rep(c(300),nsub)), nstud)
STUDY <- sort(rep(c(1:nstud), ndose*nsub))

theta.all <- 
  cbind(KA=KA, CL=CL, V2=V2,
        Q=0.2*CLCOV, V3=2*VCOV, V4 = 200*VCOV, Q2 = 0.2*CLCOV)

# Nominal times for sampling schedule
SAMPLING <- c(-24,-0.1,0.1,0.5,1,2,4,8,12,24, 36, 48)

# Nominal times for dosing schedule
DOSING <- 0

# All nominal times
NT <- data.frame(NT = c(SAMPLING, DOSING), label = c(rep("SAMPLING",length(SAMPLING)),rep("DOSING",length(DOSING))))
NT <- NT[order(NT$NT),]


SAMPLING2 <- c()
DOSING2 <- c()
NT2 <- data.frame(NT = c(SAMPLING2, DOSING2), label = c(rep("SAMPLING",length(SAMPLING2)),rep("DOSING",length(DOSING2))))

# Create dosing & sampling event table
ev.all <- list()
for(idose in 1:length(DOSE)){
  ev.all[[idose]] <- eventTable(amount.units='mg', time.units='hours')
  
  # Include some noise in the sampling & dosing schedules
  temp <- NT
  temp$time <- cumsum(c(temp$NT[1] + 0.1*rnorm(1), abs(temp$NT[2:length(temp$NT)]-temp$NT[1:length(temp$NT)-1] + 0.1*rnorm(length(temp$NT)-1))))
  temp$time <- temp$time - temp$time[temp$NT==0] # center at first dose
  
  ev.all[[idose]]$add.dosing(dose = DOSE[idose], start.time = temp$time[temp$label=="DOSING"])
  ev.all[[idose]]$add.sampling(temp$time[temp$label=="SAMPLING"])
  
}

# Generate simulations
x.all.df <- NULL
ID = 0
for(i in 1:length(ev.all)){
  ID = ID + 1
  
  if(i > length(DOSE)) j = i - length(DOSE) else j = i
  theta <- theta.all[j,]
  
  inits <- c(depot = 0, centr = 0, peri = 0, peri2 = 0, eff=theta["Kin"]/theta["Kout"])
  x <- mod1$solve(theta, ev.all[[i]], inits = inits)
  
  x.df <- data.frame(x)
  if(i > length(DOSE)){ 
    x.df$NT <- SAMPLING2 
  }else{ 
    x.df$NT <- SAMPLING
  }
  
  temp <- data.frame(ev.all[[i]]$get.dosing())
  if(i > length(DOSE)) temp$NT <- DOSING2 else temp$NT <- DOSING
  temp[,setdiff(names(x.df),names(temp))]<- NA
  x.df[,setdiff(names(temp),names(x.df))]<- NA
  x.df <- rbind(x.df,temp)
  
  x.df$ID <- ID
  x.df$DOSE <- DOSE[j]
  x.df$WT0 <- WT0[j]
  x.df$ETHN <- ETHN[j]
  x.df$STUDY <- STUDY[j]
  if(i > length(DOSE)) x.df$PART <- 2 else x.df$PART <- 1
  
  if(is.null(x.all.df)){
    x.all.df <- x.df
  }else{
    x.all.df <- rbind(x.all.df, x.df)
  }
}

# IPRED = individual prediction (without residual error)
x.all.df$IPRED <- x.all.df$Concentration

# DV = with residual error
prop_err = rnorm(length(x.all.df$Concentration),0,0.3)
add_err = 0.05*rnorm(length(x.all.df$Concentration))
x.all.df$DV <- x.all.df$Concentration*(1 + prop_err) + add_err
x.all.df$DV[x.all.df$DV<0.05]=0.05
x.all.df$DV[x.all.df$Concentration==0]=NA
x.all.df$BLQ <- 0
x.all.df$BLQ[x.all.df$DV==0.05] <- 1

# these are the dosing events, set CMT to 1
my.data <- x.all.df[!is.na(x.all.df$amt),]
my.data$CMT_label <- "Dosing"
my.data$CMT <- 1

# these are the sampling events, set CMT to 2 for PK
temp <- x.all.df[is.na(x.all.df$amt),]
temp$CMT <- 2
temp$CMT_label <- "PK Concentration"
my.data <- rbind(my.data,temp)

#round
my.data$DV <- signif(my.data$DV,3)
my.data$WT0 <- signif(my.data$WT0,3)
my.data$time <- round(my.data$time,3)
my.data$IPRED <- signif(my.data$IPRED,3)

# order by ID, time and CMT
my.data <- my.data[order(my.data$ID,my.data$time, my.data$CMT),]

my.data$DOSE_label <- paste(my.data$DOSE,"mg")
my.data$DOSE_label[my.data$DOSE==0] <- "Placebo"
my.data$DOSE_label <- factor(my.data$DOSE_label,levels = c("Placebo",paste(unique(my.data$DOSE[my.data$DOSE!=0]),"mg")))

my.data$DAY <- floor(my.data$NT/24)+1
my.data$DAY_label <- paste("Day",ceiling(my.data$DAY))
my.data$DAY_label[my.data$DAY<=0]<-"Baseline"
my.data$DAY_label <- factor(my.data$DAY_label,
                            levels = c("Baseline",paste("Day",sort(unique(ceiling(my.data$DAY))))))
my.data$CYCLE <- my.data$DAY
my.data$CYCLE[my.data$CYCLE>6] <- 6
my.data[my.data$PART == 1,]$CYCLE <- 1

PKdataset <- my.data[,c("ID","time","NT","amt","DV","CMT","CMT_label","BLQ","evid","WT0","ETHN","DOSE_label","DOSE","DAY","DAY_label","CYCLE","PART","STUDY","IPRED")]

PKdataset2 <- PKdataset
PKdataset2$PROFTIME <- PKdataset2$NT - (PKdataset2$CYCLE-1)*24
PKdataset2$NOMTIME <- PKdataset2$NT
PKdataset2$YTYPE <- PKdataset2$CMT
PKdataset2$amt[is.na(PKdataset2$amt)] <- 0 
PKdataset2 <- PKdataset2[!(PKdataset2$DOSE_label=="Placebo"&
                             PKdataset2$CMT==2),] 
PKdataset2$BLQ[PKdataset2$DOSE_label=="Placebo"&
                 PKdataset2$CMT==2] <- 1L
PKdataset2$evid <- plyr::mapvalues(PKdataset2$evid, c(NA,101), c(0,1))
PKdataset2$TIMEUNIT <- "Hours"
PKdataset2$EVENTU[PKdataset2$CMT==1] <- "mg"
PKdataset2$EVENTU[PKdataset2$CMT==2] <- "ng/mL"

PKdataset2 <- PKdataset2[,c("ID","time","NOMTIME","TIMEUNIT","amt","DV","CMT",
                            "CMT_label","EVENTU","BLQ","evid","WT0","ETHN","DOSE_label",
                            "DOSE","DAY","PROFTIME","CYCLE","PART","STUDY","IPRED")]
names(PKdataset2) <- c("ID","TIME","NOMTIME","TIMEUNIT","AMT","LIDV","CMT",
                       "NAME","EVENTU","CENS","EVID","WEIGHTB","ETHN","TRTACT",
                       "DOSE","PROFDAY","PROFTIME","CYCLE","PART","STUDY","IPRED")

write.csv(PKdataset2,"./data/402_case2_PKdataset.csv",row.names = FALSE)

