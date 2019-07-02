##############################################################################
## Case example 1 Data generation
##############################################################################


library(dplyr)
library(RxODE)
library(caTools)


## PKPD model of lung administration with lung as site of action for efficacy

ode <- "
LungConc = depot/V1;
Concentration = centr/V2;
C3 = peri/V3;
d/dt(depot) = -KA*depot;
d/dt(centr) = KA*depot - (CL + Q)*Concentration + Q*C3;
d/dt(peri) =                    Q*Concentration - Q*C3;


d/dt(eff) = Kin*(1+ Emax*LungConc/(EC50 + LungConc)) - Kout*(1)*eff;
"

work <- tempfile("Rx_intro-")
mod1 <- RxODE(model = ode, modName = "mod1")

set.seed(173592)
ndose <- 6
nsub <- 30 # number of subjects per dose
nstud <- 1 # number of studies

WT0 <- runif(nsub*ndose*nstud,80,150)
eff0 <- runif(nsub*ndose*nstud,40,60)
VCOV <- 1
CLCOV <- 1

CL <- 15*exp(rnorm(nsub*ndose*nstud,0,.5))*CLCOV
KA <- 2*exp(rnorm(nsub*ndose*nstud,0,.16))
V1 <- 0.05*exp(rnorm(nsub*ndose*nstud,0,.4))
V2 <- 65*exp(rnorm(nsub*ndose*nstud,0,.4))*VCOV

Kout = 0.08
Kin <- eff0*Kout
PL <- 0.01+rnorm(nsub*ndose*nstud,0,0.04)
EMAX <- 400
DOSE <- rep(sort(rep(c(0,3,10,30,100,300),nsub)), nstud)
STUDY <- sort(rep(c(1:nstud), ndose*nsub))

theta.all <- 
  cbind(KA=KA,V1 = V1, CL=CL,  V2=V2,
        Q=7.4*CLCOV, V3=150*VCOV, 
        Kin=Kin, Kout=Kout, EC50 = 1000, PL = PL, Emax = EMAX, isPRED = 0, WT0 = WT0, eff0 = eff0, DOSE = DOSE)


# Nominal times for sampling schedule
SAMPLING <- c(c(-24,-0.1,0.1,0.5,1,2,4,8,12,18,23.9),-0.1+c(2,3,4,6,8,12)*7*24,
              84*24 + c(0.1,0.5,1,2,4,8,12,18,23.9))

# Nominal times for dosing schedule
DOSING <- seq(0,84*24,24)

# All nominal times
NT <- data.frame(NT = c(SAMPLING, DOSING), label = c(rep("SAMPLING",length(SAMPLING)),rep("DOSING",length(DOSING))))
NT <- NT[order(NT$NT),]

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
  
  j = i
  theta <- theta.all[j,]
  
  inits <- c(depot = 0, centr = 0, peri = 0, eff=as.numeric(theta["eff0"]))
  x <- mod1$solve(theta, ev.all[[i]], inits = inits)
  
  x.df <- data.frame(x)
  x.df$NT <- SAMPLING
  
  temp <- data.frame(ev.all[[i]]$get.dosing())
  temp$NT <- DOSING
  temp[,setdiff(names(x.df),names(temp))]<- NA
  x.df[,setdiff(names(temp),names(x.df))]<- NA
  x.df <- rbind(x.df,temp)
  
  x.df$ID <- ID
  x.df$DOSE <- DOSE[j]
  x.df$WT0 <- WT0[j]
  x.df$eff0 <- eff0[j]
  x.df$STUDY <- STUDY[j]
  x.df$PART <- 1
  
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
add_err = 0.01*rnorm(length(x.all.df$Concentration))
x.all.df$DV <- x.all.df$Concentration*(1 + prop_err) + add_err
x.all.df$DV[x.all.df$DV<0.05]=0.05
x.all.df$DV[x.all.df$Concentration==0]=NA
x.all.df$BLQ <- 0
x.all.df$BLQ[x.all.df$DV==0.05] <- 1

# include residual error in the effect compartment
add_err <- rnorm(length(x.all.df$eff),0,200)
# prop_err <- rnorm(length(x.all.df$eff),0,0.01)
x.all.df$eff2 <- x.all.df$eff*(1 + prop_err) + add_err
x.all.df$eff2.ipred <- x.all.df$eff

# these are the dosing events, set CMT to 1
my.data <- x.all.df[!is.na(x.all.df$amt),]
my.data$CMT_label <- "Dosing"
my.data$CMT <- 1

# these are the sampling events, set CMT to 2 for PK 
temp <- x.all.df[is.na(x.all.df$amt),]
temp$CMT_label <- "PK Concentration"
temp$CMT <- 2
my.data <- rbind(my.data,temp)

# set CMT to 3 for PD 
temp <- x.all.df[x.all.df$NT%in%c(-0.1,23.9,(23.9+seq(1,84)*24)),]
temp$DV <- temp$eff2
temp$IPRED <- temp$eff2.ipred
temp$CMT_label <- "PD - Continuous"
temp$CMT <- 3
temp <- temp[,names(my.data)]
my.data <- rbind(my.data,temp)

# round
my.data$DV <- signif(my.data$DV,5)
my.data$WT0 <- signif(my.data$WT0,5)
my.data$eff0 <- signif(my.data$eff0,5)
my.data$time <- round(my.data$time,5)
my.data$IPRED <- signif(my.data$IPRED,5)

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

PKPDdataset <- my.data[,c("ID","time","NT","amt","DV","CMT","CMT_label","BLQ","evid","WT0","eff0","DOSE_label","DOSE","DAY","DAY_label","CYCLE","PART","STUDY","IPRED")]

PKPDdataset2 <- PKPDdataset
PKPDdataset2$PROFTIME <- PKPDdataset2$NT - (PKPDdataset2$CYCLE-1)*24
PKPDdataset2$NOMTIME <- PKPDdataset2$NT
PKPDdataset2$YTYPE <- PKPDdataset2$CMT
PKPDdataset2$amt[is.na(PKPDdataset2$amt)] <- 0 
PKPDdataset2 <- PKPDdataset2[!(PKPDdataset2$DOSE_label=="Placebo"&
                                 PKPDdataset2$CMT==2),] 
PKPDdataset2$BLQ[PKPDdataset2$DOSE_label=="Placebo"&
                   PKPDdataset2$CMT==2] <- 1L
PKPDdataset2$evid <- plyr::mapvalues(PKPDdataset2$evid, c(NA,101), c(0,1))
PKPDdataset2$TIMEUNIT <- "Hours"
PKPDdataset2$EVENTU[PKPDdataset2$CMT==1] <- "mg"
PKPDdataset2$EVENTU[PKPDdataset2$CMT==2] <- "ng/mL"
PKPDdataset2$EVENTU[PKPDdataset2$CMT==3] <- "kg"

PKPDdataset2 <- PKPDdataset2[,c("ID","time","NOMTIME","TIMEUNIT","amt","DV","CMT",
                                "CMT_label","EVENTU","BLQ","evid","WT0","eff0","DOSE_label",
                                "DOSE","DAY","PROFTIME","CYCLE","PART","STUDY","IPRED")]
# rename some columns
names(PKPDdataset2) <- c("ID","TIME","NOMTIME","TIMEUNIT","AMT","LIDV","CMT",
                         "NAME","EVENTU","CENS","EVID","WEIGHTB","eff0","TRTACT",
                         "DOSE","PROFDAY","PROFTIME","CYCLE","PART","STUDY","IPRED")

# write to file
write.csv(PKPDdataset2,"data/401_case1_PKPDdataset.csv",row.names = FALSE)

########### Post-process data to calculate AUCs ########################
my_data <- PKPDdataset2

AUC24 <- my_data[my_data$CMT==2&!is.na(my_data$LIDV)&my_data$PART==1&my_data$STUDY==1&my_data$CYCLE==1,]
AUC24 <- unique(data.frame(stack(sapply(split(AUC24,AUC24$ID),function(df) trapz(df$TIME,df$LIDV)))))
names(AUC24) <- c("AUC","ID")

AUC24$ID <- as.numeric(as.character(AUC24$ID))
AUC24 <- AUC24[order(AUC24$ID),]
AUC24$CYCLE <- 1

AUCtau <- my_data[my_data$CMT==2&!is.na(my_data$LIDV)&my_data$PART==1&my_data$STUDY==1&my_data$CYCLE==85,]
AUCtau <- unique(data.frame(stack(sapply(split(AUCtau,AUCtau$ID),function(df) trapz(df$TIME,df$LIDV)))))
names(AUCtau) <- c("AUC","ID")

AUCtau$ID <- as.numeric(as.character(AUCtau$ID))
AUCtau <- AUCtau[order(AUCtau$ID),]
AUCtau$CYCLE <- 85

PDendpoint <- subset(my_data, CMT==3&!is.na(LIDV)&PART==1&STUDY==1&CYCLE==85) %>%
  mutate(eff = LIDV, CHG=eff-eff0, PCHG = (eff- eff0)/eff0) %>% 
  select(eff,CHG,PCHG,ID)

AUCs <- rbind(AUC24, AUCtau, by = "ID")
AUCs <- merge(AUCs, PDendpoint, by = "ID")
AUCs <- merge(AUCs, unique(subset(my_data,,c("ID","DOSE","eff0"))))
AUCs$AUC <- as.numeric(AUCs$AUC)

AUCs <- AUCs %>% mutate(sCHG = CHG)

# write to file
write.csv(AUCs,"data/401_case1_PKPDdataset_ard.csv",row.names = FALSE)
