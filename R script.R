rm(list=ls())
#install.packages("urca")
#install.packages("timeDate")
#install.packages("timeSeries")
#install.packages("fBasics")
#install.packages("fUnitRoots")
#install.packages("fGarch")
#install.packages("fBasics")
#install.packages("tseries")
library("urca")
library("timeDate")
library("timeSeries")
library("fBasics")
library("fUnitRoots")
library("fGarch")
library("fBasics")
library("tseries")
setwd("/Users/stuartlee/Desktop/Project")
bpdr <- read.csv("Colombo prices daily orig war.csv")
#View(bpdr)
stnty_res_t <- matrix(data=NA,nrow=3,ncol=23)
stnty_res_p <- matrix(data=NA,nrow=3,ncol=23)
summary_stats <- matrix(data=NA,nrow=8,ncol=23)


a <- bpdr[1:1392,28]
c = basicStats(a)

#ADF Test
#adfTest(a,lags=28,type=("c"))
adftest = adf.test(a, alternative = c("stationary", "explosive"),
                   k = trunc((length(a)/100)^(1/4))*12)
stnty_res_t[1,i-1] = adftest$statistic
stnty_res_p[1,i-1] = adftest$p.value

#KPSS Test
kpsstest = kpss.test(a, null = c("Level", "Trend"), lshort = TRUE)
stnty_res_t[2,i-1] = kpsstest$statistic
stnty_res_p[2,i-1] = kpsstest$p.value

#PP test
pptest = pp.test(a, alternative = c("stationary", "explosive"),
                 type = c("Z(alpha)", "Z(t_alpha)"), lshort = TRUE) 
stnty_res_t[3,i-1] = pptest$statistic
stnty_res_p[3,i-1] = pptest$p.value



for (i in 2:24)
  {
   a <- bpdr[1:nrow(bpdr),i]
   
   # Summary Statistics
     b = a*100
     c = basicStats(b)
     summary_stats[1,i-1] = c[1,]  # No. Observations
     summary_stats[2,i-1] = c[7,]  # Mean
     summary_stats[3,i-1] = c[14,] # Standard Deviation
     summary_stats[4,i-1] = c[15,] # Skewness
     summary_stats[5,i-1] = c[16,]  # Excess Kurtosis 
     summary_stats[6,i-1] = c[3,]   # Minimum
     summary_stats[7,i-1] = c[4,]   # Maximum
     
     
     #summary_stats[8,i-1] = d = normalTest(a) Last slot will be for Jarque Bera Normality test
     
     
     
  #ADF Test
  #adfTest(a,lags=28,type=("c"))
  adftest = adf.test(a, alternative = c("stationary", "explosive"),
           k = trunc((length(a)/100)^(1/4))*12)
  stnty_res_t[1,i-1] = adftest$statistic
  stnty_res_p[1,i-1] = adftest$p.value
  
  #KPSS Test
  kpsstest = kpss.test(a, null = c("Level", "Trend"), lshort = TRUE)
  stnty_res_t[2,i-1] = kpsstest$statistic
  stnty_res_p[2,i-1] = kpsstest$p.value
  
  #PP test
  pptest = pp.test(a, alternative = c("stationary", "explosive"),
          type = c("Z(alpha)", "Z(t_alpha)"), lshort = TRUE) 
  stnty_res_t[3,i-1] = pptest$statistic
  stnty_res_p[3,i-1] = pptest$p.value
  
}


# Model 1
regression1 <- lm(a ~MonDum+TueDum+WedDum+ThuDum+FriDum-1,data=bpdr)
summary(regression1)

# Model 2
regression1 <- lm(a ~MonDum+TueDum+WedDum+ThuDum+FriDum+MSCI.World.Index-1,data=bpdr)
summary(regression1)

# Model 3
regression1 <- lm(a ~MonDum+TueDum+WedDum+ThuDum+FriDum+MonDum*MSCI.World.Index+TueDum*MSCI.World.Index+WedDum*MSCI.World.Index+ThuDum*MSCI.World.Index+FriDum*MSCI.World.Index-1,data=bpdr)
summary(regression1)

# Model 4
regression1 <- lm(a ~MonDum+TueDum+WedDum+ThuDum+FriDum+(MSCI.World.Index*Du)+(MSCI.World.Index*Dd)-1,data=bpdr)
summary(regression1)

# Model 5
regression1 <- lm(a ~MonDum+TueDum+WedDum+ThuDum+FriDum+Du*MonDum*MSCI.World.Index+TueDum*MSCI.World.Index+WedDum*MSCI.World.Index+ThuDum*MSCI.World.Index+FriDum*MSCI.World.Index-1,data=bpdr)
summary(regression1)

