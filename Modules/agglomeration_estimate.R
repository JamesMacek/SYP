
library(ggplot2)
library(readxl)
library(tidyverse)
library(estimatr)
library(matlab)
library(matlib)
library(simpleboot)
library(AER)
library(writexl)

theta=4

#Output elasticities 
phi_an = 0.04 
phi_aa = 0.16 
phi_na = 0.25 
phi_nn = 0.61 
land_na = 0.01
land_ag = 0.26
lab_na = 0.19
lab_ag = 0.27
cap_na = 0.16
cap_ag = 0.06
housing_spend = 0.13
goods_spend = 1 - housing_spend

setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")
master <- read_excel("constructed_output/master_stacked.xlsx")

#Constructing cost/ prince indicies in levels. Applying trade elasticity. 

master$cost[master$year == 2005] <- (master$cost[master$year == 2000]*master$dcost[master$year == 2005])^(1/theta)
master$cost[master$year == 2010] <- (master$dcost[master$year == 2010])^(1/theta)*master$cost[master$year == 2005]
master$cost[master$year == 2000] <- master$cost[master$year == 2000]^(1/theta)


master$pindex_na[master$year == 2005] <- (master$pindex_na[master$year == 2000]*master$dpindex_na[master$year == 2005])^(1/theta)
master$pindex_na[master$year == 2010] <- (master$dpindex_na[master$year == 2010 ])^(1/theta)*master$pindex_na[master$year == 2005]
master$pindex_ag[master$year == 2005] <- (master$pindex_ag[master$year == 2000]*master$dpindex_ag[master$year == 2005])^(1/theta)
master$pindex_ag[master$year == 2010] <- (master$dpindex_ag[master$year == 2010])^(1/theta)*master$pindex_ag[master$year == 2005]
master$pindex_ag[master$year == 2000] <- (master$pindex_ag[master$year == 2000])^(1/theta)
master$pindex_na[master$year == 2000] <- (master$pindex_na[master$year == 2000])^(1/theta)

master$dpindex_na <- master$dpindex_na^(1/theta)
master$dpindex_ag <- master$dpindex_ag^(1/theta)

master$dcost <- master$dcost^(1/theta)

#dropping international
master <- master[!master$province %in% "International",]

# Constructing output measures
master["dnomY"] <- rep(1, nrow(master))
master["dL"] <- rep(1, nrow(master))
master["dK"] <- rep(1, nrow(master))

master$dnomY[master$year == 2000] <- NA
master$dnomY[master$year == 2005] <- master$nomY[master$year == 2005]/master$nomY[master$year == 2000]
master$dnomY[master$year == 2010] <- master$nomY[master$year == 2010]/master$nomY[master$year == 2005]

master$dL[master$year == 2000] <- NA
master$dL[master$year == 2005] <- master$L[master$year == 2005]/master$L[master$year == 2000]
master$dL[master$year == 2010] <- master$L[master$year == 2010]/master$L[master$year == 2005]


master$dK[master$year == 2000] <- NA
master$dK[master$year == 2005] <- master$K[master$year == 2005]/master$K[master$year == 2000]
master$dK[master$year == 2010] <- master$K[master$year == 2010]/master$K[master$year == 2005]

#FE indicator variables
master["FE_2005"] <- ifelse(master$year == 2005, 1, 0)
master["FE_2010"] <- ifelse(master$year == 2010, 1, 0) #2010 FE for later. 

#change in nominal output
master["dOutput"] <- master$dnomY/master$dcost #deflating nominal gdp by output price, change in output 
master["Output"] <- master$nomY/master$cost

#Calculating land allocation (in levels) assuming perfect mobility of land across sectors within provinces. 
master["agGDP_ratio"] <- rep(1, nrow(master))
master$agGDP_ratio[master$sector == "ag"] <- master$nomY[master$sector == "ag"]/master$nomY[master$sector == "na"]
master$agGDP_ratio[master$sector == "na"] <- master$nomY[master$sector == "ag"]/master$nomY[master$sector == "na"]

master["commercial_land"] <- rep(1, nrow(master))
master$commercial_land[master$sector == "na"] <-  (master$landmass[master$sector == "na"]/1000)/(master$agGDP_ratio[master$sector == "na"]*(land_ag*lab_na)/(lab_ag*land_na) + (housing_spend/goods_spend)*(lab_na/land_na) + (housing_spend/goods_spend)*(lab_na/land_na)*master$agGDP_ratio[master$sector == "na"] + 1)/1000
master$commercial_land[master$sector == "ag"] <-  ((master$landmass[master$sector == "na"]/1000)*(master$agGDP_ratio[master$sector == "na"]*(land_ag*lab_na)/(lab_ag*land_na)))/(master$agGDP_ratio[master$sector == "na"]*(land_ag*lab_na)/(lab_ag*land_na) + (housing_spend/goods_spend)*(lab_na/land_na) + (housing_spend/goods_spend)*(lab_na/land_na)*master$agGDP_ratio[master$sector == "na"] + 1)

master["dcommercial_land"] <- rep(1, nrow(master))
master$dcommercial_land[master$year == 2000] <- NA
master$dcommercial_land[master$year == 2005] <- master$commercial_land[master$year == 2005]/master$commercial_land[master$year == 2000]
master$dcommercial_land[master$year == 2010] <- master$commercial_land[master$year == 2010]/master$commercial_land[master$year == 2005]

#Constructing TFP measures (net of input use, capital, "internalized" labour). This measure assumes land is immobile across sectors (for now!)
master["log_dTFP"] <- rep(1, nrow(master))
master["log_TFP"] <- rep(1, nrow(master))
master["log_lagTFP"] <- rep(1, nrow(master))

#Entering data for initial productivity to control for catch up growth/mean reversion across provinces. 
master$log_TFP <- rep(1, nrow(master))

master$log_TFP[master$sector == "ag"] <- log(master$Output[master$sector == "ag"]) - lab_ag*log(master$L[master$sector == "ag"]) - cap_ag*log(master$K[master$sector == "ag"]) - land_ag*log(master$commercial_land[master$sector == "ag"]) - #correcting for changes in labour and capital employment
                                          phi_aa*log(master$nomY[master$sector == "ag"]/master$pindex_ag[master$sector == "ag"]) - phi_na*log(master$nomY[master$sector == "ag"]/master$pindex_na[master$sector == "ag"]) 

master$log_TFP[master$sector == "na"] <- log(master$Output[master$sector == "na"]) - lab_na*log(master$L[master$sector == "na"]) - cap_na*log(master$K[master$sector == "na"]) - land_na*log(master$commercial_land[master$sector == "na"]) - #correcting for changes in labour and capital employment
                                          phi_an*log(master$nomY[master$sector == "na"]/master$pindex_ag[master$sector == "na"]) - phi_nn*log(master$nomY[master$sector == "na"]/master$pindex_na[master$sector == "na"])

#lagged values of log_TFP/ dlogTFP
master$log_dTFP[master$year == 2000] <- NA
master$log_dTFP[master$year == 2005] <- master$log_TFP[master$year == 2005] - master$log_TFP[master$year == 2000]
master$log_dTFP[master$year == 2010] <- master$log_TFP[master$year == 2010] - master$log_TFP[master$year == 2005]

master$log_lagTFP[master$year == 2000] <- NA
master$log_lagTFP[master$year == 2005] <- master$log_TFP[master$year == 2000]
master$log_lagTFP[master$year == 2010] <- master$log_TFP[master$year == 2005]
                                        
#generating employment density variables/lags for use later
master["L_density"] <- master$L/master$commercial_land

master["L_density_2000"] <- rep(NA, nrow(master))
master$L_density_2000[master$year == 2010] <- master$L_density[master$year == 2000]

master["dL_density"] <- master$dL/master$dcommercial_land

#Lag change in density controls if needed
master["dL_lagdensity"] <- rep(NA, nrow(master))
master$dL_lagdensity[master$year == 2010] <- master$dL_density[master$year == 2005]

#Changes from 2010 to 2000 variables
master["log_dTFP_2010"] <- rep(NA, nrow(master))
master$log_dTFP_2010[master$year == 2010] <- master$log_TFP[master$year == 2010]/master$log_TFP[master$year == 2000]
master["dL_density_2010"] <- rep(NA, nrow(master)) 
master$dL_density_2010[master$year == 2010] <- master$L_density[master$year == 2010]/master$L_density[master$year == 2000]

master["lagL_density"] <- rep(NA, nrow(master))
master$lagL_density[master$year == 2005] <- master$L[master$year == 2000]/master$commercial_land[master$year == 2000] 
master$lagL_density[master$year == 2010] <- master$L[master$year == 2005]/master$commercial_land[master$year == 2005]

master["log_dTFP_2010"] <- rep(NA, nrow(master)) 
master$log_dTFP_2010[master$year == 2010] <- master$log_TFP[master$year == 2010] - master$log_TFP[master$year == 2000]
master["log_lagTFP_2010"] <- rep(NA, nrow(master))
master$log_lagTFP_2010[master$year == 2010] <- master$log_TFP[master$year == 2000]


##___REGRESSIONS____

#Running regression in levels using entire sample!

ag_reg_levels_rob <- lm_robust(log_TFP~log(L_density) + FE_2005 + FE_2010, data = master[master$sector %in% "ag",], cluster = province, se_type = "stata")
na_reg_levels_rob <- lm_robust(log_TFP~log(L_density) + FE_2005 + FE_2010, data = master[master$sector %in% "na",], cluster = province, se_type = "stata") 

#Regression in changes from 2000-2010
ag_reg_2010_rob <- lm_robust(log_dTFP_2010 ~ log(dL_density_2010) + log_lagTFP_2010, data = master[master$sector %in% "ag",])
na_reg_2010_rob <- lm_robust(log_dTFP_2010 ~ log(dL_density_2010) + log_lagTFP_2010, data = master[master$sector %in% "na",]) 


#Outputting data on land prices for use with other programs
landprice <- (land_na/lab_na)*((1 - phi_an - phi_nn)*master$nomY[master$sector == "na"])/master$commercial_land[master$sector == "na"]
landprice <- data.frame(landprice)
landprice["province"] <- master$province[master$sector=="na"]
landprice["year"] <- master$year[master$sector == "na"]
landprice["Theta"] <- rep(theta, nrow(master[master$sector %in% "na",]))

write_xlsx(landprice, path = "constructed_output/landprice.xlsx")

