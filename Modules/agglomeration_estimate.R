
library(ggplot2)
library(readxl)
library(tidyverse)
library(estimatr)
library(matlab)
library(matlib)
library(simpleboot)
library(AER)
library(writexl)

#This file runs regressions for understanding the relationship between TFP and employment density.


theta=4

#Output elasticities 
phi_an = 0.04 
phi_aa = 0.16 
phi_na = 0.25 
phi_nn = 0.61 
vashare_n = 1-phi_an - phi_nn
vashare_a = 1-phi_aa - phi_na
land_na = 0.01
land_ag = 0.26
lab_na = 0.19
lab_ag = 0.27
cap_na = 0.15
cap_ag = 0.06
housing_spend = 0.13
goods_spend = 1 - housing_spend

setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")

#read output Construct_....R and rel_prod_estimate.m
master <- read_excel("constructed_output/master_stacked2.xlsx")

#Dropping international observation
master <- master[!master$province %in% "International",]


#FE indicator variables
master["FE_2005"] <- ifelse(master$year == 2005, 1, 0)
master["FE_2010"] <- ifelse(master$year == 2010, 1, 0) #2010 FE for later. 


#Constructing TFP measures (net of input use, capital, "internalized" labour).
master["log_dTFP"] <- rep(1, nrow(master))
master["log_TFP"] <- rep(NA, nrow(master))
master["log_lagTFP"] <- rep(1, nrow(master))

#Constructing log_TFP in levels for 2000 (note 2000 costs are set so that they average to 1 in rel_prod_estimate.m) 
master$log_TFP[master$sector == "na" & master$year == 2000] <- log((master$nomY[master$sector == "na" & master$year == 2000]^(vashare_n)*master$pindex_na[master$sector == "na" & master$year == 2000]^(phi_nn)*master$pindex_ag[master$sector == "na" & master$year == 2000]^(phi_an)/(master$L[master$sector == "na" & master$year == 2000]^(lab_na)*master$K[master$sector == "na" & master$year == 2000]^(cap_na)*master$commercial_land[master$sector == "na" & master$year == 2000]^(land_na)))/master$cost[master$sector == "na" & master$year == 2000])
master$log_TFP[master$sector == "ag" & master$year == 2000] <- log((master$nomY[master$sector == "ag" & master$year == 2000]^(vashare_a)*master$pindex_na[master$sector == "ag" & master$year == 2000]^(phi_na)*master$pindex_ag[master$sector == "ag" & master$year == 2000]^(phi_aa)/(master$L[master$sector == "ag" & master$year == 2000]^(lab_ag)*master$K[master$sector == "ag" & master$year == 2000]^(cap_ag)*master$commercial_land[master$sector == "ag" & master$year == 2000]^(land_ag)))/master$cost[master$sector == "ag" & master$year == 2000])
master$log_TFP[master$year == 2005] <- master$log_TFP[master$year == 2000] + log(master$dTFP[master$year == 2005])
master$log_TFP[master$year == 2010] <- master$log_TFP[master$year == 2005] + log(master$dTFP[master$year == 2010])
master$log_dTFP <- log(master$dTFP)


master$log_lagTFP[master$year == 2000] <- NA
master$log_lagTFP[master$year == 2005] <- master$log_TFP[master$year == 2000]
master$log_lagTFP[master$year == 2010] <- master$log_TFP[master$year == 2005]

master["log_dTFP_2010"] <- rep(NA, nrow(master)) 
master$log_dTFP_2010[master$year == 2010] <- master$log_TFP[master$year == 2010] - master$log_TFP[master$year == 2000]
master["log_lagTFP_2010"] <- rep(NA, nrow(master))
master$log_lagTFP_2010[master$year == 2010] <- master$log_TFP[master$year == 2000]

                                        
#generating employment density variables/lags for use later
master["L_density"] <- master$L/master$commercial_land

master["L_density_2000"] <- rep(NA, nrow(master))
master$L_density_2000[master$year == 2010] <- master$L_density[master$year == 2000]

master["dL_density"] <- master$dL/master$dcommercial_land

#Lag change in density controls if needed
master["dL_lagdensity"] <- rep(NA, nrow(master))
master$dL_lagdensity[master$year == 2010] <- master$dL_density[master$year == 2005]

#Changes from 2010 to 2000 variables
master["dL_density_2010"] <- rep(NA, nrow(master)) 
master$dL_density_2010[master$year == 2010] <- master$L_density[master$year == 2010]/master$L_density[master$year == 2000]

master["lagL_density"] <- rep(NA, nrow(master))
master$lagL_density[master$year == 2005] <- master$L[master$year == 2000]/master$commercial_land[master$year == 2000] 
master$lagL_density[master$year == 2010] <- master$L[master$year == 2005]/master$commercial_land[master$year == 2005]

##___REGRESSIONS____

#Running regression in levels using entire sample!

ag_reg_levels_rob <- lm_robust(log_TFP~log(L_density) + FE_2005 + FE_2010, data = master[master$sector %in% "ag",], cluster = province, se_type = "stata")
na_reg_levels_rob <- lm_robust(log_TFP~log(L_density) + FE_2005 + FE_2010, data = master[master$sector %in% "na",], cluster = province, se_type = "stata") 

#Regression in changes from 2000-2010
ag_reg_2010_rob <- lm_robust(log_dTFP_2010 ~ log(dL_density_2010) + log_lagTFP_2010, data = master[master$sector %in% "ag",])
na_reg_2010_rob <- lm_robust(log_dTFP_2010 ~ log(dL_density_2010) + log_lagTFP_2010, data = master[master$sector %in% "na",]) 

#massive identification issues!



