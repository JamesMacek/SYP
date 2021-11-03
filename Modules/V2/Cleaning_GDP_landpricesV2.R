#Date created: October 8th  2021
#Date modified: October 8th 2021

library(ggplot2)
library(readxl)
library(writexl)
library(tidyverse)
library(R.matlab)
#This file further constructs data, applying consistent normalization to GDP and constructing land prices.
#Under various assumptions.

#Directory
setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")

#Reading parameters from master file, putting into data frame
parameters <- readMat('constructed_outputV2/parameters.mat')
parameters <- parameters$parameters
parameters_rownames = rownames(parameters)
parameters <- unlist(parameters)
parameters <- data.frame(parameters, row.names = parameters_rownames)

#Loading parameters
phi_an = parameters["phi.an", "parameters"]
phi_aa = parameters["phi.aa", "parameters"]
phi_na = parameters["phi.na", "parameters"]
phi_nn = parameters["phi.nn", "parameters"] 
vashare_n = parameters["vashare.n", "parameters"]
vashare_a = parameters["vashare.a", "parameters"]
land_na = parameters["land.na", "parameters"]
land_ag = parameters["land.ag", "parameters"]
lab_na = parameters["lab.na", "parameters"]
lab_ag = parameters["lab.ag", "parameters"]
#Housing consumption
housing_spend = parameters["nu", "parameters"]
goods_spend = 1 - housing_spend


#Loading in data

master <- read_excel("master_raw_stacked.xlsx")
master_unstacked <- read_excel("master_raw.xlsx")

#PART 1: Choosing new normalization

#Instead, normalize GDP so that geometric value added per worker across Chinese provinces is 1 in all years. 
#This normalization is to ensure only price changes when calculating the spending share on agriculture
master["Va_perworker"] <- rep(NA, nrow(master))
master$Va_perworker[master$province != "International"] <- master$nomY[master$province != "International"]/master$L[master$province != "International"]

master$nomY[master$province == "International" & master$year == 2000] <- master$nomY[master$province == "International" & master$year == 2000]/exp(mean(log(master$Va_perworker[master$year == 2000 & master$province != "International"])))
master$Va_perworker[master$year == 2000 & master$province != "International"] <- master$Va_perworker[master$year == 2000 & master$province != "International"]/exp(mean(log(master$Va_perworker[master$year == 2000 & master$province != "International"])))

master$nomY[master$province == "International" & master$year == 2005] <- master$nomY[master$province == "International" & master$year == 2005]/exp(mean(log(master$Va_perworker[master$year == 2005 & master$province != "International"])))
master$Va_perworker[master$year == 2005 & master$province != "International"] <- master$Va_perworker[master$year == 2005 & master$province != "International"]/exp(mean(log(master$Va_perworker[master$year == 2005 & master$province != "International"])))

master$nomY[master$province == "International" & master$year == 2010] <- master$nomY[master$province == "International" & master$year == 2010]/exp(mean(log(master$Va_perworker[master$year == 2010 & master$province != "International"])))
master$Va_perworker[master$year == 2010 & master$province != "International"] <- master$Va_perworker[master$year == 2010 & master$province != "International"]/exp(mean(log(master$Va_perworker[master$year == 2010 & master$province != "International"])))

#Expressing nomY in terms of gross output (useful for calculating counterfactual)!!!!!! MAKE NOTE OF THIS.
master$nomY[master$sector == "na" & master$province != "International"] <- master$Va_perworker[master$sector == "na" & master$province != "International"]*master$L[master$sector == "na" & master$province != "International"]/(vashare_n)
master$nomY[master$sector == "ag" & master$province != "International"] <- master$Va_perworker[master$sector == "ag" & master$province != "International"]*master$L[master$sector == "ag" & master$province != "International"]/(vashare_a)
master$nomY[master$sector == "na" & master$province == "International"] <- master$nomY[master$sector == "na" & master$province == "International"]/vashare_n
master$nomY[master$sector == "ag" & master$province == "International"] <- master$nomY[master$sector == "ag" & master$province == "International"]/vashare_a

#Testing: do relative values agree with new normalization?
master_test <- read_excel("master_raw_stacked.xlsx")
test <- master$nomY/master_test$nomY #Test passes-- equal within sector and year

#Constructing measures in changes for use with other programs
# 0) Constructing input measures in changes
master["dnomY"] <- rep(NA, nrow(master))
master["dL"] <- rep(NA, nrow(master))
master["dVA"] <- rep(NA, nrow(master))

master$dnomY[master$year == 2000] <- NA
master$dnomY[master$year == 2005] <- master$nomY[master$year == 2005]/master$nomY[master$year == 2000]
master$dnomY[master$year == 2010] <- master$nomY[master$year == 2010]/master$nomY[master$year == 2005]

master$dL[master$year == 2000] <- NA
master$dL[master$year == 2005] <- master$L[master$year == 2005]/master$L[master$year == 2000]
master$dL[master$year == 2010] <- master$L[master$year == 2010]/master$L[master$year == 2005]

master$dVA[master$year == 2000] <- NA
master$dVA[master$year == 2005] <- master$Va_perworker[master$year == 2005]/master$Va_perworker[master$year == 2000]
master$dVA[master$year == 2010] <- master$Va_perworker[master$year == 2010]/master$Va_perworker[master$year == 2005]

#Outputting new dataset for use with other programs
write_xlsx(master, path = "constructed_outputV2/master_stacked.xlsx")

#Part 2: Constructing land prices under different assumptions

#2.1: If land were perfectly mobile across sectors and consumption
master["agGDP_ratio"] <- rep(1, nrow(master)) 
master$agGDP_ratio[master$sector == "ag"] <- master$nomY[master$sector == "ag"]/master$nomY[master$sector == "na"]
master$agGDP_ratio[master$sector == "na"] <- master$nomY[master$sector == "ag"]/master$nomY[master$sector == "na"]
master["commercial_land"] <- rep(1, nrow(master))
master["residential_land"] <- rep(1, nrow(master))
master$landmass[master$province == "International"] <- rep(1, nrow(master[master$province %in% "International",])) #Placeholder to apply following calculation
master$commercial_land[master$sector == "na"] <-  (master$landmass[master$sector == "na"]/1000)/(master$agGDP_ratio[master$sector == "na"]*(land_ag/land_na) + (housing_spend/goods_spend)*(1/land_na)*(1-phi_an - phi_nn) + (housing_spend/goods_spend)*(1/land_na)*(1-phi_aa - phi_na)*master$agGDP_ratio[master$sector == "na"] + 1)
master$commercial_land[master$sector == "ag"] <-  ((master$landmass[master$sector == "na"]/1000)*(master$agGDP_ratio[master$sector == "na"]*(land_ag/land_na)))/(master$agGDP_ratio[master$sector == "na"]*(land_ag/land_na) + (housing_spend/goods_spend)*(1/land_na)*(1-phi_an - phi_nn) + (housing_spend/goods_spend)*(1/land_na)*(1-phi_aa - phi_na)*master$agGDP_ratio[master$sector == "na"] + 1)
master$residential_land[master$sector == "na"] <-  ((master$landmass[master$sector == "na"]/1000)*((housing_spend/goods_spend)*(1/land_na)*(1-phi_an - phi_nn)))/(master$agGDP_ratio[master$sector == "na"]*(land_ag/land_na) + (housing_spend/goods_spend)*(1/land_na)*(1-phi_an - phi_nn) + (housing_spend/goods_spend)*(1/land_na)*(1-phi_aa - phi_na)*master$agGDP_ratio[master$sector == "na"] + 1)
master$residential_land[master$sector == "ag"] <-  ((master$landmass[master$sector == "na"]/1000)*((housing_spend/goods_spend)*(1/land_na)*(1-phi_aa - phi_na)*master$agGDP_ratio[master$sector == "na"]))/(master$agGDP_ratio[master$sector == "na"]*(land_ag/land_na) + (housing_spend/goods_spend)*(1/land_na)*(1-phi_an - phi_nn) + (housing_spend/goods_spend)*(1/land_na)*(1-phi_aa - phi_na)*master$agGDP_ratio[master$sector == "na"] + 1)
  #Deleting placeholder for missing value
master$landmass[master$province == "International"] <- rep(NA, nrow(master[master$province %in% "International",])) 
master$commercial_land[master$province == "International"] <- rep(NA, nrow(master[master$province %in% "International",]))
master$residential_land[master$province == "International"] <- rep(NA, nrow(master[master$province %in% "International",])) 

#Outputting/cleaning data on change in land prices/use for use with other programs.
landprice <- (land_na)*master$nomY[master$sector == "na"]/master$commercial_land[master$sector == "na"]

#Test: does the marginal product of land equalize?
test_landprice_na <- (land_na)*master$nomY[master$sector == "na"]/master$commercial_land[master$sector == "na"]
test_landprice_ag <- (land_ag)*master$nomY[master$sector == "ag"]/master$commercial_land[master$sector == "ag"] #Test passed. 

#Cleaning and exporting dataframe
landprice <- data.frame(landprice)
landprice["ag_commercial_land"] <- master$commercial_land[master$sector == "ag"]
landprice["na_commercial_land"] <- master$commercial_land[master$sector == "na"]
landprice["ag_residential_land"] <- master$residential_land[master$sector == "ag"]
landprice["na_residential_land"] <- master$residential_land[master$sector == "na"]
landprice["landmass"] <- landprice$ag_commercial_land + landprice$na_commercial_land + landprice$ag_residential_land + landprice$na_residential_land #Sums to original land use patterns
landprice["province"] <- master$province[master$sector=="na"]
landprice["year"] <- master$year[master$sector == "na"]
landprice["dlandprice"] <- rep(NA, nrow(landprice))
landprice$dlandprice[landprice$year == 2005] <- landprice$landprice[landprice$year==2005]/landprice$landprice[landprice$year==2000]
landprice$dlandprice[landprice$year == 2010] <- landprice$landprice[landprice$year==2010]/landprice$landprice[landprice$year==2005]
write_xlsx(landprice[!master$province %in% "International",], path = "constructed_outputV2/landprice_mobile.xlsx")

#2.2: If land adjusted according to data proxy levels.
#TBD. 


#Part 3 Updating master_raw with new data. 
#Nominal gross output
master_unstacked$nomY_ag_2000 <- master$nomY[master$sector == "ag" & master$year == 2000]
master_unstacked$nomY_na_2000 <- master$nomY[master$sector == "na" & master$year == 2000]
master_unstacked$nomY_ag_2005 <- master$nomY[master$sector == "ag" & master$year == 2005]
master_unstacked$nomY_na_2005 <- master$nomY[master$sector == "na" & master$year == 2005]
master_unstacked$nomY_ag_2010 <- master$nomY[master$sector == "ag" & master$year == 2010]
master_unstacked$nomY_na_2010 <- master$nomY[master$sector == "na" & master$year == 2010]

#Change in gross output
master_unstacked["dnomY_ag_2005"] <- master$dnomY[master$sector == "ag" & master$year == 2005]
master_unstacked["dnomY_na_2005"] <- master$dnomY[master$sector == "na" & master$year == 2005]
master_unstacked["dnomY_ag_2010"] <- master$dnomY[master$sector == "ag" & master$year == 2010]
master_unstacked["dnomY_na_2010"] <- master$dnomY[master$sector == "na" & master$year == 2010]

#Change in equilibrium labour 
master_unstacked["dL_ag_2005"] <- master$dL[master$sector == "ag" & master$year == 2005]
master_unstacked["dL_na_2005"] <- master$dL[master$sector == "na" & master$year == 2005]
master_unstacked["dL_ag_2010"] <- master$dL[master$sector == "ag" & master$year == 2010]
master_unstacked["dL_na_2010"] <- master$dL[master$sector == "na" & master$year == 2010]

#dValue added per worker
master_unstacked["dVA_ag_2005"] <-  master$dVA[master$sector == "ag" & master$year == 2005]
master_unstacked["dVA_na_2005"] <-  master$dVA[master$sector == "na" & master$year == 2005]
master_unstacked["dVA_ag_2010"] <-  master$dVA[master$sector == "ag" & master$year == 2010]
master_unstacked["dVA_na_2010"] <-  master$dVA[master$sector == "na" & master$year == 2010]

#In levels
master_unstacked["Va_ag_2000"] <- master$Va_perworker[master$sector == "ag" & master$year == 2000]
master_unstacked["Va_na_2000"] <- master$Va_perworker[master$sector == "na" & master$year == 2000]
master_unstacked["Va_ag_2005"] <- master$Va_perworker[master$sector == "ag" & master$year == 2005]
master_unstacked["Va_na_2005"] <- master$Va_perworker[master$sector == "na" & master$year == 2005]
master_unstacked["Va_ag_2010"] <- master$Va_perworker[master$sector == "ag" & master$year == 2010]
master_unstacked["Va_na_2010"] <- master$Va_perworker[master$sector == "na" & master$year == 2010]

#mobile land prices
master_unstacked["dLandprice_mobile_2005"] <- landprice$landprice[landprice$year==2005]/landprice$landprice[landprice$year==2000]

#and outputting
write_xlsx(master_unstacked, path = "constructed_outputV2/master.xlsx")
