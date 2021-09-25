#Date created: June 12th 2021
#Date modified: July 6th 2021

library(ggplot2)
library(readxl)
library(writexl)
library(tidyverse)
#This file further constructs data, applying consistent normalization to GDP and 
#Correcting output from rel_prod_estimate.m by the trade elasticity "theta".

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
#Housing consumption
housing_spend = 0.13
goods_spend = 1 - housing_spend

theta = 4

#REQUIRES RUNNING relative_productivity_estimate.m, uses its output:
setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")

master <- read_excel("constructed_output/master_stacked.xlsx")
master_unstacked <- read_excel("constructed_output/master.xlsx")
tr_ag_2002 <- read_excel("constructed_output/HRtau_ag_2002.xlsx")
tr_ag_2007 <- read_excel("constructed_output/HRtau_ag_2007.xlsx")
tr_ag_2012 <- read_excel("constructed_output/HRtau_ag_2012.xlsx")
tr_na_2002 <- read_excel("constructed_output/HRtau_na_2002.xlsx")
tr_na_2007 <- read_excel("constructed_output/HRtau_na_2007.xlsx")
tr_na_2012 <- read_excel("constructed_output/HRtau_na_2012.xlsx")

flow_ag_2002 <- read_excel("2002_ag_tradeflows.xlsx")
flow_ag_2002 <- flow_ag_2002[, -1]
flow_ag_2007 <- read_excel("2007_ag_tradeflows.xlsx")
flow_ag_2007 <- flow_ag_2007[, -1]


flow_na_2002 <- read_excel("2002_na_tradeflows.xlsx")
flow_na_2002 <- flow_na_2002[, -1]
flow_na_2007 <- read_excel("2007_na_tradeflows.xlsx")
flow_na_2007 <- flow_na_2007[, -1]


#PART 1: Applying normalizations/corrections to master_stacked data
#Applying trade elasticity to the data 
master$cost[master$year == 2000] <- master$cost[master$year == 2000]^(1/theta)
master$dcost <- master$dcost^(1/theta)

master$pindex_ag[master$year == 2000] <- (master$pindex_ag[master$year == 2000])^(1/theta)
master$pindex_na[master$year == 2000] <- (master$pindex_na[master$year == 2000])^(1/theta)

master$dpindex_na <- master$dpindex_na^(1/theta)
master$dpindex_ag <- master$dpindex_ag^(1/theta)


#First: choosing a new normalization
#Instead, normalize GDP so that average value added per worker across Chinese provinces is 1 in all years (keeping a consistent unit of account -- no "inflation" induced by artificially changing the unit of account)
master["Va_perworker"] <- rep(NA, nrow(master))
master$Va_perworker[master$province != "International"] <- master$nomY[master$province != "International"]/master$L[master$province != "International"]

master$nomY[master$province == "International" & master$year == 2000] <- 60*master$nomY[master$province == "International" & master$year == 2000]/sum(master$Va_perworker[master$year == 2000 & master$province != "International"])
master$Va_perworker[master$year == 2000 & master$province != "International"] <- 60*master$Va_perworker[master$year == 2000 & master$province != "International"]/sum(master$Va_perworker[master$year == 2000 & master$province != "International"])

master$nomY[master$province == "International" & master$year == 2005] <- 60*master$nomY[master$province == "International" & master$year == 2005]/sum(master$Va_perworker[master$year == 2005 & master$province != "International"])
master$Va_perworker[master$year == 2005 & master$province != "International"] <- 60*master$Va_perworker[master$year == 2005 & master$province != "International"]/sum(master$Va_perworker[master$year == 2005 & master$province != "International"])

master$nomY[master$province == "International" & master$year == 2010] <- 60*master$nomY[master$province == "International" & master$year == 2010]/sum(master$Va_perworker[master$year == 2010 & master$province != "International"])
master$Va_perworker[master$year == 2010 & master$province != "International"] <- 60*master$Va_perworker[master$year == 2010 & master$province != "International"]/sum(master$Va_perworker[master$year == 2010 & master$province != "International"])

#Expressing nomY in terms of gross output
master$nomY[master$sector == "na" & master$province != "International"] <- master$Va_perworker[master$sector == "na" & master$province != "International"]*master$L[master$sector == "na" & master$province != "International"]/(vashare_n)
master$nomY[master$sector == "ag" & master$province != "International"] <- master$Va_perworker[master$sector == "ag" & master$province != "International"]*master$L[master$sector == "ag" & master$province != "International"]/(vashare_a)
master$nomY[master$sector == "na" & master$province == "International"] <- master$nomY[master$sector == "na" & master$province == "International"]/vashare_n
master$nomY[master$sector == "ag" & master$province == "International"] <- master$nomY[master$sector == "ag" & master$province == "International"]/vashare_a

#Calculating land allocation (in levels) assuming perfect mobility of land across sectors (including residential) within provinces. Used to infer real GDP gaps when they include residential consumption. 
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

master$landmass[master$province == "International"] <- rep(NA, nrow(master[master$province %in% "International",])) 
master$commercial_land[master$province == "International"] <- rep(NA, nrow(master[master$province %in% "International",])) 

#Outputting data on land prices/use for use with other programs.
landprice <- (land_na)*master$nomY[master$sector == "na"]/master$commercial_land[master$sector == "na"]
landprice <- data.frame(landprice)
landprice["ag_commercial_land"] <- master$commercial_land[master$sector == "ag"]
landprice["na_commercial_land"] <- master$commercial_land[master$sector == "na"]
landprice["ag_residential_land"] <- master$residential_land[master$sector == "ag"]
landprice["na_residential_land"] <- master$residential_land[master$sector == "na"]
landprice["landmass"] <- landprice$ag_commercial_land + landprice$na_commercial_land + landprice$ag_residential_land + landprice$na_residential_land

landprice["province"] <- master$province[master$sector=="na"]
landprice["year"] <- master$year[master$sector == "na"]
write_xlsx(landprice[!master$province %in% "International",], path = "constructed_output/landuse_constructed.xlsx")

#Solving for average productivity given measures of land, measures of wages and measures of capital in the data.

# 0) Constructing input measures in changes
master["dnomY"] <- rep(NA, nrow(master))
master["dL"] <- rep(NA, nrow(master))

master["dK"] <- rep(NA, nrow(master))
master["dcommercial_land"] <- rep(NA, nrow(master))

master$dnomY[master$year == 2000] <- NA
master$dnomY[master$year == 2005] <- master$nomY[master$year == 2005]/master$nomY[master$year == 2000]
master$dnomY[master$year == 2010] <- master$nomY[master$year == 2010]/master$nomY[master$year == 2005]

master$dL[master$year == 2000] <- NA
master$dL[master$year == 2005] <- master$L[master$year == 2005]/master$L[master$year == 2000]
master$dL[master$year == 2010] <- master$L[master$year == 2010]/master$L[master$year == 2005]


master$dK[master$year == 2000] <- NA
master$dK[master$year == 2005] <- master$K[master$year == 2005]/master$K[master$year == 2000]
master$dK[master$year == 2010] <- master$K[master$year == 2010]/master$K[master$year == 2005]


master$dcommercial_land[master$year == 2000] <- NA
master$dcommercial_land[master$year == 2005] <- master$commercial_land[master$year == 2005]/master$commercial_land[master$year == 2000]
master$dcommercial_land[master$year == 2010] <- master$commercial_land[master$year == 2010]/master$commercial_land[master$year == 2005]

#Immobile land, labour, etc for international province. Assume no capital accumulation (absorb all labour productivity growth into TFP term)
master$dK[master$province == "International" & master$year != 2000] <- rep(1, nrow(master[master$province %in% "International" & !master$year %in% 2000, ]))
master$dL[master$province == "International" & master$year != 2000] <- rep(1, nrow(master[master$province %in% "International" & !master$year %in% 2000, ]))
master$dcommercial_land[master$province == "International" & master$year != 2000] <- rep(1, nrow(master[master$province %in% "International" & !master$year %in% 2000,]))

#1) Solving for normalized productivity growth from 2000-2005.
master["dTFP"] <- rep(NA, nrow(master))

master$dTFP[master$sector == "ag"] <- (master$dnomY[master$sector == "ag"]^(vashare_a)*master$dpindex_na[master$sector == "ag"]^(phi_na)*master$dpindex_ag[master$sector == "ag"]^(phi_aa)/(master$dL[master$sector == "ag"]^(lab_ag)*master$dK[master$sector == "ag"]^(cap_ag)*master$dcommercial_land[master$sector == "ag"]^(land_ag)*master$dcost[master$sector == "ag"]))
#renormalizing so that average productivity growth in china is one
master$dTFP[master$sector == "ag" & master$year == 2005 & master$province == "International"] <- master$dTFP[master$sector == "ag" & master$year == 2005 & master$province == "International"]*30/sum(master$dTFP[master$sector == "ag" & master$year == 2005 & master$province!= "International"])
master$dTFP[master$sector == "ag" & master$year == 2005 & master$province!= "International"] <- master$dTFP[master$sector == "ag" & master$year == 2005 & master$province!= "International"]*30/sum(master$dTFP[master$sector == "ag" & master$year == 2005 & master$province!= "International"])
master$dTFP[master$sector == "ag" & master$year == 2010 & master$province == "International"] <- master$dTFP[master$sector == "ag" & master$year == 2010 & master$province == "International"]*30/sum(master$dTFP[master$sector == "ag" & master$year == 2010 & master$province!= "International"])
master$dTFP[master$sector == "ag" & master$year == 2010 & master$province!= "International"] <- master$dTFP[master$sector == "ag" & master$year == 2010 & master$province!= "International"]*30/sum(master$dTFP[master$sector == "ag" & master$year == 2010 & master$province!= "International"])


master$dTFP[master$sector == "na"] <- (master$dnomY[master$sector == "na"]^(vashare_n)*master$dpindex_na[master$sector == "na"]^(phi_nn)*master$dpindex_ag[master$sector == "na"]^(phi_an)/(master$dL[master$sector == "na"]^(lab_na)*master$dK[master$sector == "na"]^(cap_na)*master$dcommercial_land[master$sector == "na"]^(land_na)*master$dcost[master$sector == "na"]))
#re-normalizing so that average productivity growth in china is one
master$dTFP[master$sector == "na" & master$year == 2005 & master$province == "International"] <- master$dTFP[master$sector == "na" & master$year == 2005 & master$province == "International"]*30/sum(master$dTFP[master$sector == "na" & master$year == 2005 & master$province!= "International"])
master$dTFP[master$sector == "na" & master$year == 2005 & master$province!= "International"] <- master$dTFP[master$sector == "na" & master$year == 2005 & master$province!= "International"]*30/sum(master$dTFP[master$sector == "na" & master$year == 2005 & master$province!= "International"])
master$dTFP[master$sector == "na" & master$year == 2010 & master$province == "International"] <- master$dTFP[master$sector == "na" & master$year == 2010 & master$province == "International"]*30/sum(master$dTFP[master$sector == "na" & master$year == 2010 & master$province!= "International"])
master$dTFP[master$sector == "na" & master$year == 2010 & master$province!= "International"] <- master$dTFP[master$sector == "na" & master$year == 2010 & master$province!= "International"]*30/sum(master$dTFP[master$sector == "na" & master$year == 2010 & master$province!= "International"])

#Getting new measure of costs after productivity normalization. Involves solving nonlinear equation
cnorm_2005 = 1
cnorm_2010 = 1
c_na_2005 = rep(1, 31)
c_na_2005_new = c_na_2005
c_na_2010 = rep(1, 31)
c_na_2010_new = c_na_2005
c_ag_2005 = rep(1, 31)
c_ag_2005_new = c_na_2005
c_ag_2010 = rep(1, 31)
c_ag_2010_new = c_na_2005

pindex_ag <- function(x, year) { #argument : column vector of costs in non-ag sector. Output: vector of new price index in nonag
  
  if (year == 2005) {
    tr2 = tr_ag_2007
    tr1 = tr_ag_2002
    trf = flow_ag_2002
  }
  if (year == 2010) {
    tr2 = tr_ag_2012
    tr1 = tr_ag_2007
    trf = flow_ag_2007
  }
  
  
  c <- (as.matrix((tr1/tr2)*trf))%*%x^(-theta) #Correcting for trade elasticity, summing up market access terms. 
  c <- c^(-1/theta) #Correcting for trade elasticity in price index. 
  return(c)
}

pindex_na <- function(x, year) { #argument : column vector of costs in non-ag sector. Output: vector of new price index in nonag
  
  if (year == 2005) {
    tr2 = tr_na_2007
    tr1 = tr_na_2002
    trf = flow_na_2002
  }
  if (year == 2010) {
    tr2 = tr_na_2012
    tr1 = tr_na_2007
    trf = flow_na_2007
  }
  
  c <- (as.matrix((tr1/tr2)*trf))%*%x^(-theta) #Correcting for trade elasticity
  c <- c^(-1/theta) #Correcting for trade elasticity in price index. 
  return(c)
}

#Setting up while loop for solution
while (cnorm_2005 > 0.0000001 & cnorm_2010 > 0.0000001) {
  
c_na_2005_new = (master$dnomY[master$sector == "na" & master$year == 2005]^(vashare_n)/(master$dL[master$sector == "na" & master$year == 2005]^(lab_na)*master$dK[master$sector == "na" & master$year == 2005]^(cap_na)*master$dcommercial_land[master$sector == "na" & master$year == 2005]^(land_na)))
c_na_2005_new = c_na_2005_new*(pindex_na(c_na_2005, 2005)^(phi_nn))*(pindex_ag(c_ag_2005, 2005)^(phi_an))/master$dTFP[master$sector == "na" & master$year == 2005]

c_na_2010_new = (master$dnomY[master$sector == "na" & master$year == 2010]^(vashare_n)/(master$dL[master$sector == "na" & master$year == 2010]^(lab_na)*master$dK[master$sector == "na" & master$year == 2010]^(cap_na)*master$dcommercial_land[master$sector == "na" & master$year == 2010]^(land_na)))
c_na_2010_new = c_na_2005_new*(pindex_na(c_na_2010, 2010)^(phi_nn))*(pindex_ag(c_ag_2010, 2010)^(phi_an))/master$dTFP[master$sector == "na" & master$year == 2010]

c_ag_2005_new = (master$dnomY[master$sector == "ag" & master$year == 2005]^(vashare_a)/(master$dL[master$sector == "ag" & master$year == 2005]^(lab_ag)*master$dK[master$sector == "ag" & master$year == 2005]^(cap_ag)*master$dcommercial_land[master$sector == "ag" & master$year == 2005]^(land_ag)))
c_ag_2005_new = c_ag_2005_new*(pindex_na(c_na_2005, 2005)^(phi_na))*(pindex_ag(c_ag_2005, 2005)^(phi_aa))/master$dTFP[master$sector == "ag" & master$year == 2005]

c_ag_2010_new = (master$dnomY[master$sector == "ag" & master$year == 2010]^(vashare_a)/(master$dL[master$sector == "ag" & master$year == 2010]^(lab_ag)*master$dK[master$sector == "ag" & master$year == 2010]^(cap_ag)*master$dcommercial_land[master$sector == "ag" & master$year == 2010]^(land_ag)))
c_ag_2010_new = c_ag_2010_new*(pindex_na(c_na_2010, 2010)^(phi_na))*(pindex_ag(c_ag_2010, 2010)^(phi_aa))/master$dTFP[master$sector == "ag" & master$year == 2010]

#Updating norm
cnorm_2005 = max(norm(c_ag_2005_new - c_ag_2005), norm(c_na_2005_new - c_na_2005))
cnorm_2010 = max(norm(c_ag_2010_new - c_ag_2010), norm(c_na_2010_new - c_na_2010))

#Updating new value
c_na_2005 = c_na_2005_new
c_na_2010 = c_na_2010_new
c_ag_2005 = c_ag_2005_new
c_ag_2010 = c_ag_2010_new

}

#Updating values into master_stacked
master$dcost[master$year == 2005 & master$sector == "ag"] <- c_ag_2005
master$dcost[master$year == 2005 & master$sector == "na"] <- c_na_2005
master$dcost[master$year == 2010 & master$sector == "ag"] <- c_ag_2010
master$dcost[master$year == 2010 & master$sector == "na"] <- c_na_2010

master$dpindex_ag[master$year == 2005 & master$sector == "ag"] <- pindex_ag(c_ag_2005, 2005)
master$dpindex_ag[master$year == 2005 & master$sector == "na"] <- pindex_ag(c_ag_2005, 2005)
master$dpindex_ag[master$year == 2010 & master$sector == "ag"] <- pindex_ag(c_ag_2010, 2010)
master$dpindex_ag[master$year == 2010 & master$sector == "na"] <- pindex_ag(c_ag_2010, 2010)

master$dpindex_na[master$year == 2005 & master$sector == "ag"] <- pindex_na(c_na_2005, 2005)
master$dpindex_na[master$year == 2005 & master$sector == "na"] <- pindex_na(c_na_2005, 2005)
master$dpindex_na[master$year == 2010 & master$sector == "ag"] <- pindex_na(c_na_2010, 2010)
master$dpindex_na[master$year == 2010 & master$sector == "na"] <- pindex_na(c_na_2010, 2010)

#cost/prices in levels. 
master$pindex_ag[master$year == 2005] <- master$pindex_ag[master$year == 2000]*master$dpindex_ag[master$year == 2005]
master$pindex_na[master$year == 2005] <- master$pindex_na[master$year == 2000]*master$dpindex_na[master$year == 2005]
master$pindex_ag[master$year == 2010] <- master$pindex_ag[master$year == 2005]*master$dpindex_ag[master$year == 2010]
master$pindex_na[master$year == 2010] <- master$pindex_na[master$year == 2005]*master$dpindex_na[master$year == 2010]

master$cost[master$year == 2005] <- master$cost[master$year == 2000]*master$dcost[master$year == 2005]
master$cost[master$year == 2010] <- master$cost[master$year == 2005]*master$dcost[master$year == 2010]

write_xlsx(master, path = "constructed_output/master_stacked2.xlsx")

#Updating values in master_unstacked
#1) Updated normalized gross nominal output by sector/year
master_unstacked$nomY_ag_2000 <- master$nomY[master$sector == "ag" & master$year == 2000]
master_unstacked$nomY_na_2000 <- master$nomY[master$sector == "na" & master$year == 2000]
master_unstacked$nomY_ag_2005 <- master$nomY[master$sector == "ag" & master$year == 2005]
master_unstacked$nomY_na_2005 <- master$nomY[master$sector == "na" & master$year == 2005]
master_unstacked$nomY_ag_2010 <- master$nomY[master$sector == "ag" & master$year == 2010]
master_unstacked$nomY_na_2010 <- master$nomY[master$sector == "na" & master$year == 2010]

#2)Updating costs/price indicies by sector_year
master_unstacked$dcost_ag_2005 <- master$dcost[master$year == 2005 & master$sector == "ag"]
master_unstacked$dcost_na_2005 <- master$dcost[master$year == 2005 & master$sector == "na"] 
master_unstacked$dcost_ag_2010 <- master$dcost[master$year == 2010 & master$sector == "ag"]
master_unstacked$dcost_na_2010 <- master$dcost[master$year == 2010 & master$sector == "na"] 

master_unstacked$dpindex_ag_2005 <- master$dpindex_ag[master$year == 2005 & master$sector == "ag"]
master_unstacked$dpindex_ag_2010 <-  master$dpindex_ag[master$year == 2010 & master$sector == "ag"]
master_unstacked$dpindex_na_2005 <- master$dpindex_na[master$year == 2005 & master$sector == "ag"]
master_unstacked$dpindex_na_2010 <-  master$dpindex_na[master$year == 2010 & master$sector == "ag"]

#3)Transcribing TFP measures from master_stacked 
master_unstacked["dTFP_na_2005"] <- rep(NA, nrow(master_unstacked))
master_unstacked["dTFP_ag_2005"] <- rep(NA, nrow(master_unstacked))
master_unstacked["dTFP_na_2010"] <- rep(NA, nrow(master_unstacked))
master_unstacked["dTFP_ag_2010"] <- rep(NA, nrow(master_unstacked))
master_unstacked$dTFP_na_2005 <- master$dTFP[master$year == 2005 & master$sector == "na"]
master_unstacked$dTFP_na_2010 <- master$dTFP[master$year == 2010 & master$sector == "na"]
master_unstacked$dTFP_ag_2005 <- master$dTFP[master$year == 2005 & master$sector == "ag"]
master_unstacked$dTFP_ag_2010 <- master$dTFP[master$year == 2010 & master$sector == "ag"]

write_xlsx(master_unstacked, path = "constructed_output/master2.xlsx")