library(ggplot2)
library(readxl)
library(writexl)
library(tidyverse)
##Estimate_trade_costs. Date Created: May 19th 2021
##                      Date Edited: June 8th 2021

##INPUT: Raw trade flow data, GDP data, etc.
##OUTPUT: Regional aggregated tradeflows + Bilateral trade costs without correcting for trade elasticity.

##Follows a procedure identical to Tombe and Zhu (2019) and Tombe et. al (2020).
##Trade costs are assumed symmetric for now (the standard Head Reiss index.)

setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")


phi_an = 0.04 
phi_aa = 0.16 
phi_na = 0.25 
phi_nn = 0.61 
vashare_n = 1 - phi_nn - phi_an
vashare_a = 1 - phi_na - phi_aa



#Note this procedure gives trade costs for the aggregated regions in all years.
#Uses change in trade costs provided by Hao et. al (2021).

#Importing data: 
master_data <- read_excel("master_raw.xlsx")
row.names(master_data) <- master_data$province

tr_ag_2002 <- read_excel("2002_ag_tradecosts.xlsx")
tr_ag_2002 <- tr_ag_2002[, -1]
row.names(tr_ag_2002) <- colnames(tr_ag_2002)
tr_ag_2002 <- tr_ag_2002^(4) #raising to power of trade elasticity used in Tombe and Zhu, (2012)

tr_na_2002 <- read_excel("2002_na_tradecosts.xlsx")
tr_na_2002 <- tr_na_2002[, -1]
row.names(tr_na_2002) <- colnames(tr_na_2002)
tr_na_2002 <- tr_na_2002^(4)

trdelta_ag_2007 <- read_excel("2007_ag_deltatradecosts.xlsx")
trdelta_ag_2007 <- trdelta_ag_2007[, -1]
row.names(trdelta_ag_2007) <- colnames(trdelta_ag_2007)
 #raising to power of trade elasticity used in Hao et. al. 
trdelta_ag_2007 <- trdelta_ag_2007^(4)

trdelta_ag_2012 <- read_excel("2012_ag_deltatradecosts.xlsx")
trdelta_ag_2012 <- trdelta_ag_2012[, -1]
row.names(trdelta_ag_2012) <- colnames(trdelta_ag_2012)
trdelta_ag_2012 <- trdelta_ag_2012^(4)


trdelta_na_2007 <- read_excel("2007_na_deltatradecosts.xlsx")
trdelta_na_2007 <- trdelta_na_2007[, -1]
row.names(trdelta_na_2007) <- colnames(trdelta_na_2007)
trdelta_na_2007 <- trdelta_na_2007^(4)

trdelta_na_2012 <- read_excel("2012_na_deltatradecosts.xlsx")
trdelta_na_2012 <- trdelta_na_2012[, -1]
row.names(trdelta_na_2012) <- colnames(trdelta_na_2012)
trdelta_na_2012 <- trdelta_na_2012^(4)


#Crosswalk for provinces to regions.
Northeast <- c(11, 18, 19)
Beijing_Tianjin <- c(2, 27)
North_coast <- c(10, 22)
Central_coast <- c(16, 23, 30)
South_coast <- c(4, 6, 9)
Central_region <- c(25, 12, 1, 13, 14, 17)
Northwest <- c(15, 24, 20, 5, 21, 28)
Southwest <- c(26, 3, 29, 8, 7)
International <- c(31)
Aggregate_reg <- list(Northeast, Beijing_Tianjin, North_coast, Central_coast, South_coast, Central_region, Northwest, Southwest, International)
reg_name <- c("Northeast", "Beijing_Tianjin", "North_coast", "Central_coast", "South_coast", "Central_region", "Northwest", "Southwest", "International")


### Part 2: Estimating trade costs (NOTE: THESE ARE NOT NET OF TRADE ELASTICITY)#################

#Constructing Head and Reiss indices for each period

Tradeflows <- list(tr_ag_2002, tr_na_2002)
Head_Ries <- list(trdelta_ag_2007, trdelta_na_2007) #Contents in this list have no use but for an empty 31x31 matrix

map <- c(6, 2, 8, 5, 7, 5, 8, 8, 5, 3, 1, 6, 6, 6, 7, 4, 6, 1, 1, 7, 7, 3, 4, 7, 6, 8, 2, 7, 8, 4, 9) #Remapping regions to provinces again by numerical order in "reg_name" 

hri = 0
for (hr in Head_Ries) {
  hri = hri + 1
  temptr <- as.data.frame(Tradeflows[hri])
  i1=0
  for (reg1 in colnames(hr)) {
    i1=i1 + 1
    i2=0
    for (reg2 in colnames(hr)) {
      i2 = i2 + 1
      hr[i1, i2] <- temptr[map[i1], map[i2]]
     
    }
  }
  Head_Ries[[hri]] <- hr
}

Head_Ries[[3]] <- Head_Ries[[1]]*trdelta_ag_2007
Head_Ries[[4]] <- Head_Ries[[2]]*trdelta_na_2007
Head_Ries[[5]] <- Head_Ries[[1]]*trdelta_ag_2012*trdelta_ag_2007
Head_Ries[[6]] <- Head_Ries[[2]]*trdelta_na_2012*trdelta_na_2007


##Saving data to xlsx 
setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData/constructed_output")
write_xlsx(Head_Ries[1], "HRtau_ag_2002.xlsx")
write_xlsx(Head_Ries[2], "HRtau_na_2002.xlsx")
write_xlsx(Head_Ries[3], "HRtau_ag_2007.xlsx")
write_xlsx(Head_Ries[4], "HRtau_na_2007.xlsx")
write_xlsx(Head_Ries[5], "HRtau_ag_2012.xlsx")
write_xlsx(Head_Ries[6], "HRtau_na_2012.xlsx")

##Estimating trade cost change asymmetries

