library(ggplot2)
library(readxl)
library(writexl)
library(tidyverse)
##Estimate_trade_costs. Date Created: May 19th 2021
##                      Date Edited: June 8th 2021

##INPUT: Raw trade flow data, GDP data, etc.
##OUTPUT: Regional aggregated tradeflows + Bilateral trade costs WITHOUT TRADE-ELASTICITY WEIGHT.

##Follows a procedure identical to Tombe and Zhu (2019) and Tombe et. al (2020). 
##Trade costs are assumed symmetric for now (the standard Head Reiss index.)

setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")



#Production network
phi_an = 0.04 
phi_aa = 0.16 
phi_na = 0.25 
phi_nn = 0.61 
vashare_n = 1 - phi_nn - phi_an
vashare_a = 1 - phi_na - phi_aa


#Importing data: 
master_data <- read_excel("master_raw.xlsx")
row.names(master_data) <- master_data$province

tr_ag_2002 <- read_excel("2002_ag_tradeflows.xlsx")
tr_ag_2002 <- tr_ag_2002[, -1]
row.names(tr_ag_2002) <- colnames(tr_ag_2002)

tr_na_2002 <- read_excel("2002_na_tradeflows.xlsx")
tr_na_2002 <- tr_na_2002[, -1]
row.names(tr_na_2002) <- colnames(tr_na_2002)

tr_ag_2007 <- read_excel("2007_ag_tradeflows.xlsx")
tr_ag_2007 <- tr_ag_2007[, -1]
row.names(tr_ag_2007) <- colnames(tr_ag_2007)

tr_na_2007 <- read_excel("2007_na_tradeflows.xlsx")
tr_na_2007 <- tr_na_2007[, -1]
row.names(tr_na_2007) <- colnames(tr_na_2007)

tr_ag_2012 <- read_excel("2012_ag_tradeflows.xlsx")
tr_ag_2012 <- tr_ag_2012[, -1]
row.names(tr_ag_2012) <- colnames(tr_ag_2012)

tr_na_2012 <- read_excel("2012_na_tradeflows.xlsx")
tr_na_2012 <- tr_na_2012[, -1]
row.names(tr_na_2012) <- colnames(tr_na_2012)

#Also have to aggregate GDP data in broad regions for sourcing shares of these broad regions (to get trade flows)
#Numerically assigning provinces to regions given order in "master_raw"
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

##CREATING MATRICES

tr_ag_2002_agr <- data.frame(matrix(nrow = 9, ncol = 9))
row.names(tr_ag_2002_agr) <- reg_name
colnames(tr_ag_2002_agr) <- reg_name

tr_na_2002_agr <- data.frame(matrix(nrow = 9, ncol = 9))
row.names(tr_na_2002_agr) <- reg_name
colnames(tr_na_2002_agr) <- reg_name

tr_ag_2007_agr <- data.frame(matrix(nrow = 9, ncol = 9))
row.names(tr_ag_2007_agr) <- reg_name
colnames(tr_ag_2007_agr) <- reg_name

tr_na_2007_agr <- data.frame(matrix(nrow = 9, ncol = 9))
row.names(tr_na_2007_agr) <- reg_name
colnames(tr_na_2007_agr) <- reg_name

tr_ag_2012_agr <- data.frame(matrix(nrow = 9, ncol = 9))
row.names(tr_ag_2012_agr) <- reg_name
colnames(tr_ag_2012_agr) <- reg_name

tr_na_2012_agr <- data.frame(matrix(nrow = 9, ncol = 9))
row.names(tr_na_2012_agr) <- reg_name
colnames(tr_na_2012_agr) <- reg_name


#2002_ag
i1 = 0
i2 = 0
for (reg1 in Aggregate_reg) {
  i1 = i1 + 1
  i2 = 0
  
  for(reg2 in Aggregate_reg) {  
    i2 = i2 + 1
    source_ti <- as.matrix(c(0)) 
    gr_gdp <- as.matrix(c(0))
    
    for (i in reg1) {
      source_i = as.matrix(c(0))
      for (j in reg2) {
        source_i <- as.matrix(source_i) + as.matrix(tr_ag_2002[i, j])
      }
      source_i <- as.matrix(source_i*(vashare_a*(master_data$agspend_na_2000[i])*master_data$nomY_na_2000[i] + vashare_n*(master_data$agspend_ag_2000[i])*master_data$nomY_ag_2000[i] + 
                                        (phi_an)*master_data$nomY_na_2000[i] + (phi_aa)*master_data$nomY_ag_2000[i]))
      source_ti <- source_i + source_ti
      gr_gdp <- as.matrix((vashare_a*(master_data$agspend_na_2000[i])*master_data$nomY_na_2000[i] + vashare_n*(master_data$agspend_ag_2000[i])*master_data$nomY_ag_2000[i] + 
                             (phi_an)*master_data$nomY_na_2000[i] + (phi_aa)*master_data$nomY_ag_2000[i])) + gr_gdp
    }
    
    tr_ag_2002_agr[i1, i2] <- source_ti/gr_gdp
    
  }
}

#2002na aggregation. 

i1 = 0
i2 = 0
for (reg1 in Aggregate_reg) {
  i1 = i1 + 1
  i2 = 0
  for(reg2 in Aggregate_reg) {  
    i2 = i2 + 1
    source_ti <- as.matrix(c(0)) 
    gr_gdp <- as.matrix(c(0))
    
    for (i in reg1) {
      source_i = as.matrix(c(0))
      for (j in reg2) {
        source_i <- as.matrix(source_i) + as.matrix(tr_na_2002[i, j])
      }
      source_i <- as.matrix(source_i*(vashare_a*(1-master_data$agspend_na_2000[i])*master_data$nomY_na_2000[i] + vashare_n*(1-master_data$agspend_ag_2000[i])*master_data$nomY_ag_2000[i] + 
                                        (phi_nn)*master_data$nomY_na_2000[i] + (phi_na)*master_data$nomY_ag_2000[i]))
      source_ti <- source_i + source_ti
      gr_gdp <- as.matrix((vashare_a*(1-master_data$agspend_na_2000[i])*master_data$nomY_na_2000[i] + vashare_n*(1-master_data$agspend_ag_2000[i])*master_data$nomY_ag_2000[i] + 
                             (phi_nn)*master_data$nomY_na_2000[i] + (phi_na)*master_data$nomY_ag_2000[i])) + gr_gdp
    }
    
    tr_na_2002_agr[i1, i2] <- source_ti/gr_gdp
    
  }
}

##2007_ag
i1 = 0
i2 = 0
for (reg1 in Aggregate_reg) {
  i1 = i1 + 1
  i2 = 0
  
  for(reg2 in Aggregate_reg) {  
    i2 = i2 + 1
    source_ti <- as.matrix(c(0)) 
    gr_gdp <- as.matrix(c(0))
    
    for (i in reg1) {
      source_i = as.matrix(c(0))
      for (j in reg2) {
        source_i <- as.matrix(source_i) + as.matrix(tr_ag_2007[i, j])
      }
      source_i <- as.matrix(source_i*(vashare_a*(master_data$agspend_na_2005[i])*master_data$nomY_na_2005[i] + vashare_n*(master_data$agspend_ag_2005[i])*master_data$nomY_ag_2005[i] + 
                                        (phi_an)*master_data$nomY_na_2005[i] + (phi_aa)*master_data$nomY_ag_2005[i]))
      source_ti <- source_i + source_ti
      gr_gdp <- as.matrix((vashare_a*(master_data$agspend_na_2005[i])*master_data$nomY_na_2005[i] + vashare_n*(master_data$agspend_ag_2005[i])*master_data$nomY_ag_2005[i] + 
                             (phi_an)*master_data$nomY_na_2005[i] + (phi_aa)*master_data$nomY_ag_2005[i])) + gr_gdp
    }
    
    tr_ag_2007_agr[i1, i2] <- source_ti/gr_gdp
    
  }
}


#2007_na
i1 = 0
i2 = 0
for (reg1 in Aggregate_reg) {
  i1 = i1 + 1
  i2 = 0
  for(reg2 in Aggregate_reg) {  
    i2 = i2 + 1
    source_ti <- as.matrix(c(0)) 
    gr_gdp <- as.matrix(c(0))
    
    for (i in reg1) {
      source_i = as.matrix(c(0))
      for (j in reg2) {
        source_i <- as.matrix(source_i) + as.matrix(tr_na_2007[i, j])
      }
      source_i <- as.matrix(source_i*(vashare_a*(1-master_data$agspend_na_2005[i])*master_data$nomY_na_2005[i] + vashare_n*(1-master_data$agspend_ag_2005[i])*master_data$nomY_ag_2005[i] + 
                                        (phi_nn)*master_data$nomY_na_2005[i] + (phi_na)*master_data$nomY_ag_2005[i]))
      source_ti <- source_i + source_ti
      gr_gdp <- as.matrix((vashare_a*(1-master_data$agspend_na_2005[i])*master_data$nomY_na_2005[i] + vashare_n*(1-master_data$agspend_ag_2005[i])*master_data$nomY_ag_2005[i] + 
                             (phi_nn)*master_data$nomY_na_2005[i] + (phi_na)*master_data$nomY_ag_2005[i])) + gr_gdp
    }
    
    tr_na_2007_agr[i1, i2] <- source_ti/gr_gdp
    
  }
}

#2012_ag
i1 = 0
i2 = 0
for (reg1 in Aggregate_reg) {
  i1 = i1 + 1
  i2 = 0
  
  for(reg2 in Aggregate_reg) {  
    i2 = i2 + 1
    source_ti <- as.matrix(c(0)) 
    gr_gdp <- as.matrix(c(0))
    
    for (i in reg1) {
      source_i = as.matrix(c(0))
      for (j in reg2) {
        source_i <- as.matrix(source_i) + as.matrix(tr_ag_2012[i, j])
      }
      source_i <- as.matrix(source_i*(vashare_a*(master_data$agspend_na_2010[i])*master_data$nomY_na_2010[i] + vashare_n*(master_data$agspend_ag_2010[i])*master_data$nomY_ag_2010[i] + 
                                        (phi_an)*master_data$nomY_na_2010[i] + (phi_aa)*master_data$nomY_ag_2010[i]))
      source_ti <- source_i + source_ti
      gr_gdp <- as.matrix((vashare_a*(master_data$agspend_na_2010[i])*master_data$nomY_na_2010[i] + vashare_n*(master_data$agspend_ag_2010[i])*master_data$nomY_ag_2010[i] + 
                             (phi_an)*master_data$nomY_na_2010[i] + (phi_aa)*master_data$nomY_ag_2010[i])) + gr_gdp
    }
    
    tr_ag_2012_agr[i1, i2] <- source_ti/gr_gdp
    
  }
}

#2012_na 
i1 = 0
i2 = 0
for (reg1 in Aggregate_reg) {
  i1 = i1 + 1
  i2 = 0
  for(reg2 in Aggregate_reg) {  
    i2 = i2 + 1
    source_ti <- as.matrix(c(0)) 
    gr_gdp <- as.matrix(c(0))
    
    for (i in reg1) {
      source_i = as.matrix(c(0))
      for (j in reg2) {
        source_i <- as.matrix(source_i) + as.matrix(tr_na_2012[i, j])
      }
      source_i <- as.matrix(source_i*(vashare_a*(1-master_data$agspend_na_2010[i])*master_data$nomY_na_2010[i] + vashare_n*(1-master_data$agspend_ag_2010[i])*master_data$nomY_ag_2010[i] + 
                                        (phi_nn)*master_data$nomY_na_2010[i] + (phi_na)*master_data$nomY_ag_2010[i]))
      source_ti <- source_i + source_ti
      gr_gdp <- as.matrix((vashare_a*(1-master_data$agspend_na_2010[i])*master_data$nomY_na_2010[i] + vashare_n*(1-master_data$agspend_ag_2010[i])*master_data$nomY_ag_2010[i] + 
                             (phi_nn)*master_data$nomY_na_2010[i] + (phi_na)*master_data$nomY_ag_2010[i])) + gr_gdp
    }
    
    tr_na_2012_agr[i1, i2] <- source_ti/gr_gdp
    
  }
}


### Part 2: Estimating trade costs (NOTE: THESE ARE NOT NET OF TRADE ELASTICITY)#################

#Constructing Head and Reiss indices for each period

Tradeflows <- list(tr_ag_2002, tr_na_2002, tr_ag_2007, tr_na_2007, tr_ag_2012, tr_na_2012)
Tradeflows_agr <- list(tr_ag_2002_agr, tr_na_2002_agr, tr_ag_2007_agr, tr_na_2007_agr, tr_ag_2012_agr, tr_na_2012_agr)
Head_Ries <- list()

map <- c(6, 2, 8, 5, 7, 5, 8, 8, 5, 3, 1, 6, 6, 6, 7, 4, 6, 1, 1, 7, 7, 3, 4, 7, 6, 8, 2, 7, 8, 4, 9)

hri = 0
for (hr in Tradeflows) {
  hri = hri + 1
  temptr <- as.data.frame(Tradeflows_agr[hri])
  i1=0
  for (reg1 in colnames(hr)) {
    i1=i1 + 1
    i2=0
    for (reg2 in colnames(hr)) {
      i2 = i2 + 1
      hr[i1, i2] <- temptr[map[i1], map[i1]]*temptr[map[i2], map[i2]]/(temptr[map[i1], map[i2]]*temptr[map[i2], map[i1]])
      
    }
  }
  Head_Ries[[hri]] <- hr
}



##Saving data to xlsx 
setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData/constructed_output")
write_xlsx(Head_Ries[1], "HRtau_ag_2002.xlsx")
write_xlsx(Head_Ries[2], "HRtau_na_2002.xlsx")
write_xlsx(Head_Ries[3], "HRtau_ag_2007.xlsx")
write_xlsx(Head_Ries[4], "HRtau_na_2007.xlsx")
write_xlsx(Head_Ries[5], "HRtau_ag_2012.xlsx")
write_xlsx(Head_Ries[6], "HRtau_na_2012.xlsx")

##Estimating trade cost change asymmetries - (need data on bilateral distances, come back later)