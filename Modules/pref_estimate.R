
library(ggplot2)
library(readxl)
library(writexl)
library(tidyverse)
library(estimatr)
library(reshape2)
library(tidyr)
library(boot)
library(NlcOptim)
library(pracma)
library(matrixcalc)
library(janitor)


#Estimates epsilon_a and eta as per the paper.
#Date created: May 28th, 2021
#Date edited: June 9th, 2021

setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")

theta = 4 #Simonovska and Waugh (20xx)

#Reading and cleaning data
master <- read_excel("constructed_output/master_stacked.xlsx")
master$cost[master$year == 2005 & master$sector == "ag"] <- master$cost[master$year == 2000 & master$sector == "ag"]*master$dcost[master$year == 2005 & master$sector == "ag"]
master$cost[master$year == 2010 & master$sector == "ag"] <- master$dcost[master$year == 2010 & master$sector == "ag"]*master$cost[master$year == 2005 & master$sector == "ag"]
master$cost[master$year == 2005 & master$sector == "na"] <- master$cost[master$year == 2000 & master$sector == "na"]*master$dcost[master$year == 2005 & master$sector == "na"]
master$cost[master$year == 2010 & master$sector == "na"] <- master$dcost[master$year == 2010 & master$sector == "na"]*master$cost[master$year == 2005 & master$sector == "na"]

#Correcting pindex by trade elasticity.
master$pindex_na[master$year == 2005] <- (master$pindex_na[master$year == 2000]*master$dpindex_na[master$year == 2005])^(1/theta)
master$pindex_na[master$year == 2010] <- (master$dpindex_na[master$year == 2010 ])^(1/theta)*master$pindex_na[master$year == 2005]
master$pindex_ag[master$year == 2005] <- (master$pindex_ag[master$year == 2000]*master$dpindex_ag[master$year == 2005])^(1/theta)
master$pindex_ag[master$year == 2010] <- (master$dpindex_ag[master$year == 2010])^(1/theta)*master$pindex_ag[master$year == 2005]
master$pindex_ag[master$year == 2000] <- (master$pindex_ag[master$year == 2000])^(1/theta)
master$pindex_na[master$year == 2000] <- (master$pindex_na[master$year == 2000])^(1/theta)
#Removing international observation-- expenditure not well measured in this case.
master <- master[!master$province %in% "International",]


#Creating data frame vars for estimation. 
#Choice of non-agriculture as base sector here.  

master["naspend"] <- rep(1, nrow(master)) - master$agspend
master["log_relprice"] <- log(master$pindex_ag/master$pindex_na)
master["log_naspend"] <- log(master$naspend)
master["log_agspend"] <- log(master$agspend)
master["log_relspend"] <- log(master$agspend/master$naspend)
master["log_rY"] <- log(master$rY)
master["log_relemp"] <- rep(1, nrow(master))
master$log_relemp[master$year == 2000] <- log(master$L[master$year == 2000 & master$sector == "ag"]/master$L[master$year == 2000 & master$sector == "na"])
master$log_relemp[master$year == 2005] <- log(master$L[master$year == 2005 & master$sector == "ag"]/master$L[master$year == 2005 & master$sector == "na"])
master$log_relemp[master$year == 2010] <- log(master$L[master$year == 2010 & master$sector == "ag"]/master$L[master$year == 2010 & master$sector == "na"])
master["log_EPa"] <- log(master$nomY/master$L) - log(master$pindex_na)
master["FE_2005"] <- ifelse(master$year == 2005, 1, 0)
master["FE_2010"] <- ifelse(master$year == 2010, 1, 0)




#______REGRESSIONS_____#
#With income per capita omitted.
share_omitted <- lm_robust(log_relspend~log_relprice + FE_2005 + FE_2010, data = master, cluster = year,  se_type = "stata", alpha = 0.05)

#Regression with relative spending vs real_gdp measures
share_reg <- lm_robust(log_relspend~log_relprice + log_rY + FE_2005 + FE_2010, data = master, cluster = year,  se_type = "stata", alpha = 0.05)

#Subsetting data if estimating using relative employment specification
master_sub <- master[master$sector %in% "na",]

#rel_employment specification
relemp_reg <- lm_robust(log_relemp~log_relprice + log_rY + FE_2005 + FE_2010, data = master_sub, cluster = year,  se_type = "stata", alpha = 0.05)

#Functions
#_______________________________________________________________________________________
estimate_un <- function(data, ini) { #takes data frame and initial value (with proper naming conventions and outputs fminunc output)
  
obj_fn <- function(x) {
  data["Errors"] <<- data$log_relemp - (x[1]-1)*data$log_naspend - (x[1] - 1)*(1-x[2])*data$log_EPa - (1-x[2])*data$log_relprice - x[3]*data$FE_2005 - x[4]*data$FE_2010 - x[5]
  SSE <- colSums(as.matrix(data$Errors)^2)
  return(SSE)
}
 

grad_fn <- function(x) {
  data["Errors"] <<- data$log_relemp - (x[1]-1)*data$log_naspend - (x[1] - 1)*(1-x[2])*data$log_EPa - (1-x[2])*data$log_relprice - x[3]*data$FE_2005 - x[4]*data$FE_2010 - x[5]
  eps_v <- colSums(as.matrix(2*data$Errors*(-(1-x[2])*data$log_EPa - data$log_naspend)))
  eta_v <- colSums(as.matrix(2*data$Errors*((x[1] - 1)*data$log_EPa + data$log_relprice)))
  t05_v <- colSums(as.matrix(2*data$Errors*(-master$FE_2005)))
  t10_v <- colSums(as.matrix(2*data$Errors*(-master$FE_2010)))
  t00_v <- colSums(as.matrix(2*data$Errors*(-1)))
  return(c(eps_v, eta_v, t05_v, t10_v, t00_v))
}
estimate_unc <- fminunc(x0 = ini, fn=obj_fn, gr = grad_fn,  maxiter = 100, maxfeval = 100)
return(estimate_unc)

}
#_________________________________________________________________________________________

init <- c(2, 1.1, 0, 0, 0) #Doesn't matter, estimate globally convergent. 

estimate_unc <- estimate_un(master_sub, init)

#Statistic function for bootstrap
estimate_un_boot <- function(data, indices) {
  resampled_data <- data[indices, ]
  estimate_unc <- estimate_un(resampled_data, init)
  return(estimate_unc$par)
}

#bootstrapping output
set.seed(4321)
#estimate_unc_bootstrap <- boot(data = master_sub, statistic = estimate_un_boot, R = 5000)
#boot.ci(estimate_unc_bootstrap, conf = 0.99, index = 1, type = "all")
#boot.ci(estimate_unc_bootstrap, conf = 0.99, index = 2, type = "all")

print(estimate_unc_bootstrap)

#Checking correlation with real income. 
master["log_Model_consumption_index"] <- master$log_EPa + (1/(1-estimate_unc$par[2]))*master$log_naspend
check_reg <- lm_robust(log_Model_consumption_index~log_rY, data=master)


#_______________PART 2_____________________________________
#Estimating NLS specification where ag consumption shares are on the LFS.  
#No need to calculate gradient as we will require that epsilon > 0 (so that inversion actually works.)



slnint <- c(0, 1) #solution interval (global parameter with estimate_nlscon)

#NLS FUNCTION
#________________________________________________________________________________________________________________
estimate_nlscon <- function(data, ini) {

  fn_tosolve <- function(y) {
  return(log(y) - epsilon*log(1-y) - (epsilon - 1)*(1-etaa)*data$log_EPa[ig] - (1-etaa)*data$log_relprice[ig] - f05*data$FE_2005[ig] - f10*data$FE_2010[ig] - interc)
  }

  nls_inverse <- function(x) {
  #Creates function and solves for inverse.
  #Globals for use with fn_solution function. 
    epsilon <<- x[1]
    etaa <<- x[2]
    f05 <<- x[3]
    f10 <<- x[4]
    interc <<- x[5]
  
    sln <- uniroot(f = fn_tosolve, interval = slnint)
    return(sln$root)
  }

#Objective function
  nls_obj_fn <- function(x) {
  
   for (i in 1:nrow(master)) {
     ig <<- i # setting global to use with function
     data$NLS_Errors[ig] <- data$agspend[ig] - nls_inverse(x)
    }
  
    SSE <- colSums(as.matrix(data$NLS_Errors)^2) #rescaling objective function for machine precision. 
    return(SSE)
  
  }
  #final output
  data["NLS_Errors"] <- rep(NA, nrow(data))
  estimate_nls <- fmincon(x0 = ini, fn = nls_obj_fn, lb = c(0.001, 0.001, -50, -50, -50))
  return(estimate_nls)
}
#____________________________________________________________________________________________________________
#estimating

init <- c(7, 1.1, 0, 0, 0)

#fmincon appears to have the highest performance and speed here, even without supplying gradient.
estimates_nls <- estimate_nlscon(master, init) 

#Statistic function for bootstrap
estimate_nls_boot <- function(data, indices) {
  resampled_data <- data[indices, ]
  estimate <- estimate_nlscon(resampled_data, init)
  return(estimate$par)
}

set.seed(4321)
#estimate_nls_bootstrap <- boot(data = master, statistic = estimate_nls_boot, R = 100) #bootstrap takes forever with this model-- and estimates are extremely topheavy
#boot.ci(estimate_nls_bootstrap, conf = 0.65, index = 1, type = "all")
#boot.ci(estimate_nls_bootstrap, conf = 0.65, index = 2, type = "all")

#Correlation with model implied index and actual observed real GDP measures performs well w/ entire sample.
master["log_Model_consumption_index_nls"] <- master$log_EPa + (1/(1-estimates_nls$par[2]))*master$log_naspend
check_reg2 <- lm_robust(log_Model_consumption_index_nls~log_rY, data = master)



#___________________________________________________________________________________________________________________
#PART 3: Estimating productivity growth assuming change in relative prices from Groningen 10 sector database
load("constructed_output/d_g10s_constructed.Rda")
groningen_10s_CHN <- d_g10s_constructed[d_g10s_constructed$Country %in% "CHN",]
rm(d_g10s_constructed)
groningen_10s_CHN <- clean_names(groningen_10s_CHN)
#constructing China wide na price index-- vashare weighted geometric average of price indices in all other 7 sectors (mining incl in primary industry) 
#Omit government services because no price data. 
groningen_10s_CHN["Price_na"] <- (groningen_10s_CHN$manufacturing_price^(groningen_10s_CHN$manufacturing_v_ashare)* 
                                 groningen_10s_CHN$utilities_price^(groningen_10s_CHN$utilities_v_ashare)* 
                                 groningen_10s_CHN$construction_price^(groningen_10s_CHN$construction_v_ashare)* 
                                 groningen_10s_CHN$trade_restaurants_and_hotels_price^(groningen_10s_CHN$trade_restaurants_and_hotels_v_ashare)* 
                                 groningen_10s_CHN$transport_storage_and_communication_price^(groningen_10s_CHN$transport_storage_and_communication_v_ashare)* 
                                 groningen_10s_CHN$finance_insurance_real_estate_and_business_services_price^(groningen_10s_CHN$finance_insurance_real_estate_and_business_services_v_ashare)* 
                                 groningen_10s_CHN$community_social_and_personal_services_price^(groningen_10s_CHN$community_social_and_personal_services_v_ashare))

#Adjusting for VASHARE of all these sectors
groningen_10s_CHN$Price_na <- groningen_10s_CHN$Price_na^(1/(1 - groningen_10s_CHN$agriculture_v_ashare - groningen_10s_CHN$mining_v_ashare - groningen_10s_CHN$government_services_v_ashare))

rel_price_china <- as.matrix(groningen_10s_CHN$agriculture_price/groningen_10s_CHN$Price_na)
row.names(rel_price_china) <- as.matrix(groningen_10s_CHN$year)
rm(groningen_10s_CHN)

#Aggregate relative price increase
rel_priceag_2005 <- rel_price_china["2005",]/rel_price_china["2000",]
rel_priceag_2010 <- rel_price_china["2010",]/rel_price_china["2005",]
rm(rel_price_china)

rel_priceag_2005 <- unname(rel_priceag_2005) #price of ag increased by 8% in aggregate 2000-2005
rel_priceag_2010 <- unname(rel_priceag_2010) #price of ag increased by 18% in aggregate 2005-2010


#Choosing G_n to match the population-weighted average fall in relative agricultural spending in the data. from 2000-2005 and 2005-2010.
#NOTE: using model implied index above, which is partially calibrated to future ag consumption shares. As such, the indirect utility function will not be satisfied exactly!
#This is because the model is overidentified.

#NOTE: estimates of Gn will depend on which estimates of eta and epsilon are chosen!

#Using estimate_unc specification for this (estimated using non-agricultural subsample)
master["relspend_error_Gn"] <- rep(NA, nrow(master)) 
master$relspend_error_Gn[master$year == 2005] <- (master$log_relspend[master$year == 2005] - master$log_relspend[master$year == 2000]) - 
                                               (1-estimate_unc$par[2])*(master$log_relprice[master$year == 2005] - master$log_relprice[master$year == 2000] + log(rel_priceag_2005)*rep(1, nrow(master[master$year %in% 2005,]))) -
                                               (1-estimate_unc$par[2])*(estimate_unc$par[1] - 1)*(master$log_Model_consumption_index[master$year == 2005] - master$log_Model_consumption_index[master$year == 2000])


master$relspend_error_Gn[master$year == 2010] <- (master$log_relspend[master$year == 2010] - master$log_relspend[master$year == 2005]) - 
  (1-estimate_unc$par[2])*(master$log_relprice[master$year == 2010] - master$log_relprice[master$year == 2005] + log(rel_priceag_2010)*rep(1, nrow(master[master$year %in% 2010,]))) -
  (1-estimate_unc$par[2])*(estimate_unc$par[1] - 1)*(master$log_Model_consumption_index[master$year == 2010] - master$log_Model_consumption_index[master$year == 2005])



na_growth_2005 <- exp((1/((1-estimate_unc$par[2])*(estimate_unc$par[1] - 1)))*mean(master$relspend_error_Gn[master$year == 2005]))
na_growth_2010 <- exp((1/((1-estimate_unc$par[2])*(estimate_unc$par[1] - 1)))*mean(master$relspend_error_Gn[master$year == 2010]))
#conclusion-- nonag sector grew approximatley 56% from 2000-2005, 88% from 2005-2010 -- below official numbers of real gdp per capita growth. 
#Note: this growth does not include falling trade costs (which contribute separately to real gdp growth.) 

#Creating and exporting dataframe
export <- data.frame(c(theta), c(estimate_unc$par[1]), c(estimate_unc$par[2]), c(1/na_growth_2005), c(1/na_growth_2010), c(rel_priceag_2005), c(rel_priceag_2010))
colnames(export) <- c("Theta", "Epsilon", "Eta", "Gn_2005", "Gn_2010", "Gs/Gn_2005", "Gs/Gn_2010")


write_xlsx(export, path = "constructed_output/pref_estimates.xlsx")
