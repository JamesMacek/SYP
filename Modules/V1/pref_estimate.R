
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

phi_an = 0.04 
phi_aa = 0.16 
phi_na = 0.25 
phi_nn = 0.61 


land_na = 0.01
land_ag = 0.26
lab_na = 0.19
lab_ag = 0.27
cap_na = 0.15
cap_ag = 0.06

theta = 4 #Simonovska and Waugh (20xx)

#Estimates epsilon_a and eta as per the paper.
#Date created: May 28th, 2021
#Date edited: October 7th, 2021

loadBootstrap = 1 #Set to 0 if you want to run bootstrap command. 
loadSensitivity = 1 #Set to 0 to run sensitivity analyses.

setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")

#read output Construct_....R and rel_prod_estimate.m
master <- read_excel("constructed_output/master_stacked2.xlsx")

#Removing international observation-- expenditure not well measured in this case.
master <- master[!master$province %in% "International",]

#Creating data frame vars for estimation. 
#Choosing non-agriculture as a base sector. 
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
master["log_EPa"] <- log(master$Va_perworker) - log(master$pindex_na)
master["FE_2005"] <- ifelse(master$year == 2005, 1, 0)
master["FE_2010"] <- ifelse(master$year == 2010, 1, 0)
master["FE_ag"] <- ifelse(master$sector == "ag", 1, 0)



#______REGRESSIONS_____#
#With income per capita omitted.
share_omitted <- lm_robust(log_relspend~log_relprice + FE_2005 + FE_2010 + FE_ag, data = master, cluster = year,  se_type = "stata", alpha = 0.05)

#Regression with relative spending vs real_gdp measures in Tombe and Zhu (2019)
share_reg <- lm_robust(log_relspend~log_relprice + log_rY + FE_2005 + FE_2010 + FE_ag, data = master, cluster = year,  se_type = "stata", alpha = 0.05)

#Rel employment regression
relemp_reg <- lm_robust(log_relemp~log_relprice + log_rY + FE_2005 + FE_2010 + FE_ag, data = master, cluster = year,  se_type = "stata", alpha = 0.05)

#Subsetting data if estimating using relative employment specification
master_sub_na <- master[master$sector %in% "na",]
master_sub_ag <- master[master$sector %in% "ag",]

#rel_employment/spend specification for each subsample
relemp_reg_na <- lm_robust(log_relemp~log_relprice + log_rY + FE_2005 + FE_2010, data = master_sub_na, cluster = year,  se_type = "stata", alpha = 0.05)
relemp_reg_ag <- lm_robust(log_relemp~log_relprice + log_rY + FE_2005 + FE_2010, data = master_sub_ag, cluster = year,  se_type = "stata", alpha = 0.05)
share_reg_ag <- lm_robust(log_relspend~log_relprice + log_rY + FE_2005 + FE_2010, data = master_sub_ag, cluster = year,  se_type = "stata", alpha = 0.05)
share_reg_na <- lm_robust(log_relspend~log_relprice + log_rY + FE_2005 + FE_2010, data = master_sub_na, cluster = year,  se_type = "stata", alpha = 0.05)
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
  t05_v <- colSums(as.matrix(2*data$Errors*(-data$FE_2005)))
  t10_v <- colSums(as.matrix(2*data$Errors*(-data$FE_2010)))
  t00_v <- colSums(as.matrix(2*data$Errors*(-1)))
  return(c(eps_v, eta_v, t05_v, t10_v, t00_v))
}
estimate_unc <- fminunc(x0 = ini, fn=obj_fn, gr = grad_fn,  maxiter = 100, maxfeval = 100)
return(estimate_unc)

}
#_________________________________________________________________________________________

init <- c(2, 1.1, 0, 0, 0) #Doesn't matter, estimate globally convergent. 

estimate_unc <- estimate_un(master_sub_na, init) #prefer to estimate on subsetted data since no variation of relative employment within provinces. 

#Statistic function for bootstrap
estimate_un_boot <- function(data, indices) {
  resampled_data <- data[indices, ]
  estimate_unc <- estimate_un(resampled_data, init)
  return(estimate_unc$par)
}

#BOOTSTRAPPING OUTPUT 
set.seed(4321)
if (loadBootstrap == 0) {
  estimate_unc_bootstrap <- boot(data = master_sub_na, statistic = estimate_un_boot, R = 5000)
  #saving bootstrap output
  save(estimate_unc_bootstrap, file = "constructed_output/unc_bootstrap_Robject.Rda")  
}  else {
  load("constructed_output/unc_bootstrap_Robject.Rda")
}

#confidence intervals
boot.ci(estimate_unc_bootstrap, conf = 0.99, index = 1, type = "all")
boot.ci(estimate_unc_bootstrap, conf = 0.99, index = 2, type = "all")
boot.ci(estimate_unc_bootstrap, conf = 0.99, index = 3, type = "all")
boot.ci(estimate_unc_bootstrap, conf = 0.99, index = 4, type = "all")
boot.ci(estimate_unc_bootstrap, conf = 0.99, index = 5, type = "all")
boot.ci(estimate_unc_bootstrap, conf = 0.99, index = 6, type = "all")
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
  return(log(y) - epsilon*log(1-y) - (epsilon - 1)*(1-etaa)*data$log_EPa[ig] - (1-etaa)*data$log_relprice[ig] - f05*data$FE_2005[ig] - f10*data$FE_2010[ig] - fa*data$FE_ag[ig] - interc)
  }

  nls_inverse <- function(x) {
  #Creates function and solves for inverse.
  #Globals for use with fn_solution function. 
    epsilon <<- x[1]
    etaa <<- x[2]
    f05 <<- x[3]
    f10 <<- x[4]
    fa <<- x[5]
    interc <<- x[6]
  
    sln <- uniroot(f = fn_tosolve, interval = slnint) 
    return(sln$root)
  }

#Objective function
  nls_obj_fn <- function(x) {
  
   for (i in 1:nrow(data)) {
     ig <<- i # setting global to use with function
     data$NLS_Errors[ig] <- data$agspend[ig] - nls_inverse(x)
    }
  
    SSE <- colSums(as.matrix(data$NLS_Errors)^2) 
    return(SSE)
  
  }
  #final output
  data["NLS_Errors"] <- rep(NA, nrow(data))
  estimate_nls <- fmincon(x0 = ini, fn = nls_obj_fn, lb = c(0.001, 0.001, -50, -50, -50, -50))
  return(estimate_nls)
}
#____________________________________________________________________________________________________________
#estimating

init <- c(2, 1.1, 0, 0, 0, 0) #Set initial value near best point estimate

#fmincon appears to have the highest performance and speed here, even without supplying gradient.
estimate_nls <- estimate_nlscon(master, init) 

#Sensitivity analysis for point estimate using random initial conditions. 
set.seed(4231)

if (loadSensitivity == 0) {
sensitivity = list()
#Sample of 50 initial conditions. Takes quite long to run.  
for (r in c(1:50)) {  
  ie = abs(rnorm(2, sd=1, mean=1))  #Initial values centered at (about) 1 and forced positive for eps and eta
  ir = rnorm(4, sd=2, mean=0) # Intercept terms can be any number centered at 0 (normal)
  init_loop = c(ie, ir)
  estimate_nls_loop <- try(estimate_nlscon(master, init_loop), silent = FALSE)
  if(inherits(estimate_nls_loop, "try-error"))
  {
    next #Skips if fmincon fails (due to machine precision error?)
  }
  sensitivity[[r]] <- c(estimate_nls_loop$par, estimate_nls_loop$value) #Looks good! seems we found a global maximum. 
  }
}


#Statistic function for bootstrap
estimate_nls_boot <- function(data, indices) {
  resampled_data <- data[indices, ]
  estimate <- estimate_nlscon(resampled_data, init)
  return(estimate$par)
}

set.seed(4321)
if (loadBootstrap == 0) {
  estimate_nls_bootstrap <- boot(data = master, statistic = estimate_nls_boot, R = 1000)
  #saving bootstrap output
  save(estimate_nls_bootstrap, file = "constructed_output/nls_bootstrap_Robject.Rda")  
  
}  else {
  load("constructed_output/nls_bootstrap_Robject.Rda")
}

boot.ci(estimate_nls_bootstrap, conf = 0.99, index = 1, type = "all")
boot.ci(estimate_nls_bootstrap, conf = 0.99, index = 2, type = "all")
boot.ci(estimate_nls_bootstrap, conf = 0.99, index = 3, type = "all")
boot.ci(estimate_nls_bootstrap, conf = 0.99, index = 4, type = "all")
boot.ci(estimate_nls_bootstrap, conf = 0.99, index = 5, type = "all")
boot.ci(estimate_nls_bootstrap, conf = 0.99, index = 6, type = "all")
print(estimate_nls_bootstrap)

#Correlation with model implied index and actual observed real GDP measures performs well w/ entire sample.
master["log_Model_consumption_index_nls"] <- master$log_EPa + (1/(1-estimate_nls$par[2]))*master$log_naspend
check_reg2 <- lm_robust(log_Model_consumption_index_nls~log_rY, data = master)

setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/Writeups/Drafts")
#Exporting with GGplot
ggplot(master, aes(x=log_rY, y=log_Model_consumption_index_nls)) + geom_point(aes(colour=sector))  + 
  geom_smooth(method="lm") + labs(x="Log(Real GDP per capita)", y="Log(Model Implied Utility Index)")
ggsave("model_implied_index.png")
setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")

#___________________________________________________________________________________________________________________
#PART 3: Estimating productivity growth assuming change in relative prices from Groningen 10 sector database

setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")
load("constructed_outputV2/d_g10s_constructed.Rda")
groningen_10s_CHN <- d_g10s_constructed[d_g10s_constructed$Country %in% "CHN",]
rm(d_g10s_constructed)
groningen_10s_CHN <- clean_names(groningen_10s_CHN)
#constructing China wide na price index-- vashare weighted average of price indices in all other 7 sectors (mining incl in primary industry) 
#Omit government services because no price data. 
#Keep consumption weights at 2005 levels (i.e. use only 2005 levels of value added since prices are expressed relative to 2005)
groningen_10s_CHN["Price_na"] <- (groningen_10s_CHN$manufacturing_price*(groningen_10s_CHN$manufacturing_v_ashare[groningen_10s_CHN$year == 2005]) +
                                    groningen_10s_CHN$utilities_price*(groningen_10s_CHN$utilities_v_ashare[groningen_10s_CHN$year == 2005]) +
                                    groningen_10s_CHN$construction_price*(groningen_10s_CHN$construction_v_ashare[groningen_10s_CHN$year == 2005]) +
                                    groningen_10s_CHN$trade_restaurants_and_hotels_price*(groningen_10s_CHN$trade_restaurants_and_hotels_v_ashare[groningen_10s_CHN$year == 2005]) +
                                    groningen_10s_CHN$transport_storage_and_communication_price*(groningen_10s_CHN$transport_storage_and_communication_v_ashare[groningen_10s_CHN$year == 2005]) +
                                    groningen_10s_CHN$finance_insurance_real_estate_and_business_services_price*(groningen_10s_CHN$finance_insurance_real_estate_and_business_services_v_ashare[groningen_10s_CHN$year == 2005]) +
                                    groningen_10s_CHN$community_social_and_personal_services_price*(groningen_10s_CHN$community_social_and_personal_services_v_ashare[groningen_10s_CHN$year == 2005]))

#Adjusting for VASHARE of all these sectors
groningen_10s_CHN$Price_na <- groningen_10s_CHN$Price_na/(1 - groningen_10s_CHN$agriculture_v_ashare[groningen_10s_CHN$year == 2005] - groningen_10s_CHN$mining_v_ashare[groningen_10s_CHN$year == 2005] - groningen_10s_CHN$government_services_v_ashare[groningen_10s_CHN$year == 2005])

#Calculating the implied relative increase of agricultural prices
rel_price_china <- as.matrix(groningen_10s_CHN$agriculture_price/groningen_10s_CHN$Price_na)
row.names(rel_price_china) <- as.matrix(groningen_10s_CHN$year)
rm(groningen_10s_CHN)

#Aggregate relative price increase
drel_priceag_2005 <- rel_price_china["2005",]/rel_price_china["2000",]
drel_priceag_2010 <- rel_price_china["2010",]/rel_price_china["2005",]
rm(rel_price_china)

drel_priceag_2005 <- unname(drel_priceag_2005) #rel price of ag increased by 8% in aggregate 2000-2005
drel_priceag_2010 <- unname(drel_priceag_2010) #rel price of ag increased by 19% in aggregate 2005-2010


#Objects: G_n and G_a are the inverse scaling factors of prices relative to numeraire. Can recover these to fit fall in agricultural spending. 
#Choosing G_n and  to match the average fall in relative agricultural spending in the data over provinces from 2000-2005 and 2005-2010.
#NOTE: estimates of Gn will depend on which estimates of eta and epsilon are chosen!

#Solving for G_a/G_n (exp of discrepancy between log drelprice in Groningen 10 sector and average difference in log price index)
relGa_2005 <-  1/(exp(log(drel_priceag_2005) - mean(log(master$dpindex_ag[master$year==2005]) - log(master$dpindex_na[master$year==2005])))) #Barely any change in 2000!
relGa_2010 <- 1/(exp(log(drel_priceag_2010) - mean(log(master$dpindex_ag[master$year==2010]) - log(master$dpindex_na[master$year==2010]))))

#Gn using NLS estimates. Make sense! 
Gn_2005_nls = 1/(exp((mean(master$log_agspend[master$year == 2005] - master$log_agspend[master$year == 2000]) - estimate_nls$par[1]*mean((master$log_naspend[master$year == 2005] - master$log_naspend[master$year == 2000])) -
                        (1-estimate_nls$par[2])*log(drel_priceag_2005) -
                        (1-estimate_nls$par[2])*(estimate_nls$par[1] - 1)*mean(master$log_EPa[master$year==2005] - master$log_EPa[master$year==2000]))/((1-estimate_nls$par[2])*(1 - estimate_nls$par[1]))))

Gn_2010_nls = 1/(exp((mean(master$log_agspend[master$year == 2010] - master$log_agspend[master$year == 2005]) - estimate_nls$par[1]*mean((master$log_naspend[master$year == 2010] - master$log_naspend[master$year == 2005])) -
                        (1-estimate_nls$par[2])*log(drel_priceag_2010) -
                        (1-estimate_nls$par[2])*(estimate_nls$par[1] - 1)*mean(master$log_EPa[master$year==2010] - master$log_EPa[master$year==2005]))/((1-estimate_nls$par[2])*(1 - estimate_nls$par[1]))))



#Gn using unconstrained estimates. Absolute nonsense results! Elasticity of substitution insanely high. 
Gn_2005_unc = 1/(exp((mean(master$log_agspend[master$year == 2005] - master$log_agspend[master$year == 2000]) - estimate_unc$par[1]*mean((master$log_naspend[master$year == 2005] - master$log_naspend[master$year == 2000])) -
                        (1-estimate_unc$par[2])*log(drel_priceag_2005) -
                        (1-estimate_unc$par[2])*(estimate_unc$par[1] - 1)*mean(master$log_EPa[master$year==2005] - master$log_EPa[master$year==2000]))/((1-estimate_unc$par[2])*(1 - estimate_unc$par[1]))))

Gn_2010_unc = 1/(exp((mean(master$log_agspend[master$year == 2010] - master$log_agspend[master$year == 2005]) - estimate_unc$par[1]*mean((master$log_naspend[master$year == 2010] - master$log_naspend[master$year == 2005])) -
                        (1-estimate_unc$par[2])*log(drel_priceag_2010) -
                        (1-estimate_unc$par[2])*(estimate_unc$par[1] - 1)*mean(master$log_EPa[master$year==2010] - master$log_EPa[master$year==2005]))/((1-estimate_unc$par[2])*(1 - estimate_unc$par[1]))))


#How much was the geometric average fall in prices in each sector for NLS estimates in 2005? 
avg_logfall_na_nls <- exp(mean(log(master$dpindex_na[master$year==2005])))/Gn_2005_nls 
avg_logfall_ag_nls <- exp(mean(log(master$dpindex_ag[master$year==2005])))/(Gn_2005_nls*relGa_2005)
avg_logfall_na_unc <- exp(mean(log(master$dpindex_na[master$year==2005])))/Gn_2005_unc
avg_logfall_ag_unc <- exp(mean(log(master$dpindex_ag[master$year==2005])))/(Gn_2005_unc*relGa_2005)
#Relative fall is designed to agree with the Groningen data. 


#What about eta = 0.7 and epsilon = 0.1 as in Comin et al and others?


#Creating and exporting dataframe
export <- data.frame(c(theta), c(estimate_nls$par[1]), c(estimate_nls$par[2]), c(Gn_2005_nls), c(Gn_2010_nls), c(relGa_2005), c(relGa_2010), c(estimate_unc$par[1]), c(estimate_unc$par[2]), c(Gn_2005_unc), c(Gn_2010_unc))
colnames(export) <- c("Theta", "Epsilon_nls", "Eta_nls", "Gn_2005_nls", "Gn_2010_nls", "Ga/Gn_2005", "Ga/Gn_2010", "Epsilon_unc", "Eta_unc", "Gn_2005_unc", "Gn_2010_unc")
write_xlsx(export, path = "constructed_output/pref_estimates.xlsx")



