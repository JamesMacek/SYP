
library(ggplot2)
library(readxl)
library(tidyverse)
library(estimatr)


################PART 1######################################
### Setting Working Directory
theta=4
na_cap = 0.16
ag_cap = 0.06
#Prod_network. 
phi_an = 0.04 
phi_aa = 0.16 
phi_na = 0.25 
phi_nn = 0.61 
landshare_n = 0.01
landshare_a = 0.26
lab_na = 0.19
lab_ag = 0.27


setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")
#2002 trade flow data 
emp_GDP_data <- read_excel("master.xlsx")


#####################################################
#W/2005-2000

emp_GDP_data["log_outputg_ag_2005"] <- log((emp_GDP_data$Ag_nomY_2005/emp_GDP_data$Ag_nomY_2000)*(emp_GDP_data$invcost_ag_2005)^(1/theta)) - ag_cap*log(emp_GDP_data$Ka_2005/emp_GDP_data$Ka_2000) - lab_ag*log(emp_GDP_data$La2005/emp_GDP_data$La2000)
emp_GDP_data$log_outputg_ag_2005 <- emp_GDP_data$log_outputg_ag_2005 - phi_aa*log(emp_GDP_data$Ag_nomY_2005/emp_GDP_data$Ag_nomY_2000/(emp_GDP_data$pindex_ag_2005)^(1/theta)) - phi_na*log(emp_GDP_data$Na_nomY_2005/emp_GDP_data$Na_nomY_2000/(emp_GDP_data$pindex_na_2005)^(1/theta))

emp_GDP_data["log_outputg_na_2005"] <- log((emp_GDP_data$Na_nomY_2005/emp_GDP_data$Na_nomY_2000)*(emp_GDP_data$invcost_na_2005)^(1/theta)) - na_cap*log(emp_GDP_data$Kn_2005/emp_GDP_data$Kn_2000) - lab_na*log(emp_GDP_data$Ln2005/emp_GDP_data$Ln2000)
emp_GDP_data$log_outputg_na_2005 <- emp_GDP_data$log_outputg_na_2005 - phi_an*log(emp_GDP_data$Ag_nomY_2005/emp_GDP_data$Ag_nomY_2000/(emp_GDP_data$pindex_ag_2005)^(1/theta)) - phi_nn*log(emp_GDP_data$Na_nomY_2005/emp_GDP_data$Na_nomY_2000/(emp_GDP_data$pindex_na_2005)^(1/theta))

emp_GDP_data["log_emp_ag_2005"] <- log(emp_GDP_data$La2005/emp_GDP_data$La2000)
emp_GDP_data["log_emp_na_2005"] <- log(emp_GDP_data$Ln2005/emp_GDP_data$Ln2000)

ag_2005 <- lm_robust(log_outputg_ag_2005~log_emp_ag_2005, data = emp_GDP_data) #Shanghai is a clear outlier that makes the estimate skyrocket. 
na_2005 <- lm_robust(log_outputg_na_2005~log_emp_na_2005, data = emp_GDP_data)

############################
#2005-2010
emp_GDP_data["log_outputg_ag_2010"] <- log((emp_GDP_data$Ag_nomY_2010/emp_GDP_data$Ag_nomY_2005)*(emp_GDP_data$invcost_ag_2010)^(1/theta)) - ag_cap*log(emp_GDP_data$Ka_2010/emp_GDP_data$Ka_2005) 
emp_GDP_data$log_outputg_ag_2010 <- emp_GDP_data$log_outputg_ag_2010 - phi_aa*log(emp_GDP_data$Ag_nomY_2010/emp_GDP_data$Ag_nomY_2005/(emp_GDP_data$pindex_ag_2010)^(1/theta)) - phi_na*log(emp_GDP_data$Na_nomY_2010/emp_GDP_data$Na_nomY_2005/(emp_GDP_data$pindex_na_2010)^(1/theta))

emp_GDP_data["log_outputg_na_2010"] <- log((emp_GDP_data$Na_nomY_2010/emp_GDP_data$Na_nomY_2005)*(emp_GDP_data$invcost_na_2010)^(1/theta)) - na_cap*log(emp_GDP_data$Kn_2010/emp_GDP_data$Kn_2005) 
emp_GDP_data$log_outputg_na_2010 <- emp_GDP_data$log_outputg_na_2010 - phi_an*log(emp_GDP_data$Ag_nomY_2010/emp_GDP_data$Ag_nomY_2005/(emp_GDP_data$pindex_ag_2010)^(1/theta)) - phi_nn*log(emp_GDP_data$Na_nomY_2010/emp_GDP_data$Na_nomY_2005/(emp_GDP_data$pindex_na_2010)^(1/theta))

emp_GDP_data["log_emp_ag_2010"] <- log(emp_GDP_data$La2010/emp_GDP_data$La2005)
emp_GDP_data["log_emp_na_2010"] <- log(emp_GDP_data$Ln2010/emp_GDP_data$Ln2005)


#Assume land unmoving.
ag_2010 <- lm_robust(log_outputg_ag_2010~log_emp_ag_2010, data = emp_GDP_data) #Shanghai is a clear outlier that makes the estimates skyrocket. 
na_2010 <- lm_robust(log_outputg_na_2010~log_emp_na_2010, data = emp_GDP_data)

####################################################


emp_GDP_data["log_outputg_ag_t"] <- log((emp_GDP_data$Ag_nomY_2010/emp_GDP_data$Ag_nomY_2000)*(emp_GDP_data$invcost_ag_2010*emp_GDP_data$invcost_ag_2005)^(1/theta)) - ag_cap*log(emp_GDP_data$Ka_2010/emp_GDP_data$Ka_2000) 
emp_GDP_data$log_outputg_ag_t <- emp_GDP_data$log_outputg_ag_t - phi_aa*log(emp_GDP_data$Ag_nomY_2010/emp_GDP_data$Ag_nomY_2000/(emp_GDP_data$pindex_ag_2010*emp_GDP_data$pindex_ag_2005)^(1/theta)) - phi_na*log(emp_GDP_data$Na_nomY_2010/emp_GDP_data$Na_nomY_2000/(emp_GDP_data$pindex_na_2010*emp_GDP_data$pindex_na_2005)^(1/theta))

emp_GDP_data["log_outputg_na_t"] <- log((emp_GDP_data$Na_nomY_2010/emp_GDP_data$Na_nomY_2000)*(emp_GDP_data$invcost_na_2010*emp_GDP_data$invcost_na_2005)^(1/theta)) - na_cap*log(emp_GDP_data$Kn_2010/emp_GDP_data$Kn_2000) 
emp_GDP_data$log_outputg_na_t <- emp_GDP_data$log_outputg_na_t - phi_an*log(emp_GDP_data$Ag_nomY_2010/emp_GDP_data$Ag_nomY_2000/(emp_GDP_data$pindex_ag_2010*emp_GDP_data$pindex_ag_2005)^(1/theta)) - phi_nn*log(emp_GDP_data$Na_nomY_2010/emp_GDP_data$Na_nomY_2000/(emp_GDP_data$pindex_na_2010*emp_GDP_data$pindex_na_2005)^(1/theta))

emp_GDP_data["log_emp_ag_t"] <- log(emp_GDP_data$La2010/emp_GDP_data$La2000)
emp_GDP_data["log_emp_na_t"] <- log(emp_GDP_data$Ln2010/emp_GDP_data$Ln2000)


#Assume land unmoving.
ag <- lm_robust(log_outputg_ag_t~log_emp_ag_t, data = emp_GDP_data[!emp_GDP_data$province %in% "Shanghai", ]) #Shanghai is a clear outlier that makes the estimate skyrocket. become negative!!!1!
na <- lm_robust(log_outputg_na_t~log_emp_na_t, data = emp_GDP_data)

#some plots for 2005
#####################################################################################
##WHY THE DOWNWARD BIAS...? Catch up growth. Residual productivity growth NOT ASSIGNED RANDOMLY-- it appears to be assigned more to lagging places!
##...
##BUT AGRICULTURE IS SLIGHTLY DIFFERENT. MORE DISEMPLOYMENT IF HIGHER RESIDUAL GROWTH BECAUSE OF STRUCTURAL CHANGE. 
##All this with ridiculous sample size :()
ggplot(emp_GDP_data, aes(x=log_emp_na_2005, y=log_outputg_na_2005)) + geom_point() +
  geom_text(label=emp_GDP_data$province, nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") 
ggplot(emp_GDP_data, aes(x=log_emp_ag_2005, y=log_outputg_ag_2005)) + geom_point() +
  geom_text(label=emp_GDP_data$province, nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") 
ggplot(emp_GDP_data, aes(x=log_emp_na_2010, y=log_outputg_na_2010)) + geom_point() +
  geom_text(label=emp_GDP_data$province, nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") 

##Correlation between output per cap and worker growth is by and large negative... interesting. Migration closed gaps. 
emp_GDP_data["log_GDPworker_ag_growth_2005"] <- log(emp_GDP_data$Ag_nomY_2005/emp_GDP_data$Ag_nomY_2000) - emp_GDP_data$log_emp_ag_2005
emp_GDP_data["log_GDPworker_na_growth_2005"] <- log(emp_GDP_data$Na_nomY_2005/emp_GDP_data$Na_nomY_2000) - emp_GDP_data$log_emp_na_2005
emp_GDP_data["log_GDPworker_na_growth_2010"] <- log(emp_GDP_data$Na_nomY_2010/emp_GDP_data$Na_nomY_2005) - emp_GDP_data$log_emp_na_2010

ggplot(emp_GDP_data, aes(x=log_emp_ag_2005, y=log_GDPworker_ag_growth_2005)) + geom_point() +
  geom_text(label=emp_GDP_data$province, nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") 
ggplot(emp_GDP_data, aes(x=log_emp_na_2005, y=log_GDPworker_na_growth_2005)) + geom_point() +
  geom_text(label=emp_GDP_data$province, nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") 
ggplot(emp_GDP_data, aes(x=log_emp_na_2010, y=log_GDPworker_na_growth_2010)) + geom_point() +
  geom_text(label=emp_GDP_data$province, nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") 
