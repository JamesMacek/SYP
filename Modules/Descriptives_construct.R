
library(ggplot2)
library(readxl)
library(tidyverse)
library(estimatr)
library(simpleboot)
library(Hmisc)


#Date created: March 22nd, 2021
#Date modified: July 21st, 2021

N=30 #No. provinces.
################PART 1######################################
### Setting Working Directory

setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData")
#2002 trade flow data 
tr_ag_2002 <- read_excel("2002_ag_tradeflows.xlsx")
tr_na_2002 <- read_excel("2002_na_tradeflows.xlsx")
emp_GDP_data <- read_excel("master_raw.xlsx")

#constructing dataframe for plot
data2plot <- tr_ag_2002[, 1]

##To Clear
ag_home <- diag(as.matrix(tr_ag_2002[, -1]))
na_home <- diag(as.matrix(tr_na_2002[, -1]))
##

#Getting all the vars together 
data2plot["Ag Home Share"] <- ag_home 
data2plot["Non-Ag Home Share"] <- na_home
data2plot["La2000"] <- emp_GDP_data$L_ag_2000
data2plot["Ln2000"] <- emp_GDP_data$L_na_2000
data2plot["La2005"] <- emp_GDP_data$L_ag_2005
data2plot["Ln2005"] <- emp_GDP_data$L_na_2005
data2plot["La2010"] <- emp_GDP_data$L_ag_2010
data2plot["Ln2010"] <- emp_GDP_data$L_na_2010


data2plot["Landmass"] <- (1/1000)*as.matrix(emp_GDP_data["landmass"])
data2plot["Ag_Share_2000"] <- as.matrix(data2plot["La2000"])/(as.matrix(data2plot["La2000"]) + as.matrix(data2plot["Ln2000"]))
data2plot["Ag_Na_Rel_spend"] <- as.matrix(data2plot["Ag Home Share"])/(as.matrix(data2plot["Non-Ag Home Share"]))
data2plot["Emp_density_2000"] <- (data2plot$La2000 + data2plot$Ln2000)/(as.matrix(data2plot["Landmass"]))
data2plot["Emp_density_2005"] <- (data2plot$La2005 + data2plot$Ln2005)/data2plot$Landmass
data2plot["Emp_density_2010"] <- (data2plot$La2010 + data2plot$Ln2010)/data2plot$Landmass
data2plot["Emp_2000"] <- data2plot$La2000 + data2plot$Ln2000
data2plot["Emp_2005"] <- data2plot$La2005 + data2plot$Ln2005
data2plot["Emp_2010"] <- data2plot$La2010 + data2plot$Ln2010

data2plot <- data.frame(data2plot[, -1], row.names=emp_GDP_data$province)

####Plot data for fig 1 - relationship between employment density and agriculture's share of employment. 
ggplot(data2plot, aes(x=Emp_density_2000, y=Ag_Share_2000)) +
geom_point() + geom_text(label=rownames(data2plot), nudge_x=0.15, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
geom_smooth(method="lm") + labs(x ="Employment Density in 2000", y = "Agricultural Employment Share, 2000")
ggsave("fig1.png")

#####################PART 2########################
data2plot["Ag_share_2005"] <- as.matrix(data2plot["La2005"])/(as.matrix(data2plot["La2005"]) + as.matrix(data2plot["Ln2005"]))
#Explore employment growth from 2000 - 2005 (and beyond) as fn of initial pop density
data2plot["na_2005_growth"] <- data2plot$Ln2005/data2plot$Ln2000 - 1*rep(1, nrow(data2plot))
data2plot["ag_2005_growth"] <- data2plot$La2005/data2plot$La2000 - 1*rep(1, nrow(data2plot))
data2plot["na_2010_growth"] <- data2plot$Ln2010/data2plot$Ln2005 - 1*rep(1, nrow(data2plot))
data2plot["ag_2010_growth"] <- data2plot$La2010/data2plot$La2005 - 1*rep(1, nrow(data2plot))
data2plot["Emp_2005_growth"] <- (data2plot$La2005 + data2plot$Ln2005)/(data2plot$La2000 + data2plot$Ln2000) -  1*rep(1, nrow(data2plot))
data2plot["Emp_2010_growth"] <- (data2plot$La2010 + data2plot$Ln2010)/(data2plot$La2005 + data2plot$Ln2005) -  1*rep(1, nrow(data2plot))

#Testing linear model - Employment growth against employment density
#2005
emp_glm_2005 <-  lm_robust(Emp_2005_growth ~ Emp_density_2000, data=data2plot) 
emp_glm_2005_w <- lm_robust(Emp_2005_growth ~ Emp_density_2000, weights = Emp_2000 , data=data2plot) 
emp_glm_2005_nosh <- lm_robust(Emp_2005_growth ~ Emp_density_2000, weights = Emp_2000, data=data2plot[!row.names(data2plot) %in% "Shanghai" & !row.names(data2plot) %in% "Beijing",]) #One observation makes standard errors go way up here.  #Outliers (somewhat)

#2010
emp_glm_2010 <- lm_robust(Emp_2010_growth ~ Emp_density_2005, data=data2plot)
emp_glm_2010_w <- lm_robust(Emp_2010_growth ~ Emp_density_2005, weights = Emp_2005, data=data2plot)

ggplot(data2plot, aes(x=Emp_density_2000, y=Emp_2005_growth)) + geom_point() +
  geom_text(label=rownames(data2plot), nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") + labs(x="Employment Density, 2000", y="Employment growth, 2000-2005")
ggsave("emp_growth_density.png")



#Decompose growth rates into two components (Agriculture and Non-agriculture)
data2plot["ag_emp_cont_2005"] <- (data2plot$La2005 - data2plot$La2000)/(data2plot$La2000 + data2plot$Ln2000)
data2plot["na_emp_cont_2005"] <- (data2plot$Ln2005 - data2plot$Ln2000)/(data2plot$La2000 + data2plot$Ln2000)
data2plot["ag_emp_cont_2010"] <- (data2plot$La2010 - data2plot$La2005)/(data2plot$La2005 + data2plot$Ln2005)
data2plot["na_emp_cont_2010"] <- (data2plot$Ln2010 - data2plot$Ln2005)/(data2plot$La2005 + data2plot$Ln2005)

#Check how growth rates in ag/nonag are contributing.  

ag_glm_2005_w <- lm_robust(ag_emp_cont_2005 ~ Emp_density_2000, data=data2plot)
ag_glm_2005_w <- lm_robust(ag_emp_cont_2005 ~ Emp_density_2000, weights = Emp_2000, data=data2plot)

na_glm_2005 <- lm_robust(na_emp_cont_2005 ~ Emp_density_2000, data=data2plot)
na_glm_2005_w <-  lm_robust(na_emp_cont_2005 ~ Emp_density_2000, weights = Emp_2000, data=data2plot)
na_glm_2005_nosh <-  lm_robust(na_emp_cont_2005 ~ Emp_density_2000, weights = Emp_2000, data=data2plot[!row.names(data2plot) %in% "Shanghai" & !row.names(data2plot) %in% "Beijing",]) #Outliers driving the magnitude of these regressions.

#Weighted only for 2010
ag_glm_2010_w <- lm_robust(ag_emp_cont_2010~Emp_density_2005, weights = Emp_2005, data=data2plot)
na_glm_2010_w <-  lm_robust(na_emp_cont_2010 ~ Emp_density_2005, weights = Emp_2005, data=data2plot)

ggplot(data2plot, aes(x=Emp_density_2000, y=na_emp_cont_2005)) + geom_point() +
  geom_text(label=rownames(data2plot), nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") + labs(x="Employment Density, 2000", y="Non-Ag Contribution to Employment growth, 2000-2005")
ggsave("na_cont_growth_density.png")

ggplot(data2plot, aes(x=Emp_density_2000, y=ag_emp_cont_2005)) + geom_point() +
  geom_text(label=rownames(data2plot), nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") + labs(x="Employment Density, 2000", y="Agriculture Contribution to Employment growth, 2000-2005")
ggsave("ag_cont_growth_density.png")

#####################################################################################

##Checking data implied structural change
data2plot_noint <- data2plot[!row.names(data2plot) %in% "International",]
Aggregate_Ag_Share_2000 <- colSums(as.matrix(data2plot_noint$La2000))/(colSums(as.matrix(data2plot_noint$La2000)) + colSums(as.matrix(data2plot_noint$Ln2000)))
Aggregate_Ag_Share_2005 <- colSums(as.matrix(data2plot_noint$La2005))/(colSums(as.matrix(data2plot_noint$La2005)) + colSums(as.matrix(data2plot_noint$Ln2005))) #Fell 8%
Aggregate_Ag_Share_2010 <- colSums(as.matrix(data2plot_noint$La2010))/(colSums(as.matrix(data2plot_noint$La2010)) + colSums(as.matrix(data2plot_noint$Ln2010))) #Fell 8%


##Checking Population Density Dispersion at provincial level. Coefficient of variation of population density
data2plot_noint <- data2plot[!row.names(data2plot) %in% "International",]

#Unweighted density dispersion
dens_disp_2000 <- sqrt((N-1)/N)*sd(data2plot_noint$Emp_density_2000)/mean(data2plot_noint$Emp_density_2000)
dens_disp_2005 <- sqrt((N-1)/N)*sd(data2plot_noint$Emp_density_2005)/mean(data2plot_noint$Emp_density_2005)
dens_disp_2010 <- sqrt((N-1)/N)*sd(data2plot_noint$Emp_density_2010)/mean(data2plot_noint$Emp_density_2010)

#Employment weighted density dispersion
empdens_disp_2000 <- sqrt(wtd.var(data2plot_noint$Emp_density_2000, weights = data2plot_noint$Emp_2000, method = c("ML")))/(wtd.mean(data2plot_noint$Emp_density_2000, weights = data2plot_noint$Emp_2000)) #No Basel correction used here - c("ML")
empdens_disp_2005 <- sqrt(wtd.var(data2plot_noint$Emp_density_2005, weights = data2plot_noint$Emp_2005, method = c("ML")))/(wtd.mean(data2plot_noint$Emp_density_2005, weights = data2plot_noint$Emp_2005))
empdens_disp_2010 <- sqrt(wtd.var(data2plot_noint$Emp_density_2010, weights = data2plot_noint$Emp_2010, method = c("ML")))/(wtd.mean(data2plot_noint$Emp_density_2010, weights = data2plot_noint$Emp_2010))

#Similar patterns. 

#Increases by 12 pp. There is a 12 ppoint increase in sd relative to mean of population density across provinces. 

#Checking how much agricultural spending shares fell, how this related to pop density/realGDP
emp_GDP_data_noint <- emp_GDP_data[!emp_GDP_data$province %in% "International",]

#GDP weighted average of fall in ag spending shares (or fall in aggregate ag spending share)
avgspend_2000 <- sum(emp_GDP_data_noint$nomY_ag_2000)/(sum(emp_GDP_data_noint$nomY_ag_2000) + sum(emp_GDP_data_noint$nomY_na_2000))*wtd.mean(emp_GDP_data_noint$agspend_ag_2000, weights = emp_GDP_data_noint$nomY_ag_2000) + 
                 sum(emp_GDP_data_noint$nomY_na_2000)/(sum(emp_GDP_data_noint$nomY_ag_2000) + sum(emp_GDP_data_noint$nomY_na_2000))*wtd.mean(emp_GDP_data_noint$agspend_na_2000, weights = emp_GDP_data_noint$nomY_na_2000)

avgspend_2005 <- sum(emp_GDP_data_noint$nomY_ag_2005)/(sum(emp_GDP_data_noint$nomY_ag_2005) + sum(emp_GDP_data_noint$nomY_na_2005))*wtd.mean(emp_GDP_data_noint$agspend_ag_2005, weights = emp_GDP_data_noint$nomY_ag_2005) + 
                 sum(emp_GDP_data_noint$nomY_na_2005)/(sum(emp_GDP_data_noint$nomY_ag_2005) + sum(emp_GDP_data_noint$nomY_na_2005))*wtd.mean(emp_GDP_data_noint$agspend_na_2005, weights = emp_GDP_data_noint$nomY_na_2005)

avgspend_2010 <- sum(emp_GDP_data_noint$nomY_ag_2010)/(sum(emp_GDP_data_noint$nomY_ag_2010) + sum(emp_GDP_data_noint$nomY_na_2010))*wtd.mean(emp_GDP_data_noint$agspend_ag_2010, weights = emp_GDP_data_noint$nomY_ag_2010) + 
                 sum(emp_GDP_data_noint$nomY_na_2010)/(sum(emp_GDP_data_noint$nomY_ag_2010) + sum(emp_GDP_data_noint$nomY_na_2010))*wtd.mean(emp_GDP_data_noint$agspend_na_2010, weights = emp_GDP_data_noint$nomY_na_2010)

#Fell 3 percent per year approximately (average across provinces) 31 - 25% over 10 year period.



