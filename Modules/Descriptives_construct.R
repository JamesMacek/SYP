
library(ggplot2)
library(readxl)
library(tidyverse)
library(estimatr)


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
data2plot["La2000"] <- emp_GDP_data["La2000"]
data2plot["Ln2000"] <- emp_GDP_data["Ln2000"]
data2plot["La2005"] <- emp_GDP_data["La2005"]
data2plot["Ln2005"] <- emp_GDP_data["Ln2005"]
data2plot["Landmass"] <- (1/1000)*as.matrix(emp_GDP_data["Landmass (sqm)"])
data2plot["Ag_Share_2000"] <- as.matrix(data2plot["La2000"])/(as.matrix(data2plot["La2000"]) + as.matrix(data2plot["Ln2000"]))
data2plot["Ag_Na_Rel_spend"] <- as.matrix(data2plot["Ag Home Share"])/(as.matrix(data2plot["Non-Ag Home Share"]))
data2plot["Emp_density_2000"] <- (as.matrix(data2plot["La2000"]) + as.matrix(data2plot["Ln2000"]))/(as.matrix(data2plot["Landmass"]))

data2plot <- data.frame(data2plot[, -1], row.names=emp_GDP_data$province)



####Plot data for fig 1
ggplot(data2plot, aes(x=Emp_density_2000, y=Ag_Share_2000)) +
geom_point() + geom_text(label=rownames(data2plot), nudge_x=0.15, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
geom_smooth(method="lm") + labs(x ="Employment Density in 2000", y = "Agricultural Employment Share, 2000")
ggsave("fig1.png")



#####################PART 2########################
data2plot["Ag_share_2005"] <- as.matrix(data2plot["La2005"])/(as.matrix(data2plot["La2005"]) + as.matrix(data2plot["Ln2005"]))
#Explore employment growth from 2000 - 2005 (and beyond) as fn of initial pop density
data2plot["na_2005_growth"] <- emp_GDP_data["Ln2005"]/emp_GDP_data["Ln2000"] - 1*rep(1, nrow(data2plot))
data2plot["ag_2005_growth"] <- emp_GDP_data["La2005"]/emp_GDP_data["La2000"] - 1*rep(1, nrow(data2plot))
data2plot["na_2010_growth"] <- emp_GDP_data["Ln2010"]/emp_GDP_data["Ln2005"] - 1*rep(1, nrow(data2plot))
data2plot["ag_2010_growth"] <- emp_GDP_data["La2010"]/emp_GDP_data["La2005"] - 1*rep(1, nrow(data2plot))
data2plot["Emp_2005_growth"] <- (emp_GDP_data["La2005"] + emp_GDP_data["Ln2005"])/(emp_GDP_data["La2000"] + emp_GDP_data["Ln2000"]) -  1*rep(1, nrow(data2plot))
data2plot["Emp_2010_growth"] <- (emp_GDP_data["La2010"] + emp_GDP_data["Ln2010"])/(emp_GDP_data["La2005"] + emp_GDP_data["Ln2005"]) -  1*rep(1, nrow(data2plot))
data2plot["Emp_tot_growth"] <- (emp_GDP_data["La2010"] + emp_GDP_data["Ln2010"])/(emp_GDP_data["La2000"] + emp_GDP_data["Ln2000"]) -  1*rep(1, nrow(data2plot))
#Testing linear model
emp_glm_2005 <- lm_robust(Emp_2005_growth ~ Emp_density_2000, data=data2plot)
emp_glm_tot <- lm_robust(Emp_tot_growth ~ Emp_density_2000, data=data2plot)

ggplot(data2plot, aes(x=Emp_density_2000, y=Emp_2005_growth)) + geom_point() +
  geom_text(label=rownames(data2plot), nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") + labs(x="Employment Density, 2000", y="Employment growth, 2000-2010")
ggsave("emp_growth_density.png")



#Decompose growth rates into two components (Agriculture and Non-agriculture)
data2plot["ag_emp_cont_2005"] <- (emp_GDP_data["La2005"] - emp_GDP_data["La2000"])/(emp_GDP_data["La2000"] + emp_GDP_data["Ln2000"])
data2plot["na_emp_cont_2005"] <- (emp_GDP_data["Ln2005"] - emp_GDP_data["Ln2000"])/(emp_GDP_data["La2000"] + emp_GDP_data["Ln2000"])
data2plot["ag_emp_cont_tot"] <- (emp_GDP_data["La2010"] - emp_GDP_data["La2000"])/(emp_GDP_data["La2000"] + emp_GDP_data["Ln2000"])
data2plot["na_emp_cont_tot"] <- (emp_GDP_data["Ln2010"] - emp_GDP_data["Ln2000"])/(emp_GDP_data["La2000"] + emp_GDP_data["Ln2000"])


ag_glm_2005 <- lm_robust(ag_emp_cont_2005 ~ Emp_density_2000, data=data2plot)
na_glm_2005 <-  lm_robust(na_emp_cont_2005 ~ Emp_density_2000, data=data2plot)
ag_glm_tot <- lm_robust(ag_emp_cont_tot~Emp_density_2000, data=data2plot)
na_glm_tot <-  lm_robust(na_emp_cont_tot ~ Emp_density_2000, data=data2plot)

ggplot(data2plot, aes(x=Emp_density_2000, y=na_emp_cont_2005)) + geom_point() +
  geom_text(label=rownames(data2plot), nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") + labs(x="Employment Density, 2000", y="Non-Ag Contribution to Employment growth, 2000-2005")
ggsave("na_cont_growth_density.png")

ggplot(data2plot, aes(x=Emp_density_2000, y=ag_emp_cont_2005)) + geom_point() +
  geom_text(label=rownames(data2plot), nudge_x=0.1, nudge_y=0, check_overlap = F, alpha = 0.8, size = 3.5) +
  geom_smooth(method="lm") + labs(x="Employment Density, 2000", y="Agriculture Contribution to Employment growth, 2000-2005")
ggsave("ag_cont_growth_density.png")


##Outliers?
data2plot_nob <- data2plot[!rownames(data2plot) %in% "Beijing", ]


#####################################################################################

##Checking data implied structural change
data2plot_noint <- data2plot[!row.names(data2plot) %in% "International",]
Aggregate_Ag_Share_2000 <- colSums(as.matrix(data2plot_noint$
                                               La2000))/(colSums(as.matrix(data2plot_noint$La2000)) + colSums(as.matrix(data2plot_noint$Ln2000)))
Aggregate_Ag_Share_2005 <- colSums(as.matrix(data2plot_noint$La2005))/(colSums(as.matrix(data2plot_noint$La2005)) + colSums(as.matrix(data2plot_noint$Ln2005))) #Fell 8%

##Checking Population Density Dispersion. Coefficient of variation of population density
data2plot["Emp_density_2005"] <- (data2plot$La2005 + data2plot$Ln2005)/data2plot$Landmass
data2plot["Emp_density_2010"] <- (emp_GDP_data$La2010 + emp_GDP_data$Ln2010)/data2plot$Landmass
data2plot_noint <- data2plot[!row.names(data2plot) %in% "International",]


#Should theoretically be population weighted but whatever (for now)
empdens_disp_2000 <- sd(data2plot_noint$Emp_density_2000)/mean(data2plot_noint$Emp_density_2000)
empdens_disp_2005 <- sd(data2plot_noint$Emp_density_2005)/mean(data2plot_noint$Emp_density_2005)
empdens_disp_2010 <- sd(data2plot_noint$Emp_density_2010)/mean(data2plot_noint$Emp_density_2010)
#Increases by 12 pp. There is a 12 ppoint increase in sd relative to mean of population density across provinces. 


