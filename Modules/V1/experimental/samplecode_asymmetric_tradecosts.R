
###_________PART 2___________
##Estimating trade cost change asymmetries 
##With provincial data: aggregating bilateral distance to regions using product of total employment in 2000 as weights. 

#master <- read_excel("master_raw.xlsx")
#dist <- read_excel("province_distance.xlsx")

#provname <- c("Anhui", "Beijing", 
"Chongqing", "Fujian", "Gansu", "Guangdong", "Guangxi",	"Guizhou",	"Hainan",	"Hebei",	"Heilongjiang",	"Henan",	
"Hubei",	"Hunan",	"Inner Mongolia",	"Jiangsu",	"Jiangxi",	"Jilin",	"Liaoning",	
"Ningxia",	"Qinghai",	"Shandong",	"Shanghai",	"Shaanxi",	"Shanxi",	"Sichuan",	
"Tianjin",	"Xinjiang",	"Yunnan",	"Zhejiang") #For constructing distances

#master["emp_2000"] <- master$L_ag_2000 + master$L_na_2000 #for weights

#######construct distance matrix###_______________
#dist_mat =  matrix(nrow = 30, ncol = 30)
#row.names(dist_mat) <- row.names(tr_ag_2002)
#colnames(dist_mat) <- row.names(tr_ag_2002)

#for (i in 1:size(provname)[2]) { #looping over all provinces
# temp <- dist[dist$exporter %in% provname[i] ,]
#  dist_mat[i , ] <- t(as.matrix(temp$distance))
#}

#dist_mat_reg <- matrix(nrow = 8, ncol = 8) #regions not including international. For now make international trade costs fall as in the symmetric Head-Riess index. 

#Loop constructs aggregate distances 
#for (i in 1:(size(reg_name)[2]-1)) { #loop over all combinations of regions not including international
#for (j in 1:(size(reg_name)[2]-1)) {
#   emp_ij_t = 0
#    emp_dij_t = 0

#      for (prov1 in Aggregate_reg[[i]]) { #loop over all provinces within a region
#      for (prov2 in Aggregate_reg[[j]]) {

#              emp_ij_t = emp_ij_t + master$emp_2000[prov1]*master$emp_2000[prov2]
#              emp_dij_t = emp_dij_t + master$emp_2000[prov1]*master$emp_2000[prov2]*dist_mat[prov1, prov2]

#      }

#    }
#    dist_mat_reg[i, j] <- emp_dij_t/emp_ij_t

#  }
#}

#dist_mat_reg <- data.frame(dist_mat_reg)
#colnames(dist_mat_reg) <- colnames(tr_ag_2002_agr[, -9])

#Putting it into data frame
#Regression_matrix  <- data.frame(rep(NA, 64), rep(NA, 64))
#colnames(Regression_matrix) <- c("Importer", "Exporter")

#temp = 0
#for (i in 1:(size(reg_name)[2]-1)) { #loop over all combinations of regions not including international
#  temp <- i*8 - 7
#  for (j in 1:(size(reg_name)[2]-1)) {
#    Regression_matrix$Importer[temp] <- reg_name[i]
#   Regression_matrix$Exporter[temp] <- reg_name[j]
#    temp <- temp + 1
#  }
#}

#Regression_matrix["Distance"] <- rep(NA, nrow(Regression_matrix))
#Regression_matrix["Ag_relspend_2000"] <- rep(NA, nrow(Regression_matrix))
#Regression_matrix["Na_relspend_2000"] <- rep(NA, nrow(Regression_matrix))
#Regression_matrix["Ag_relspend_2005"] <- rep(NA, nrow(Regression_matrix))
#Regression_matrix["Na_relspend_2005"] <- rep(NA, nrow(Regression_matrix))
#Regression_matrix["Ag_relspend_2010"] <- rep(NA, nrow(Regression_matrix))
#Regression_matrix["Na_relspend_2010"] <- rep(NA, nrow(Regression_matrix))

#for (i in 1:(size(reg_name)[2]-1)) { #Imputing distance into regional distance matrix
# Regression_matrix$Distance[Regression_matrix$Importer == reg_name[i]] <- t(dist_mat_reg[i, ])
#}

#Importing relative spending share by ag and nonag
#for (i in 1:(size(reg_name)[2]-1)) { 
#  Regression_matrix$Ag_relspend_2000[Regression_matrix$Importer == reg_name[i]] <- t(tr_ag_2002_agr[i, -9])/tr_ag_2002_agr[i, i]
#  Regression_matrix$Ag_relspend_2005[Regression_matrix$Importer == reg_name[i]] <- t(tr_ag_2007_agr[i, -9])/tr_ag_2007_agr[i, i]
#  Regression_matrix$Ag_relspend_2010[Regression_matrix$Importer == reg_name[i]] <- t(tr_ag_2012_agr[i, -9])/tr_ag_2012_agr[i, i]
# Regression_matrix$Na_relspend_2000[Regression_matrix$Importer == reg_name[i]] <- t(tr_na_2002_agr[i, -9])/tr_na_2002_agr[i, i]
#  Regression_matrix$Na_relspend_2005[Regression_matrix$Importer == reg_name[i]] <- t(tr_na_2002_agr[i, -9])/tr_na_2007_agr[i, i]
#  Regression_matrix$Na_relspend_2010[Regression_matrix$Importer == reg_name[i]] <- t(tr_na_2002_agr[i, -9])/tr_na_2012_agr[i, i]
#}

#Importer_exporter FE's

#Regressions 
#gravity_ag_2000 <- lm_robust(Ag_relspend_2000 ~ Distance, data = Regression_matrix, fixed_effects = Importer + Exporter)


##Saving data to xlsx 
setwd("C:/Users/James/Dropbox/SchoolFolder/SYP/data/MyData/constructed_output")
write_xlsx(Head_Ries[1], "HRtau_ag_2002.xlsx")
write_xlsx(Head_Ries[2], "HRtau_na_2002.xlsx")
write_xlsx(Head_Ries[3], "HRtau_ag_2007.xlsx")
write_xlsx(Head_Ries[4], "HRtau_na_2007.xlsx")
write_xlsx(Head_Ries[5], "HRtau_ag_2012.xlsx")
write_xlsx(Head_Ries[6], "HRtau_na_2012.xlsx")