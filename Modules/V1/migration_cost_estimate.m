
%Date created: June 4th, 2021
%Date modified: October 7th, 2021
global theta epsilon eta Ga_2005 Gn_2005 Ga_2010 Gn_2010 kappa N nu
%This module takes the migration elasticity w.r.t. real income and
%calculates what the implied migration costs are. 
%Are expressing migration elasticity with respect to an economic quantity
%index (or wages deflated by an economic price index) 
% as in Samuelson and Swamy (1974). 

%Directory
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData

%Data 
%Read output from rel_prod_estimate.m + renormalization.R
master =  readtable('constructed_output/master2.xlsx') ;
%Migration flows from raw data
migflows_2000 = readtable('2000_migrantflows.xlsx') ; 
migflows_2000(:, 1) = [] ;
migflows_2005 = readtable('2005_migrantflows.xlsx') ;
migflows_2005(:, 1) = [] ;
migflows_2010 = readtable('2010_migrantflows.xlsx') ;
migflows_2010(:, 1) = [] ;
%Landprices from "agglomeration_estimate.R"
landprice = readtable('constructed_output/landuse_constructed.xlsx') ;

%Reading parameters from pref.estimate.R. Using NLS specification
parameters = readtable('constructed_output/pref_estimates.xlsx') ;
theta = parameters.Theta(1) ;
epsilon = parameters.Epsilon_nls(1) ;
eta = parameters.Eta_nls(1) ;
Ga_2005 = 1/(parameters.Gn_2005_nls(1)*parameters.Ga_Gn_2005(1)) ;
Gn_2005 = 1/(parameters.Gn_2005_nls(1)) ;
Ga_2010 = 1/(parameters.Gn_2010_nls*parameters.Ga_Gn_2010(1)) ;
Gn_2010 = 1/(parameters.Gn_2010_nls) ;
kappa = 1.5 ; %Tombe and Zhu (2019) income elasticity of migration.
nu = 0.87 ; %non-housing expenditure share 

phi_an = 0.04 ;
phi_aa = 0.16 ;
phi_na = 0.25 ;
phi_nn = 0.61 ;
vashare_n = 1 - phi_an - phi_nn ;
vashare_a = 1 - phi_aa - phi_na ;

%Constructing measures of compensating variation. Equal to homothetic CES
%welfare formulas in Caliendo and Parro (2015). 

%Deleting rows in "International"
master(31, :) = [] ;
N = size(master, 1) ;

master.dCompVariation_ag_2005 = repelem(NaN, N)' ;
master.dCompVariation_na_2005 = repelem(NaN, N)' ;
master.dCompVariation_ag_2010 = repelem(NaN, N)' ;
master.dCompVariation_na_2010 = repelem(NaN, N)' ;

%

master.Vawork_ag_2000 = vashare_a*master.nomY_ag_2000./master.L_ag_2000 ;
master.Vawork_na_2000 = vashare_n*master.nomY_na_2000./master.L_na_2000 ;
master.Vawork_ag_2005 = vashare_a*master.nomY_ag_2005./master.L_ag_2005 ;
master.Vawork_na_2005 = vashare_n*master.nomY_na_2005./master.L_na_2005 ;
master.Vawork_ag_2010 = vashare_a*master.nomY_ag_2010./master.L_ag_2010 ;
master.Vawork_na_2010 = vashare_n*master.nomY_na_2010./master.L_na_2010 ;

% %Change in nominal VA/capita
master.dVaw_ag_2005 = (master.Vawork_ag_2005)./(master.Vawork_ag_2000) ;
master.dVaw_ag_2010 = (master.Vawork_ag_2010)./(master.Vawork_ag_2005) ;
master.dVaw_na_2005 = (master.Vawork_na_2005)./(master.Vawork_na_2000) ;
master.dVaw_na_2010 = (master.Vawork_na_2010)./(master.Vawork_na_2005) ;

%Compensating variation calculation
master.dCompVariation_ag_2005 = master.dVaw_ag_2005./((master.agspend_ag_2000.*((Ga_2005*master.dpindex_ag_2005).^(1-eta)) + (1-master.agspend_ag_2000).*((Gn_2005*master.dpindex_na_2005).^(1-eta))).^(1/(1-eta))) ;
master.dCompVariation_na_2005 = master.dVaw_na_2005./((master.agspend_na_2000.*((Ga_2005*master.dpindex_ag_2005).^(1-eta)) + (1-master.agspend_na_2000).*((Gn_2005*master.dpindex_na_2005).^(1-eta))).^(1/(1-eta))) ;
master.dCompVariation_ag_2010 = master.dVaw_ag_2010./((master.agspend_ag_2005.*((Ga_2010*master.dpindex_ag_2010).^(1-eta)) + (1-master.agspend_ag_2005).*((Gn_2010*master.dpindex_na_2010).^(1-eta))).^(1/(1-eta))) ;
master.dCompVariation_na_2010 = master.dVaw_na_2010./((master.agspend_na_2005.*((Ga_2010*master.dpindex_ag_2010).^(1-eta)) + (1-master.agspend_na_2005).*((Gn_2010*master.dpindex_na_2010).^(1-eta))).^(1/(1-eta))) ;

%Putting everything into stacked vectors Prov1_ag, Prov1_na, Prov2_ag, ...
dCompVariation_2005 = [master.dCompVariation_ag_2005' ; master.dCompVariation_na_2005'] ;
dCompVariation_2005 = reshape(dCompVariation_2005, [2*N, 1]) ;
dCompVariation_2010 = [master.dCompVariation_ag_2010' ; master.dCompVariation_na_2010'] ;
dCompVariation_2010 = reshape(dCompVariation_2010, [2*N, 1]) ;


dLandprice_2005 = [(landprice.landprice((N + 1):2*N, 1)./landprice.landprice(1:N, 1))' ; (landprice.landprice((N + 1):2*N, 1)./landprice.landprice(1:N, 1))'] ;
dLandprice_2005 = reshape(dLandprice_2005, [2*N, 1]) ;
dLandprice_2010 = [(landprice.landprice((2*N + 1):3*N, 1)./landprice.landprice((N+1):2*N, 1))' ; (landprice.landprice((2*N + 1):3*N, 1)./landprice.landprice((N+1):2*N, 1))'] ;
dLandprice_2010 = reshape(dLandprice_2010, [2*N, 1]) ;

dNomY_2005 = [(master.nomY_ag_2005./master.L_ag_2005)./(master.nomY_ag_2000./master.L_ag_2000) ; (master.nomY_na_2005./master.L_na_2005)./(master.nomY_na_2000./master.L_na_2000)] ;
dNomY_2005 = reshape(dNomY_2005, [2*N, 1]) ;
dNomY_2010 = [(master.nomY_ag_2010./master.L_ag_2005)./(master.nomY_ag_2005./master.L_ag_2005) ; (master.nomY_na_2010./master.L_na_2010)./(master.nomY_na_2005./master.L_na_2005)] ;
dNomY_2010 = reshape(dNomY_2010, [2*N, 1]) ;

%Total welfare index after accounting for housing consumption
dWelfare_2005 = ((dNomY_2005./dLandprice_2005).^(1-nu)).*((dCompVariation_2005).^(nu)) ;
dWelfare_2010 = ((dNomY_2010./dLandprice_2010).^(1-nu)).*((dCompVariation_2010).^(nu)) ;

%Solving for matrix of migration share changes
temp = repmat(diag(table2array(migflows_2000)), 1, 2*N); %matrix of diagonal elements from migflows
norm_kappa_mig_flows_2000 = (table2array(migflows_2000)./temp).^(-1/kappa) ;
temp = repmat(diag(table2array(migflows_2005)), 1, 2*N); %matrix of diagonal elements from migflows
norm_kappa_mig_flows_2005 = (table2array(migflows_2005)./temp).^(-1/kappa) ;
temp = repmat(diag(table2array(migflows_2010)), 1, 2*N) ;
norm_kappa_mig_flows_2010 = (table2array(migflows_2010)./temp).^(-1/kappa) ; 
clear temp

%Migration cost of moving from sector location i to j
dMigrationCost_2005 = zeros(N*2, 2) ;
dMigrationCost_2010 = zeros(N*2, 2) ;

for i = 1:2*N
   for j = 1:2*N
       dMigrationCost_2005(i, j) = (dWelfare_2005(j)/dWelfare_2005(i))*(norm_kappa_mig_flows_2005(i, j)/norm_kappa_mig_flows_2000(i, j)) ;
       dMigrationCost_2010(i, j) = (dWelfare_2010(j)/dWelfare_2010(i))*(norm_kappa_mig_flows_2010(i, j)/norm_kappa_mig_flows_2005(i, j)) ;
   end
end

dMigrationCost_2005 = array2table(dMigrationCost_2005, 'VariableNames', migflows_2000.Properties.VariableNames) ;
dMigrationCost_2010 = array2table(dMigrationCost_2010, 'VariableNames', migflows_2005.Properties.VariableNames) ;

%Storing migration costs to be used with other programs.
writetable(dMigrationCost_2005, "constructed_output/dMigrationCost_2005.xlsx")
writetable(dMigrationCost_2010, "constructed_output/dMigrationCost_2010.xlsx")

