%Date created: October 14th 2021
%Date edited: October 14th 2021

%This file calibrates productivity given cost data, and data on land prices
%and value added per worker (and employment density). Normalizes so that geometric average
%productivity = 1, then applies the growth factor to productivity to fit
%fall in agricultural spending calculated in pref_estimateV2.R. 

global parameters N tradef


N=31*2 ; % Number of province/sector pairs. 31/62 are international sectors

%Load data (tradeflows + tradecosts) + parameters
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData\constructed_outputV2
load parameters
master = readtable('master2.xlsx') ;
master_stacked = readtable('master_stacked2.xlsx') ;
pref_estimates = readtable('pref_estimates.xlsx') ;


load tradecosts_struct
load tradeflows_struct


landprice_mobile = readtable('landprice_mobile.xlsx') ;

%%Note: immobile land/labour + no IO in international sector implies growth factor in
%%international productivity = growth factor in gross output/growth factor
%%in costs

%Changes in commercial land 2000-2005
dCommercial_land_na = NaN(N/2, 1) ;
dCommercial_land_ag = NaN(N/2, 1) ;

%If mobile features perfectly mobile land...
if parameters.land_mobile == 1
    dCommercial_land_na([1:N/2-1], 1) = landprice_mobile.na_commercial_land([N/2:N-2], 1)./landprice_mobile.na_commercial_land([1:N/2-1], 1) ;
    dCommercial_land_ag([1:N/2-1], 1) = landprice_mobile.ag_commercial_land([N/2:N-2], 1)./landprice_mobile.ag_commercial_land([1:N/2-1], 1) ;
end

%If change in land set to fit data...

%If using estimated values of epsilon and eta...
if parameters.pointest_eta_epsilon == 1
   parameters.eta = pref_estimates.Eta_nls(1) ;
   parameters.epsilon = pref_estimates.Epsilon_nls(1) ;
   save parameters parameters
end

%Changes in self trade shares raised to respective inverse trade
%elasticities (for calculation of price index!)
dSelftrade_ag_2005 = (diag(tradef.ag_2007./tradef.ag_2002)).^(1/parameters.theta_a) ;
dSelftrade_na_2005 = (diag(tradef.na_2007./tradef.na_2002)).^(1/parameters.theta_n) ;

%Apply scale factor implied by pref_estimateV2.R to match geometric average
%fall in price indicies. Note: since these price indicies are homogenous of
%degree one in dCosts, scale dCost and dPrice index BY THE SAME FACTOR. 

%2005 only.
master.dCost_ag_2005 = master.dCost_ag_2005/(pref_estimates.Ga_Gn_2005(1)*pref_estimates.Gn_2005_nls(1)) ;
master.dpindex_ag_2005 = master.dpindex_ag_2005/(pref_estimates.Ga_Gn_2005(1)*pref_estimates.Gn_2005_nls(1)) ;
master.dCost_na_2005 = master.dCost_na_2005/(pref_estimates.Gn_2005_nls(1)) ;
master.dpindex_na_2005 = master.dpindex_na_2005/(pref_estimates.Gn_2005_nls(1)) ;

%TEST: geometric average price index growth should be consistent with
%pref_estimateV2.R
test_na = geomean(master.dpindex_na_2005([1:N/2-1], 1)) ; %Only differ due to rounding error between programs
test_ag = geomean(master.dpindex_ag_2005([1:N/2-1], 1)) ;
test3 = test_ag/test_na ; %agrees!

test4 = master.dCost_ag_2005.*dSelftrade_ag_2005 - master.dpindex_ag_2005 ; %Should equal change in price index (it does to machine precision)

%% Calculating implied productivity growth from 2000-2005


%calculating
prod_growth_ag = (((master.dVA_ag_2005).^(parameters.vashare_a)).*((master.dL_ag_2005./dCommercial_land_ag).^(parameters.land_ag - parameters.alpha_a)).*((master.dCost_ag_2005.*dSelftrade_ag_2005).^(parameters.phi_aa)).*((master.dCost_na_2005.*dSelftrade_na_2005).^(parameters.phi_na)))./master.dCost_ag_2005;
prod_growth_na = (((master.dVA_na_2005).^(parameters.vashare_n)).*((master.dL_na_2005./dCommercial_land_na).^(parameters.land_na - parameters.alpha_n)).*((master.dCost_ag_2005.*dSelftrade_ag_2005).^(parameters.phi_an)).*((master.dCost_na_2005.*dSelftrade_na_2005).^(parameters.phi_nn)))./master.dCost_na_2005;

%International (note: change in costs normalized to 1 internationally by
%construction, + no labour/land mobility assumed)
prod_growth_ag(31, 1) = ((master.nomY_ag_2005(N/2, 1)./master.nomY_ag_2000(N/2, 1))^(parameters.vashare_a))*(((master.dCost_ag_2005(N/2, 1).*dSelftrade_ag_2005(N/2, 1)).^(parameters.phi_aa)).*((master.dCost_na_2005(N/2, 1).*dSelftrade_na_2005(N/2, 1)).^(parameters.phi_na)))/master.dCost_ag_2005(N/2, 1) ;
prod_growth_na(31, 1) = ((master.nomY_na_2005(N/2, 1)./master.nomY_na_2000(N/2, 1))^(parameters.vashare_n))*(((master.dCost_ag_2005(N/2, 1).*dSelftrade_ag_2005(N/2, 1)).^(parameters.phi_an)).*((master.dCost_na_2005(N/2, 1).*dSelftrade_na_2005(N/2, 1)).^(parameters.phi_nn)))/master.dCost_na_2005(N/2, 1) ;

%NOTE: TFP not in every sense comparable with international because of no labour/land mobility and
%unchanging inelastic supply.

histfit(prod_growth_ag, 6, 'kernel')
histfit(prod_growth_na, 6, 'kernel')

%Reloading data, saving and exporting productivity growth
master = readtable('master2.xlsx') ;
master.dTFP_ag_2005 = prod_growth_ag ;
master.dTFP_na_2005 = prod_growth_na ;

writetable(master, "master3.xlsx")
