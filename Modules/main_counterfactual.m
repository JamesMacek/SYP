
%This file performs the main counterfactual experiment from 2000-2005 and 2005-2010 given:
%Changes in productivity (both average vs absolute TFP and capital)
%Changes in trade costs 
%Changes in migration costs.


%Date created: June 15th, 2021
%Date modified: July 24th, 2021



global N parameters co_obj tradec tradef migc migf 

parameters = struct ;

parameters.phi_an = 0.04 ;
parameters.phi_aa = 0.16 ;
parameters.phi_na = 0.25 ;
parameters.phi_nn = 0.61 ;
parameters.vashare_n = 1 - parameters.phi_an - parameters.phi_nn ;
parameters.vashare_a = 1 - parameters.phi_aa - parameters.phi_na ;
parameters.land_na = 0.01 ;
parameters.land_ag = 0.26 ;
parameters.lab_na = 0.19 ;
parameters.lab_ag = 0.27 ;

parameters.cap_na = 0.15 ;
parameters.cap_ag = 0.06 ;
%Returns to scale
parameters.alpha_na = 0.05 ;
parameters.alpha_ag = 0.05 ;

N = 62 ; %number of province-sector pairs. 
%Directory
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData

%Reading parameters from pref.estimate.R. Using NLS specification.
parameters1 = readtable('constructed_output/pref_estimates.xlsx') ;

landuse = readtable('constructed_output/landuse_constructed.xlsx') ;
d_landuse_2005 = [((landuse.ag_commercial_land(31:60))./(landuse.ag_commercial_land(1:30))) , ((landuse.na_commercial_land(31:60))./(landuse.na_commercial_land(1:30)))];
d_landuse_2005(N/2, [1:2]) = [1, 1] ;
d_landuse_2010 = [((landuse.ag_commercial_land(61:90))./(landuse.ag_commercial_land(31:60))) , ((landuse.na_commercial_land(61:90))./(landuse.na_commercial_land(31:60)))] ;
d_landuse_2010(N/2, [1:2]) = [1, 1] ;



parameters.theta = parameters1.Theta(1) ;
parameters.epsilon = parameters1.Epsilon_nls(1) ;
parameters.eta = parameters1.Eta_nls(1) ;

%Solving for TFP growth given fall in prices observed in previous modules.

P_temp2005 = exp(inv([- 1 - parameters.phi_aa, -parameters.phi_na ; -parameters.phi_an, -1 - parameters.phi_nn])*[log(parameters1.Gn_2005_nls(1)*parameters1.Ga_Gn_2005(1)) ; log(parameters1.Gn_2005_nls(1))]) ; 
parameters.Pa_2005 = P_temp2005(1) ;
parameters.Pn_2005 = P_temp2005(2) ;

P_temp2010 = exp(inv([- 1 - parameters.phi_aa, -parameters.phi_na ; -parameters.phi_an, -1 - parameters.phi_nn])*[log(parameters1.Gn_2010_nls(1)*parameters1.Ga_Gn_2010(1)) ; log(parameters1.Gn_2010_nls(1))]) ;
parameters.Pa_2010 = P_temp2010(1) ;  % CHANGE THIS LATER!
parameters.Pn_2010 = P_temp2010(2) ;


parameters.kappa = 1.5 ; %Tombe and Zhu (2019) income elasticity of migration.
parameters.nu = 0.87 ; %non-housing expenditure share 



%Reading data 
%master data 
master =  readtable('constructed_output/master2.xlsx') ;
calibrated_GDP_tradebalance = readtable('constructed_output/Calibrated_GDP_balanced.xlsx') ;

master(31, :) = [] ;
master_noint = master ;
master = readtable('constructed_output/master2.xlsx') ;

%%Trade flows and trade costs
tradec = struct ;
tradec.ag_2002 = table2array(readtable('constructed_output/HRtau_ag_2002'));
tradec.ag_2007 = table2array(readtable('constructed_output/HRtau_ag_2007'));
tradec.na_2002 = table2array(readtable('constructed_output/HRtau_na_2002'));
tradec.na_2007 = table2array(readtable('constructed_output/HRtau_na_2007'));

tradef=struct ;
tradef.ag_2002 = readtable('2002_na_tradeflows.xlsx');
tradef.ag_2002 = table2array(removevars(tradef.ag_2002, 'Var1'));
tradef.ag_2007 = readtable('2007_na_tradeflows.xlsx');
tradef.ag_2007 = table2array(removevars(tradef.ag_2007, 'Var1'));
tradef.na_2002 = readtable('2002_na_tradeflows.xlsx');
tradef.na_2002 = table2array(removevars(tradef.na_2002, 'Var1'));
tradef.na_2007 = readtable('2007_na_tradeflows.xlsx');
tradef.na_2007 = table2array(removevars(tradef.na_2007 , 'Var1'));

%%Migration flows and migration costs 
migc = struct ;
migc.c2005 = table2array(readtable('constructed_output/dMigrationCost_2005.xlsx')) ;
migc.c2010 = table2array(readtable('constructed_output/dMigrationCost_2010.xlsx')) ;

migf =struct ;
migf.f2000 = readtable('2000_migrantflows.xlsx') ; 
migf.f2000 = table2array(removevars(migf.f2000, 'Var1'));
migf.f2005 = readtable('2005_migrantflows.xlsx') ;
migf.f2005 = table2array(removevars(migf.f2005, 'Var1')) ;
migf.f2010 = readtable('2010_migrantflows.xlsx') ;
migf.f2010 = table2array(removevars(migf.f2010, 'Var1')) ;

%Structure for all counterfactual objects
co_obj = struct ;

%Note: structure of all variables will be [ag, na] then by province 1-30
%in order as they appear in master and master_stacked data 

%% Part 1: Setting up additional parameters in co_obj

%Temporary reshaping for the migration matrix structure
emp_lab_2000 = reshape([master_noint.L_ag_2000' ; master_noint.L_na_2000'], [N-2, 1]) ;
emp_lab_2005 = reshape([master_noint.L_ag_2005' ; master_noint.L_na_2005'], [N-2, 1]) ;
emp_lab_2010 = reshape([master_noint.L_ag_2010' ; master_noint.L_na_2010'], [N-2, 1]) ;

%Registered workers

co_obj.registered_lab_2000 = (migf.f2000')\emp_lab_2000 ;
co_obj.registered_lab_2005 = (migf.f2005')\emp_lab_2005 ;
co_obj.registered_lab_2010 = (migf.f2010')\emp_lab_2010 ;
co_obj.registered_lab_2000 = reshape(co_obj.registered_lab_2000, [2, (N-2)/2])' ;
co_obj.registered_lab_2005 = reshape(co_obj.registered_lab_2005, [2, (N-2)/2])' ;
co_obj.registered_lab_2010 = reshape(co_obj.registered_lab_2010, [2, (N-2)/2])' ;

co_obj.emp_lab_2000 = [master_noint.L_ag_2000 , master_noint.L_na_2000]  ;
co_obj.emp_lab_2005 = [master_noint.L_ag_2005 , master_noint.L_na_2005] ;
co_obj.emp_lab_2010 = [master_noint.L_ag_2010 , master_noint.L_na_2010] ;
co_obj.emp_lab_2000(N/2, [1,2]) = [1, 1] ;
co_obj.emp_lab_2005(N/2, [1,2]) = [1, 1] ;
co_obj.emp_lab_2010(N/2, [1,2]) = [1, 1] ;

demp_lab_2005 = co_obj.emp_lab_2005./co_obj.emp_lab_2000 ;
demp_lab_2010 = co_obj.emp_lab_2010./co_obj.emp_lab_2005 ;

%TFP growth
co_obj.dTFP_2005 = [parameters.Pa_2005*master.dTFP_ag_2005 , parameters.Pn_2005*master.dTFP_na_2005] ;
co_obj.dTFP_2010 = [parameters.Pa_2010*master.dTFP_ag_2010 , parameters.Pn_2010*master.dTFP_na_2010] ;

%Adjusting for population density growth's contribution to TFP growth
adj_2005 = [(demp_lab_2005(:, 1)./d_landuse_2005(:, 1)).^(parameters.alpha_ag) , (demp_lab_2005(:, 2)./d_landuse_2005(:, 2)).^(parameters.alpha_na)] ;
adj_2010 = [(demp_lab_2010(:, 1)./d_landuse_2010(:, 1)).^(parameters.alpha_ag) , (demp_lab_2010(:, 2)./d_landuse_2010(:, 2)).^(parameters.alpha_na)] ;

co_obj.dTFP_2005 = co_obj.dTFP_2005./(adj_2005) ;
adjPa_2005 = mean(co_obj.dTFP_2005(:, 1)) ; %Calculating average productivity growth over all locations
adjPn_2005 = mean(co_obj.dTFP_2005(:, 2)) ; 
co_obj.dTFP_2010 = co_obj.dTFP_2010./(adj_2010) ;
adjPa_2010 = mean(co_obj.dTFP_2010(:, 1)) ;
adjPn_2010 = mean(co_obj.dTFP_2010(:, 2)) ;

%Capital stock growth
co_obj.dK_2005 = [(master.K_ag_2005./master.K_ag_2000) , (master.K_na_2005./master.K_na_2000)] ;
co_obj.dK_2005(N/2, [1, 2]) = [1 , 1] ;
co_obj.dK_2010 = [(master.K_ag_2010./master.K_ag_2005) , (master.K_na_2010./master.K_na_2005)] ;
co_obj.dK_2010(N/2, [1, 2]) = [1 , 1] ; %Absorbing international capital growth into TFP term (i.e assume international gets no TFP)
%Average capital stock growth == 1
co_obj.RdK_2005 = [co_obj.dK_2005(:, 1)/mean(co_obj.dK_2005(:, 1), 'all'), co_obj.dK_2005(:, 2)/mean(co_obj.dK_2005(:, 2), 'all')] ;
co_obj.RdK_2010 = [co_obj.dK_2010(:, 1)/mean(co_obj.dK_2010(:, 1), 'all'), co_obj.dK_2010(:, 2)/mean(co_obj.dK_2010(:, 2), 'all')] ;

%Equalized to average capital stock growth
%Note average contains international's capital stock == 1. 
co_obj.AdK_2005 = [repelem(mean(co_obj.dK_2005(:, 1), 'all'), N/2, 1), repelem(mean(co_obj.dK_2005(:, 2), 'all'), N/2, 1)] ;
co_obj.AdK_2010 = [repelem(mean(co_obj.dK_2010(:, 1), 'all'), N/2, 1), repelem(mean(co_obj.dK_2010(:, 2), 'all'), N/2, 1)] ;


%Average capital stock/TFP growth for china only. Used for Shapely value
%decomposition
co_obj.AdTFP_2005 = [repelem(adjPa_2005, (1/2)*N, 1) , repelem(adjPn_2005, (1/2)*N, 1)] ;
co_obj.AdTFP_2010 = [repelem(adjPa_2010, (1/2)*N, 1) , repelem(adjPn_2010, (1/2)*N, 1)] ;

%Relative TFP growth (i.e. average over all locations ==1
co_obj.RdTFP_2005 = co_obj.dTFP_2005 ;
co_obj.RdTFP_2005(:, 1) = co_obj.RdTFP_2005(:, 1)/mean(co_obj.RdTFP_2005(:, 1), 'all') ;
co_obj.RdTFP_2005(:, 2) = co_obj.RdTFP_2005(:, 2)/mean(co_obj.RdTFP_2005(:, 2), 'all') ;

%Initial gross output values from GDP calibrated to satisfy market
%clearing.
co_obj.nomY_2000 = [calibrated_GDP_tradebalance.nomY([1:N/2], 1) , calibrated_GDP_tradebalance.nomY([(3/2*N+1):2*N], 1)] ;
co_obj.nomY_2005 = [calibrated_GDP_tradebalance.nomY([(N/2 + 1):N], 1) , calibrated_GDP_tradebalance.nomY([(2*N+1):(5/2)*N], 1)] ;

co_obj.Agspend_2000 = [master.agspend_ag_2000, master.agspend_na_2000] ;
co_obj.Agspend_2005 = [master.agspend_ag_2005, master.agspend_na_2005] ;

co_obj.commercial_land_2000 = [landuse.ag_commercial_land([1:30]), landuse.na_commercial_land([1:30])] ;
co_obj.commercial_land_2005 = [landuse.ag_commercial_land([31:60]), landuse.na_commercial_land([31:60])] ;
co_obj.residential_land_2000 = [landuse.ag_residential_land([1:30]), landuse.na_residential_land([1:30])] ;
co_obj.residential_land_2005 = [landuse.ag_residential_land([31:60]), landuse.na_residential_land([31:60])] ;
co_obj.landmass = landuse.landmass(1:30) ;

%% Part 2: Calculate implied equilibrium from entire model.
%Saving these objects for use with other counterfactual programs. 
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData\constructed_output
save counterfactual_objects co_obj migc migf tradec tradef parameters


cd C:\Users\James\Dropbox\SchoolFolder\SYP\modules

clearvars -except co_obj migc migf tradec tradef master master_noint N parameters

Coeff_density = zeros(2, 2, 2, 2) ; %Coefficient of variation in population density
wCoeff_density = zeros(2, 2, 2, 2) ; %Coefficient of variation weighted by employment

%Absolute, relative productivity, trade costs and migration costs. 
for aprod = 1:2 %2 for on, 1 for off
for rprod = 1:2
for trc = 1:2
for mic = 1:2
    
    dK = ones(N/2, 2) ;
    dTFP = ones(N/2, 2) ;
    dTFP(N/2, [1:2]) = co_obj.dTFP_2005(N/2, [1:2]) ;
    dM = ones(N-2, N-2) ;
    
    if aprod == 2 && rprod == 2
        dK = co_obj.dK_2005 ;
        dTFP = co_obj.dTFP_2005 ;
    end

    if aprod == 2 && rprod == 1
       dK = co_obj.AdK_2005 ;
       dTFP = co_obj.AdTFP_2005 ;
    end
        
    if aprod == 1 && rprod == 2
        dK = co_obj.RdK_2005 ;
        dTFP = co_obj.RdTFP_2005 ;
    end
    
    if trc == 2
        
       pindexa = @p_index_ag ;
       pindexn = @p_index_na ;
    else 
       pindexa = @p_index_ag_notr ;
       pindexn = @p_index_na_notr ;
    end
    
    if mic == 2
      dM = migc.c2005 ;
    end
    
%Initial values = 2005 equilibrium
Lab_2005 = co_obj.emp_lab_2005 ;
dLab_2005 = co_obj.emp_lab_2005./co_obj.emp_lab_2000 ;
dWage_2005 = ones(N/2, 2) ; %Change in gross output per worker-- or change in value added per worker=change in wage per worker
G_2005 = co_obj.nomY_2005./Lab_2005 ; %Gross output per worker 
aG_2005 = co_obj.nomY_2005 ; %Gross output 
Agshare_2005 = ones(N/2, 2)*0.2 ; %Initial value of agricultural shares. Does not matter as updated right away. 
Agshare_2005(N/2, [1:2]) = co_obj.Agspend_2005(N/2, [1:2]) ; %International spending shares remain unchanged.

% + Other objects
dCommercial_land_2005 = ones(N/2, 2) ;
dResidential_land_2005 = ones(N/2, 2) ; 
options = optimset('Display','none') ; %options for fsolve

Lnorm_2005 = 1 ;
while Lnorm_2005 > 0.00001
    
Gnorm_2005 = 1 ;
while Gnorm_2005 > 0.00001 %Margin of error allowed for goods market clearing
    
%1: Calculating land allocation implied by the fixed values of dWage and
%dLab and perfect land mobility. International land immobile. 
dVA_rat_2005 = (aG_2005(:, 1)./aG_2005(:, 2))./(co_obj.nomY_2000(:, 1)./co_obj.nomY_2000(:, 2)) ;
dCommercial_land_2005([1:30], 2) = co_obj.landmass([1:30])./(dVA_rat_2005([1:30], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat_2005([1:30], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ;
dResidential_land_2005([1:30], 2) = dCommercial_land_2005([1:30], 2) ;
dCommercial_land_2005([1:30], 1) = (dVA_rat_2005([1:30], 1).*co_obj.landmass([1:30]))./(dVA_rat_2005([1:30], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat_2005([1:30], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ;
dResidential_land_2005([1:30], 1) = dCommercial_land_2005([1:30], 1) ; 
dRental_rate_2005 = (aG_2005./co_obj.nomY_2000)./dCommercial_land_2005 ; %Change in VA per unit of land in any sector

    
%2: Calculating price index
cost_2005 = ones(N/2, 2)  ;
cost_2005_new = ones(N/2, 2) ; %initial value for costs. 
cnorm_2005 = 1 ;

%IN LOOP: TOTAL TFP, CAPITAL, TRADE COSTS
while cnorm_2005 > 0.00001 %Margin of error for fixed point in determining marginal costs.  
    cost_2005_new(:, 1) = (dWage_2005(:, 1).^(parameters.vashare_a)).*(pindexa(cost_2005(:, 1), 2005).^(parameters.phi_aa)).*(pindexn(cost_2005(:, 2), 2005).^(parameters.phi_na)).*(((dLab_2005(:, 1)./dCommercial_land_2005(:, 1)).^(parameters.land_ag)).*((dLab_2005(:, 1)./dK(:, 1)).^(parameters.cap_ag)))./(dTFP(:, 1).*(dLab_2005(:, 1)./dCommercial_land_2005(:, 1)).^(parameters.alpha_ag)) ;
    cost_2005_new(:, 2) = (dWage_2005(:, 2).^(parameters.vashare_n)).*(pindexa(cost_2005(:, 1), 2005).^(parameters.phi_an)).*(pindexn(cost_2005(:, 2), 2005).^(parameters.phi_nn)).*(((dLab_2005(:, 2)./dCommercial_land_2005(:, 2)).^(parameters.land_na)).*((dLab_2005(:, 2)./dK(:, 2)).^(parameters.cap_na)))./(dTFP(:, 2).*(dLab_2005(:, 2)./dCommercial_land_2005(:, 2)).^(parameters.alpha_na)) ;
    
    cnorm_2005 = max(norm(cost_2005_new(:,1) - cost_2005(:, 1)), norm(cost_2005_new(:,2) - cost_2005(:, 2))) ; %Updating norm
    cost_2005 = cost_2005_new ;   %Updating cost values. 
end
dP_index_2005 = [p_index_ag(cost_2005(:, 1), 2005) , p_index_na(cost_2005(:, 2), 2005)] ;

  %3.2 Solving for agricultural consumption shares
for i = 1:30 %30 provinces. Make international ag consumption share unchanged.
    for k = 1:2 % sectors
     fun = @(x) log(x) - parameters.epsilon*log(1 - x) - (1-parameters.eta)*log(dP_index_2005(i, 1)/dP_index_2005(i, 2)) - (1-parameters.eta)*(parameters.epsilon - 1)*log(dWage_2005(i, k)/dP_index_2005(i, 2)) - log(co_obj.Agspend_2000(i, k)) + parameters.epsilon*log(1 - co_obj.Agspend_2000(i, k)) ;
     Agshare_2005(i, k) = fzero(fun, [0.001, .999], options) ; 
   end
end

%Market clearing/excess demand function
%Calculating trade flows
temp_cost_2005 = repelem([cost_2005(:, 1)' , cost_2005(:, 2)'], N, 1).^(-parameters.theta) ; 
temp_2005_tc = [tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002; tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002].^(-1) ; %NOTE: No trade costs in some specifications
temp_2000_tf = [tradef.ag_2002, tradef.na_2002; tradef.ag_2002, tradef.na_2002] ;
temp_2005_pi = [repelem(dP_index_2005(:, 1), 1 , N/2), repelem(dP_index_2005(:, 2), 1 , N/2) ; repelem(dP_index_2005(:, 1), 1 , N/2), repelem(dP_index_2005(:, 2), 1 , N/2)].^(-parameters.theta) ;
Tradeshare_2005 = (temp_cost_2005.*temp_2005_tc.*temp_2000_tf)./temp_2005_pi ;
%Calculating payments to each sector location
temp_2005_Y = repelem([G_2005(:, 1).*Lab_2005(:, 1) ; G_2005(:, 2).*Lab_2005(:, 2)], 1, N) ; %Matrix of gross output in 2005
temp_2005_agsp = [repelem(Agshare_2005(:, 1), 1, N/2), ones(N/2, N/2) - repelem(Agshare_2005(:, 1), 1, N/2) ; repelem(Agshare_2005(:, 2), 1, N/2), ones(N/2, N/2) - repelem(Agshare_2005(:, 2), 1, N/2) ] ; %matrix of updated ag spending shares
temp_2005_sa = [repelem(parameters.phi_aa, N/2, N/2), repelem(parameters.phi_na, N/2, N/2) ; repelem(parameters.phi_an, N/2, N/2), repelem(parameters.phi_nn, N/2, N/2)] ; %matrix of production network parameters
temp_2005_va = [repelem(parameters.vashare_a, N/2, N) ; repelem(parameters.vashare_n, N/2, N)] ; %matrix of value added shares
temp_2005_L = repelem([Lab_2005(:, 1)' , Lab_2005(:, 2)'], N, 1) ;

MarketClear = ((temp_2005_agsp.*temp_2005_va.*temp_2005_Y + temp_2005_sa.*temp_2005_Y).*Tradeshare_2005);
MarketClear = sum(MarketClear, 1) ;
%Excess demand
ExcessDemand = (MarketClear - [G_2005(:, 1)'.*Lab_2005(:, 1)' , G_2005(:, 2)'.*Lab_2005(:, 2)'])./([G_2005(:, 1)', G_2005(:, 2)']) ;
%Calculating tattonement function. Fixed point is a goods market clearing
%equilibrium
G_2005_new = ([G_2005(:, 1)', G_2005(:, 2)']).*(ones(1, N) + (1/2)*ExcessDemand./[Lab_2005(:, 1)' , Lab_2005(:, 2)']) ;
G_2005_new = G_2005_new/(parameters.vashare_a*sum(G_2005_new(1, [1:30]), 'all') + parameters.vashare_n*sum(G_2005_new(1, [32:61]), 'all'))*60 ;
%Renormalizing so that average value added per worker is 1 in china
%(consistent with normalization in 2000)


%Calculating difference between wages and new wages deduced from
%tattonement process. 
Gnorm_2005 = norm(G_2005_new - [G_2005(:, 1)', G_2005(:, 2)'], 1) ;

%Updating values
G_2005(:, 1) = G_2005_new(1, [1:N/2])' ;
G_2005(:, 2) = G_2005_new(1, [(N/2 + 1):N])' ;

%Renormalizing so that average value added per worker is 1 in china
%(consistent with normalization in 2000)
dWage_2005 = G_2005./(co_obj.nomY_2000./co_obj.emp_lab_2000) ;
aG_2005 = G_2005.*Lab_2005 ;

end %Ending goods market clearing process


%Part 1: Compensating variation in equilibrium
dCompvariation_2005(:, 1) = (dWage_2005(:, 1))./((((co_obj.Agspend_2000(:, 1).*(dP_index_2005(:, 1).^(1-parameters.eta)) + (ones(N/2, 1) - co_obj.Agspend_2000(:, 1)).*(dP_index_2005(:, 2).^(1 - parameters.eta))).^(1/(1-parameters.eta))).^(parameters.nu)).*(dRental_rate_2005(:, 1)).^(1-parameters.nu)) ;
dCompvariation_2005(:, 2) = (dWage_2005(:, 2))./((((co_obj.Agspend_2000(:, 2).*(dP_index_2005(:, 1).^(1-parameters.eta)) + (ones(N/2, 1) - co_obj.Agspend_2000(:, 2)).*(dP_index_2005(:, 2).^(1 - parameters.eta))).^(1/(1-parameters.eta))).^(parameters.nu)).*(dRental_rate_2005(:, 2)).^(1-parameters.nu)) ;

%Part 2: Calculating new labour allocations
temp_dcomp_2005 = repelem(reshape(dCompvariation_2005([1:30], :)', [ N - 2, 1])', N - 2, 1) ;
Migshare_2005 = (temp_dcomp_2005./dM).^(parameters.kappa).*migf.f2000 ; %Numerator for discrete choice problem
Migshare_2005 = Migshare_2005./repelem(sum(Migshare_2005, 2), 1, N-2) ; %Correcting by the denominator for choice probabilities.

%Calculating new labour allocation
temp_lab_2005 = (Migshare_2005'*(reshape(co_obj.registered_lab_2005', [N-2, 1]))) ; 
temp_lab_2005 = reshape(temp_lab_2005, [2 ,(N-2)/2])' ;  %International change in labour is 1
temp_lab_2005(N/2, [1:2]) = [1, 1] ;
Lab_2005_new = temp_lab_2005;

%Updating norm
Lnorm_2005 = norm([Lab_2005_new(:, 1) ; Lab_2005_new(:, 2)] - [Lab_2005(:, 1) ; Lab_2005(:, 2)], 1) 

%Updating labour allocations
Lab_2005 = Lab_2005_new ;
dLab_2005 = Lab_2005./co_obj.emp_lab_2000 ;

end %Ending migration market clearing process.

%Calculating the coefficient of variation of population density)

Density = (Lab_2005([1:30], 1) + Lab_2005([1:30], 2))./co_obj.landmass ;
Coeff_density(aprod, rprod, trc, mic) = sqrt(((N-2)/2 - 1)/((N-2)/2))*std(Density, 1)/mean(Density) ; %Inputting coefficient into table. Recorrecting for Basel's correction here. 

%Repeating for employment weighted variation in population density
wmeanDensity = sum(((Lab_2005([1:30], 1) + Lab_2005([1:30], 2))/sum(Lab_2005([1:30], :), 'all')).*(Lab_2005([1:30], 1) + Lab_2005([1:30], 2))./co_obj.landmass, 'all');
wstdDensity = sqrt(sum(((Lab_2005([1:30], 1) + Lab_2005([1:30], 2))/sum(Lab_2005([1:30], :), 'all')).*((((Lab_2005([1:30], 1) + Lab_2005([1:30], 2))./co_obj.landmass) - wmeanDensity*ones(30, 1)).^2), 'all')) ;

wCoeff_density(aprod, rprod, trc, mic) = wstdDensity/wmeanDensity ;

end
end
end
end

%Outputting coefficient of variation in population density statistics
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData\constructed_output
save density_dispersion Coeff_density
save wdensity_dispersion wCoeff_density
