
%This file performs the main counterfactual experiment from 2000-2005 and 2005-2010 given:
%Changes in productivity (both average TFP and TFP)
%Changes in trade costs 
%Changes in migration costs.

%Date created: June 15th, 2021
%Date modified: June 18th, 2021


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
parameters.alpha_na = 0.00 ;
parameters.alpha_ag = 0.00 ;

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
%TFP growth after adjusting for production network elasticity
parameters.Pa_2005 = (parameters1.Gn_2005_nls(1)*parameters1.Ga_Gn_2005(1))^(-1/(1 + 1 - parameters.vashare_a)) ;
parameters.Pn_2005 = parameters1.Gn_2005_nls(1)^(-1/(1 + 1 - parameters.vashare_n)) ;
parameters.Pa_2010 = (parameters1.Gn_2010_nls*parameters1.Ga_Gn_2010(1))^(-1/(1 + 1 - parameters.vashare_a)) ;
parameters.Pn_2010 = parameters1.Gn_2010_nls^(-1/(1 + 1 - parameters.vashare_n)) ;
parameters.kappa = 1.5 ; %Tombe and Zhu (2019) income elasticity of migration.
parameters.nu = 0.87 ; %non-housing expenditure share 



%Reading data 
%master data 
master =  readtable('constructed_output/master2.xlsx') ;
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

%Adjusting for population density growth and TFP growth
adj_2005 = [(demp_lab_2005(:, 1)./d_landuse_2005(:, 1)).^(parameters.alpha_ag) , (demp_lab_2005(:, 2)./d_landuse_2005(:, 2)).^(parameters.alpha_na)] ;
adj_2010 = [(demp_lab_2010(:, 1)./d_landuse_2010(:, 1)).^(parameters.alpha_ag) , (demp_lab_2010(:, 2)./d_landuse_2010(:, 2)).^(parameters.alpha_na)] ;

co_obj.dTFP_2005 = co_obj.dTFP_2005./(adj_2005) ;
adjPa_2005 = mean(co_obj.dTFP_2005([1:(N-2)/2], 1)) ;
adjPn_2005 = mean(co_obj.dTFP_2005([1:(N-2)/2], 2)) ; 
co_obj.dTFP_2010 = co_obj.dTFP_2010./(adj_2010) ;
adjPa_2010 = mean(co_obj.dTFP_2010([1:(N-2)/2], 1)) ;
adjPn_2010 = mean(co_obj.dTFP_2010([1:(N-2)/2], 2)) ;

%Capital stock growth
co_obj.dK_2005 = [(master.K_ag_2005./master.K_ag_2000) , (master.K_na_2005./master.K_na_2000)] ;
co_obj.dK_2005(N/2, [1, 2]) = [1 , 1] ;
co_obj.dK_2010 = [(master.K_ag_2005./master.K_ag_2000) , (master.K_na_2005./master.K_na_2000)] ;
co_obj.dK_2010(N/2, [1, 2]) = [1 , 1] ; %Absorbing international capital growth into TFP term (i.e assume international gets no TFP)

%Average capital stock/TFP growth for china only. Used for Shapely value
%decomposition
co_obj.AdTFP_2005 = [[repelem(adjPa_2005, (1/2)*(N-2), 1); co_obj.dTFP_2005(N-1)] , [repelem(adjPn_2005, (1/2)*(N-2), 1); co_obj.dTFP_2005(N-1)]] ;
co_obj.AdTFP_2010 = [[repelem(adjPa_2010, (1/2)*(N-2), 1); co_obj.dTFP_2010(N-1)] , [repelem(adjPn_2010, (1/2)*(N-2), 1); co_obj.dTFP_2010(N-1)]] ;

%Initial gross output values
co_obj.nomY_2000 = [master.nomY_ag_2000 , master.nomY_na_2000] ;
co_obj.nomY_2005 = [master.nomY_ag_2005 , master.nomY_na_2005] ;

co_obj.Agspend_2000 = [master.agspend_ag_2000, master.agspend_na_2000] ;
co_obj.Agspend_2005 = [master.agspend_ag_2005, master.agspend_na_2005] ;

co_obj.commercial_land_2000 = [landuse.ag_commercial_land([1:30]), landuse.na_commercial_land([1:30])] ;
co_obj.commercial_land_2005 = [landuse.ag_commercial_land([31:60]), landuse.na_commercial_land([31:60])] ;
co_obj.residential_land_2000 = [landuse.ag_residential_land([1:30]), landuse.na_residential_land([1:30])] ;
co_obj.residential_land_2005 = [landuse.ag_residential_land([31:60]), landuse.na_residential_land([31:60])] ;
co_obj.landmass = landuse.landmass(1:30) ;


%% Part 2: Calculate implied equilibrium from entire model. 
cd C:\Users\James\Dropbox\SchoolFolder\SYP\modules
%state vars for convergence: dVA or dGrossIncome (31x2) and dLabour (30x2)

%test: 2005 first. 
clearvars -except co_obj migc migf tradec tradef master master_noint N parameters

%Initializing equilibrium objects that are state variables for convergence
%initial values for gross output per worker. 
Lab_2005 = co_obj.emp_lab_2005 ;
dLab_2005 = co_obj.emp_lab_2005./co_obj.emp_lab_2000 ;
nY_2005 = ones(N/2, 2) ; %Initial values for gross output
dVA_2005 = (nY_2005./co_obj.nomY_2000) ; %Initial values for aggregate change in VA/gross output


Labnorm_2005 = 1 ;
nYnorm_2005 = 1 ;

%+Other objects. 
dCommercial_land_2005 = ones(N/2, 2) ;
dResidential_land_2005 = ones(N/2, 2) ; 
dCompvariation_2005 = ones(N/2, 2) ;
Agshare_2005 = ones(N/2, 2)/2 ; %Initializing. Values here don't matter. 
%Agshare_2005(N/2, [1:2]) = co_obj.Agspend_2005(N/2, [1:2]) ; %International spending shares remain unchanged.  
iter_no = 0;

while nYnorm_2005 > 0.0001 %|| Labnorm_2005 > 0.0001
iter_no = iter_no + 1 ;
%__________________________________________________________________________
%1: Calculating land allocation implied by the fixed values of dVA and
%dLab and perfect land mobility. International land immobile. 
%dVA_rat_2005 = dVA_2005(:, 1)./dVA_2005(:, 2) ;
%dCommercial_land_2005([1:30], 2) = co_obj.landmass([1:30])./(dVA_rat_2005([1:30], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat_2005([1:30], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ;
%dResidential_land_2005([1:30], 2) = dCommercial_land_2005([1:30], 2) ;
%dCommercial_land_2005([1:30], 1) = (dVA_rat_2005([1:30], 1).*co_obj.landmass([1:30]))./(dVA_rat_2005([1:30], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat_2005([1:30], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ;
%dResidential_land_2005([1:30], 1) = dCommercial_land_2005([1:30], 1) ; 
%dRental_rate_2005 = dVA_2005(:, 1)./dCommercial_land_2005(:, 1) ; %Change in VA per unit of land in any sector

%__________________________________________________________________________
%2: Calculating dmarginalcosts given dVA and dLabour. NOTE: DIFFERENT
%MODELS WILL NEED CHANGE IN CODE IN THIS SECTION-- TFP/Capital and Trade
%costs.

cost_2005 = ones(N/2, 2)  ;
cost_2005_new = ones(N/2, 2) ; %initial value for costs. 
cnorm_2005 = 1 ;

%IN LOOP: TOTAL TFP, CAPITAL, TRADE COSTS
while cnorm_2005 > 0.00001 %Margin of error for fixed point in determining marginal costs.  
    cost_2005_new(:, 1) = (dVA_2005(:, 1).^(parameters.vashare_a))./((dLab_2005(:, 1).^(parameters.vashare_a))) ;
    cost_2005_new(:, 1) = (cost_2005_new(:, 1).*(p_index_ag(cost_2005(:, 1), 2005).^(parameters.phi_aa)).*(p_index_na(cost_2005(:, 2), 2005).^(parameters.phi_na)))./(co_obj.dTFP_2005(:, 1)) ;
    
    cost_2005_new(:, 2) = (dVA_2005(:, 2).^(parameters.vashare_n))./((dLab_2005(:, 2).^(parameters.vashare_n))) ;
    cost_2005_new(:, 2) = (cost_2005_new(:, 2).*(p_index_ag(cost_2005(:, 1), 2005).^(parameters.phi_an)).*(p_index_na(cost_2005(:, 2), 2005).^(parameters.phi_nn)))./(co_obj.dTFP_2005(:, 2)) ;
    
    cnorm_2005 = max(norm(cost_2005_new(:,1) - cost_2005(:, 1)), norm(cost_2005_new(:,2) - cost_2005(:, 2))) ; %Updating norm
    cost_2005 = cost_2005_new ;   %Updating cost values. 
end

%Updating p_index values. 
dP_index_2005 = [p_index_ag(cost_2005(:, 1), 2005) , p_index_na(cost_2005(:, 2), 2005)] ;

%4: Calculating new dnomY allocation from goods market clearing. 
%Structure for matrix: [prov1_ag, prov2_ag, prov3_ag... prov1_na, etc]
MarketClear = zeros(N, N) ; %matrix to arrive at market clearing, rowsum will be sum of payments to location sector i

%4.1 Constructing matrix of new trade shares. NOTE ASSUMES TRADE COSTS ARE
%CHANGING!
temp_cost_2005 = repelem([cost_2005(:, 1)' , cost_2005(:, 2)'], N, 1).^(-parameters.theta) ; 
temp_2005_tc = [tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002; tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002].^(-1) ; %NOTE: No trade costs in some specifications
temp_2000_tf = [tradef.na_2002, tradef.na_2002; tradef.na_2002, tradef.na_2002] ;
temp_2005_pi = [repelem(dP_index_2005(:, 1), 1 , N/2), repelem(dP_index_2005(:, 2), 1 , N/2) ; repelem(dP_index_2005(:, 1), 1 , N/2), repelem(dP_index_2005(:, 2), 1 , N/2)].^(-parameters.theta) ;
Tradeshare_2005 = (temp_cost_2005.*temp_2005_tc.*temp_2000_tf)./temp_2005_pi ;
%4.2 Constructing matrix of spending allocations
temp_2005_Y = repelem([nY_2005(:, 1) ; nY_2005(:, 2)], 1, N) ; %Matrix of gross output in 2005
temp_2005_agsp = [repelem(Agshare_2005(:, 1), 1, N/2), ones(N/2, N/2) - repelem(Agshare_2005(:, 1), 1, N/2) ; repelem(Agshare_2005(:, 2), 1, N/2), ones(N/2, N/2) - repelem(Agshare_2005(:, 2), 1, N/2) ] ; %matrix of updated ag spending shares
temp_2005_sa = [repelem(parameters.phi_aa, N/2, N/2), repelem(parameters.phi_na, N/2, N/2) ; repelem(parameters.phi_an, N/2, N/2), repelem(parameters.phi_nn, N/2, N/2)] ; %matrix of production network parameters
temp_2005_va = [repelem(parameters.vashare_a, N/2, N) ; repelem(parameters.vashare_n, N/2, N)] ; %matrix of value added shares

%Putting this all into "market clear", taking rowsum
MarketClear = ((temp_2005_agsp.*temp_2005_va.*temp_2005_Y + temp_2005_sa.*temp_2005_Y).*Tradeshare_2005) ;
MarketClear = sum(MarketClear, 1) ;

%Updating dVA with payments to sector (i, k)
nY_2005_new(:, 1) = MarketClear([1:N/2])';
nY_2005_new(:, 2) = MarketClear([((N/2)+1):N])' ;

%%
%__________________________________________________________________________
%5: Calculating new dLabour allocation from labour market clearing
%New migration share matrix 
%Note: structure is [prov1_ag, prov1_na, etc]
%temp_dcomp_2005 = repelem(reshape(dCompvariation_2005([1:30], :)', [ N - 2, 1])', N - 2, 1) ;
%Migshare_2005 = (temp_dcomp_2005./migc.c2005).^(parameters.kappa).*migf.f2000 ; %Numerator for discrete choice problem
%Migshare_2005 = Migshare_2005./repelem(sum(Migshare_2005, 2), 1, N-2) ; %Correcting by the denominator for choice probabilities.

%Calculating new labour allocation
%temp_lab_2005 = (Migshare_2005'*(reshape(co_obj.registered_lab_2005', [N-2, 1]))) ; 
%temp_lab_2005 = reshape(temp_lab_2005, [2 ,(N-2)/2])' ;  %International change in labour is 1
%temp_lab_2005(N/2, [1:2]) = [1, 1] ;
%Lab_2005_new = temp_lab_2005;

%__________________________________________________________________________
%6: Checking norm + updating values 
nY_norm_2005 = norm(nY_2005_new - nY_2005, 1)
%Labnorm_2005 = norm(Lab_2005_new - Lab_2005, 1) 

nY_2005 = nY_2005_new ;
%Lab_2005 = Lab_2005_new ;
dVA_2005 = (nY_2005)./co_obj.nomY_2000 ; %
%dLab_2005 = Lab_2005./co_obj.emp_lab_2000 ;

pause(1)
end