%Date created: October 15th 2021
%Date edited: October 16th 2021


%This file computes migration costs that rationalize the employment
%distribution in 2005 when trade costs, migration costs and productivity
%all grow at observed levels. 


global N parameters co_obj tradec tradef migc migf

N = 62 ; %number of province-sector pairs. 
%Directory
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData
master = readtable('constructed_outputV2/master3.xlsx') ;
calibrated_GDP_tradebalance_2000 = readtable('constructed_outputV2/Calibrated_GDP_balanced_2000.xlsx') ;
land_mobile = readtable('constructed_outputV2/landprice_mobile.xlsx') ;
load constructed_outputV2/tradecosts_struct ;
load constructed_outputV2/tradeflows_struct ;
load constructed_outputV2/parameters

%Setting up all additional province-sector specific parameters in the co_obj structure

%Labour
co_obj.emp_lab_2000 = [master.L_ag_2000 , master.L_na_2000]  ;
co_obj.emp_lab_2005 = [master.L_ag_2005 , master.L_na_2005]  ;
co_obj.emp_lab_2000(N/2, [1,2]) = [1, 1] ; %Normalization of international labour -- doesn't matter for equilibrium outcome. 
co_obj.emp_lab_2005(N/2, [1,2]) = [1, 1] ;
co_obj.dLab_2005 = co_obj.emp_lab_2005./co_obj.emp_lab_2000 ;

%Productivity
co_obj.dTFP_2005 = [master.dTFP_ag_2005 , master.dTFP_na_2005] ;

%Initial GDP 
co_obj.nomY_balanced_2000 = [master.nomY_ag_2000, master.nomY_na_2005] ;

%Observed GDP as initial condition for solving equilibrium
co_obj.nomY_2005 = [master.nomY_ag_2005, master.nomY_na_2005] ;

%Ag spending shares 
co_obj.Agspend_2000 = [master.agspend_ag_2000, master.agspend_na_2000] ;
co_obj.Agspend_2005 = [master.agspend_ag_2005, master.agspend_na_2005] ;

%Initial land use 
%If land mobile
if parameters.land_mobile == 1
    co_obj.commercial_land_2000 = [land_mobile.ag_commercial_land([1:N/2-1]), land_mobile.na_commercial_land([1:N/2-1])] ;
    co_obj.residential_land_2000 = [land_mobile.ag_residential_land([1:N/2-1]), land_mobile.na_residential_land([1:N/2-1])] ;
    co_obj.landmass = land_mobile.landmass(1:N/2-1) ; %ONLY FOR domestic provinces. 
end

%If land immobile = observed levels, 


%% Part 1: Calculating balanced trade equilibrium migration costs if: 1) TFP changes as observed, 2) Employment is at 2005 levels observed, 3) Trade costs fall as observed, 4) ag consumption shares are at observed levels

cd C:\Users\James\Dropbox\SchoolFolder\SYP\modules\V2 %Going back to code folder

%If land assumed completely mobile: 
if parameters.land_mobile==1
%________________________________________________________________________________________________________________________________
%Intitial values = 2005 equilibrium
Lab = co_obj.emp_lab_2005 ;
dLab = co_obj.emp_lab_2005./co_obj.emp_lab_2000 ;
dGpw = (co_obj.nomY_2005./co_obj.emp_lab_2005)./(co_obj.nomY_balanced_2000./co_obj.emp_lab_2000) ; %Equilibrium change in gross output per worker = eq change in VA/worker = eq change in labour wages initial value
Gpw = co_obj.nomY_2005./co_obj.emp_lab_2005 ; %Equilibrium gross output per worker in levels initial value
G = co_obj.nomY_2005 ; % Observed Gross output in levels as initial value for Equilibrium
Agshare = co_obj.Agspend_2005 ; %Initial value of agricultural shares. Does not matter as updated right away.  ; %International spending shares remain unchanged (we don't allow for endogenous adjustment)

% + Other objects
dCommercial_land = ones(N/2, 2) ; %initial values for change in residential + commercial land 
dResidential_land = ones(N/2, 2) ; 
options = optimset('Display','none') ; %options for fsolve

%%START LOOP HERE
Gnorm = 1 ;

while Gnorm > 0.0000000001
%1: Calculating land allocation implied by the fixed values of dG and
%dLab and perfect land mobility. International land immobile. Change in
%land comes from assume land allocation that equates real rate of return on
%land in 2000
dVA_rat = (G(:, 1)./G(:, 2))./(co_obj.nomY_balanced_2000(:, 1)./co_obj.nomY_balanced_2000(:, 2)) ; %Ratio of change in ag/nonag VA in equilibrium
dCommercial_land([1:N/2-1], 2) = co_obj.landmass([1:N/2-1])./(dVA_rat([1:N/2-1], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat([1:N/2-1], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ; %Change in commercial land
dResidential_land([1:N/2-1], 2) = dCommercial_land([1:N/2-1], 2) ; %change in residential land = change in commercial land if all workers paid same value added
dCommercial_land([1:N/2-1], 1) = (dVA_rat([1:30], 1).*co_obj.landmass([1:N/2-1]))./(dVA_rat([1:N/2-1], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat([1:N/2-1], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ;
dResidential_land([1:N/2-1], 1) = dCommercial_land([1:N/2-1], 1) ; 
dRental_rate = (G./co_obj.nomY_balanced_2000)./dCommercial_land ; %equilibrium change in rental rates

%Test: implied change in rental rates equalize except internationally
test1 = dRental_rate(:, 1) - dRental_rate(:, 2) ; %Equal 

%2: calculating changes in unit costs, price indices
cost = ones(N/2, 2)  ;
cost_new = ones(N/2, 2) ; %initial value for costs. 
cnorm = 1 ;

%Calculating implied costs. 
while cnorm > 0.00001 %Margin of error for fixed point in determining marginal costs.  
    cost_new(:, 1) = ((dGpw(:, 1).^(parameters.vashare_a)).*((dLab(:, 1)./dCommercial_land(:, 1)).^(parameters.land_ag - parameters.alpha_a)).*(p_index_ag(cost(:, 1), 2005).^(parameters.phi_aa)).*(p_index_na(cost(:, 2), 2005).^(parameters.phi_na)))./co_obj.dTFP_2005(:, 1) ;
    cost_new(:, 2) = ((dGpw(:, 2).^(parameters.vashare_n)).*((dLab(:, 2)./dCommercial_land(:, 2)).^(parameters.land_na - parameters.alpha_n)).*(p_index_ag(cost(:, 1), 2005).^(parameters.phi_an)).*(p_index_na(cost(:, 2), 2005).^(parameters.phi_nn)))./co_obj.dTFP_2005(:, 2) ;
   
    cnorm = max(norm(cost_new(:, 1) - cost(:, 1)), norm(cost_new(:, 2) - cost(:, 2))) ;
    cost = cost_new ; %updating new cost vector
end
dP_index = [p_index_ag(cost(:, 1), 2005) , p_index_na(cost(:, 2), 2005)] ;

%3. Solving for agricultural consumption shares if preference parameter unchanging
    %from 2000
%for i = 1:30 %30 provinces. Make international ag consumption share unchanged.
%    for k = 1:2 % sectors
%     fun = @(x) log(x) - parameters.epsilon*log(1 - x) - (1-parameters.eta)*log(dP_index(i, 1)/dP_index(i, 2)) - (1-parameters.eta)*(parameters.epsilon - 1)*log(dGpw(i, k)/dP_index(i, 2)) - log(co_obj.Agspend_2000(i, k)) + parameters.epsilon*log(1 - co_obj.Agspend_2000(i, k)) ;
%     Agshare(i, k) = fzero(fun, [0.001, .999], options) ; 
%   end
%end

%4. Trade shares, market clearing and excess demand
    %Trade shares
temp_cost = repelem([(cost(:, 1)').^(-parameters.theta_a) , (cost(:, 2)').^(-parameters.theta_n)], N, 1) ; %matrix of unit costs, same unit costs within columns
temp_tc = [tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002; tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002].^(-1) ; %matrix of changes in trade costs by region. 
temp_tf = [tradef.ag_2002, tradef.na_2002; tradef.ag_2002, tradef.na_2002] ; %Matrix of 2002 trade flows
temp_pi = [repelem(dP_index(:, 1).^(-parameters.theta_a), 1 , N/2), repelem(dP_index(:, 2).^(-parameters.theta_n), 1 , N/2) ; repelem(dP_index(:, 1).^(-parameters.theta_a), 1 , N/2), repelem(dP_index(:, 2).^(-parameters.theta_n), 1 , N/2)] ; %matrix of changes in the price index. 
Tradeshare = (temp_cost.*temp_tc.*temp_tf)./temp_pi ;
%TEST: do tradeshares sum to 2 (sum to 1 for each half row?)
test2 = sum(Tradeshare, 2) ; %test passed 
%Calculating payments to each sector location
temp_Y = repelem([G(:, 1) ; G(:, 2)], 1, N) ; %Matrix of gross output in 2005
temp_agsp = [repelem(Agshare(:, 1), 1, N/2), ones(N/2, N/2) - repelem(Agshare(:, 1), 1, N/2) ; repelem(Agshare(:, 2), 1, N/2), ones(N/2, N/2) - repelem(Agshare(:, 2), 1, N/2) ] ; %matrix of updated ag spending shares
temp_sa = [repelem(parameters.phi_aa, N/2, N/2), repelem(parameters.phi_na, N/2, N/2) ; repelem(parameters.phi_an, N/2, N/2), repelem(parameters.phi_nn, N/2, N/2)] ; %matrix of production network parameters
temp_va = [repelem(parameters.vashare_a, N/2, N) ; repelem(parameters.vashare_n, N/2, N)] ; %matrix of value added shares

MarketClear = ((temp_agsp.*temp_va.*temp_Y + temp_sa.*temp_Y).*Tradeshare) ; %total payment matrix
MarketClear = sum(MarketClear, 1) ; %Summing total payments made to each sector location
ExcessDemand = (MarketClear - [Gpw(:, 1)'.*Lab(:, 1)' , Gpw(:, 2)'.*Lab(:, 2)'])./([Gpw(:, 1)', Gpw(:, 2)']) ; %excess demand function
%Test: ExcessDemand must sum to 0
test3 = sum(ExcessDemand.*[Gpw(:, 1)', Gpw(:, 2)']) ; %=0 to machine precision

%Calculating tattonement function. Fixed point is a goods market clearing
%equilibrium
Gpw_new = ([Gpw(:, 1)', Gpw(:, 2)']).*(ones(1, N) + (1/2)*ExcessDemand./[Lab(:, 1)' , Lab(:, 2)']) ;
Gpw_new = Gpw_new/geomean([parameters.vashare_a*Gpw_new(1, [1:N/2-1]), parameters.vashare_n*Gpw_new(1, [N/2+1:N-1])]) ; %normalizing so geometric average Value added = 1 in china

Gnorm = norm(Gpw_new - [Gpw(:, 1)', Gpw(:, 2)']) ;

%Updating values
Gpw(:, 1) = Gpw_new(1, [1:N/2])' ;
Gpw(:, 2) = Gpw_new(1, [N/2+1:N])' ;
dGpw = Gpw./(co_obj.nomY_balanced_2000./co_obj.emp_lab_2000) ;
G = Gpw.*Lab ;
end
end

%Calculating preference parameter that rationalizes this change in
%equilibrium

co_obj.CalibratedPrefP_2005 = log(Agshare) - parameters.epsilon*log(ones(N/2, 2) - Agshare) - (1-parameters.eta)*log(repelem(dP_index(:, 1), 1, 2)./repelem(dP_index(:, 2), 1, 2)) - (1-parameters.eta)*(parameters.epsilon - 1)*log(dGpw./repelem(dP_index(:, 2), 1, 2)) ;



%% Part 2: Calculating changes in migration frictions that rationalize this as equilibrium given observed employment distribution. 

%1. Compensating variation associated with the equilibrium
dCompvariation = zeros(N/2, 2) ; 

dCompvariation(:, 1) = (dGpw(:, 1))./((((co_obj.Agspend_2000(:, 1).*(dP_index(:, 1).^(1-parameters.eta)) + (ones(N/2, 1) - co_obj.Agspend_2000(:, 1)).*(dP_index(:, 2).^(1 - parameters.eta))).^(1/(1-parameters.eta))).^(1-parameters.nu)).*(dRental_rate(:, 1)).^(parameters.nu)) ;
dCompvariation(:, 2) = (dGpw(:, 2))./((((co_obj.Agspend_2000(:, 2).*(dP_index(:, 1).^(1-parameters.eta)) + (ones(N/2, 1) - co_obj.Agspend_2000(:, 2)).*(dP_index(:, 2).^(1 - parameters.eta))).^(1/(1-parameters.eta))).^(1-parameters.nu)).*(dRental_rate(:, 2)).^(parameters.nu)) ;

%Omitting international (Compensating variation not interpretable) 
dCompvariation = dCompvariation([1:N/2-1], :) ;


%Reading in migration flow data
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData
migflows_2000 = readtable('2000_migrantflows.xlsx') ; 
migflows_2000(:, 1) = [] ;
migflows_2005 = readtable('2005_migrantflows.xlsx') ;
migflows_2005(:, 1) = [] ;

%Solving for matrix of migration share changes, accounting for income
%elasticity of migration 

temp = repmat(diag(table2array(migflows_2000)), 1, N-2); %matrix of diagonal elements from migflows
norm_kappa_mig_flows_2000 = (table2array(migflows_2000)./temp).^(1/parameters.kappa) ;
temp = repmat(diag(table2array(migflows_2005)), 1, N-2); %matrix of diagonal elements from migflows
norm_kappa_mig_flows_2005 = (table2array(migflows_2005)./temp).^(1/parameters.kappa) ;
clear temp

%Reshapingc comp variation so it is in the format [ag_1, na_1, ag_2,
%na_2... etc]


temp_dCompvariation = reshape(dCompvariation([1:30], :)', [ N - 2, 1]) ;

%Changes in migration costs
dMigrationCost = zeros(N-2, N-2) ;

for i = 1:(N-2)
   for j = 1:(N-2)
       dMigrationCost(i, j) = (temp_dCompvariation(j)/temp_dCompvariation(i))*((norm_kappa_mig_flows_2005(i, j)/norm_kappa_mig_flows_2000(i, j))^(-1)) ;
   end
end

migc = struct() ;
migc.c2005 = dMigrationCost ; %Note: this is the CHANGE IN MIGRATION COSTS.
migf.f2000 = table2array(migflows_2000) ;
migf.f2005 = table2array(migflows_2005) ;

cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData\constructed_outputV2
save migrationcosts_struct migc
save migrationflows_struct migf 


%% Part 3: Doing the same for equilibrium with no changes. -- calibrated to match 2000 employment distribution and 2000 ag consumption shares

cd C:\Users\James\Dropbox\SchoolFolder\SYP\modules\V2

%If land assumed completely mobile: 
if parameters.land_mobile==1
%________________________________________________________________________________________________________________________________
%Intitial values = 2005 equilibrium
Lab = co_obj.emp_lab_2000 ;
dLab = co_obj.emp_lab_2000./co_obj.emp_lab_2000 ;
dGpw = (co_obj.nomY_balanced_2000./co_obj.emp_lab_2000)./(co_obj.nomY_balanced_2000./co_obj.emp_lab_2000) ; %Equilibrium change in gross output per worker = eq change in VA/worker = eq change in labour wages initial value
Gpw = co_obj.nomY_balanced_2000./co_obj.emp_lab_2000 ; %Equilibrium gross output per worker in levels initial value
G = co_obj.nomY_balanced_2000 ; % Observed Gross output in levels as initial value for Equilibrium
Agshare = co_obj.Agspend_2000 ; %Initial value of agricultural shares. (Calibrated to match)

% + Other objects
dCommercial_land = ones(N/2, 2) ; %initial values for change in residential + commercial land 
dResidential_land = ones(N/2, 2) ; 
options = optimset('Display','none') ; %options for fsolve

%%START LOOP HERE
Gnorm = 1 ;

while Gnorm > 0.0000000001
%1: Calculating land allocation implied by the fixed values of dG and
%dLab and perfect land mobility. International land immobile. Change in
%land comes from assume land allocation that equates real rate of return on
%land in 2000
dVA_rat = (G(:, 1)./G(:, 2))./(co_obj.nomY_balanced_2000(:, 1)./co_obj.nomY_balanced_2000(:, 2)) ; %Ratio of change in ag/nonag VA in equilibrium
dCommercial_land([1:N/2-1], 2) = co_obj.landmass([1:N/2-1])./(dVA_rat([1:N/2-1], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat([1:N/2-1], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ; %Change in commercial land
dResidential_land([1:N/2-1], 2) = dCommercial_land([1:N/2-1], 2) ; %change in residential land = change in commercial land if all workers paid same value added
dCommercial_land([1:N/2-1], 1) = (dVA_rat([1:30], 1).*co_obj.landmass([1:N/2-1]))./(dVA_rat([1:N/2-1], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat([1:N/2-1], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ;
dResidential_land([1:N/2-1], 1) = dCommercial_land([1:N/2-1], 1) ; 
dRental_rate = (G./co_obj.nomY_balanced_2000)./dCommercial_land ; %equilibrium change in rental rates

%Test: implied change in rental rates equalize except internationally
test1 = dRental_rate(:, 1) - dRental_rate(:, 2) ; %Equal 

%2: calculating changes in unit costs, price indices
cost = ones(N/2, 2)  ;
cost_new = ones(N/2, 2) ; %initial value for costs. 
cnorm = 1 ;

%Calculating implied costs. 
while cnorm > 0.00001 %Margin of error for fixed point in determining marginal costs.  
    cost_new(:, 1) = ((dGpw(:, 1).^(parameters.vashare_a)).*((dLab(:, 1)./dCommercial_land(:, 1)).^(parameters.land_ag - parameters.alpha_a)).*(p_index_ag_notr(cost(:, 1), 2005).^(parameters.phi_aa)).*(p_index_na_notr(cost(:, 2), 2005).^(parameters.phi_na))) ;
    cost_new(:, 2) = ((dGpw(:, 2).^(parameters.vashare_n)).*((dLab(:, 2)./dCommercial_land(:, 2)).^(parameters.land_na - parameters.alpha_n)).*(p_index_ag_notr(cost(:, 1), 2005).^(parameters.phi_an)).*(p_index_na_notr(cost(:, 2), 2005).^(parameters.phi_nn))) ;
   
    cnorm = max(norm(cost_new(:, 1) - cost(:, 1)), norm(cost_new(:, 2) - cost(:, 2))) ;
    cost = cost_new ; %updating new cost vector
end
dP_index = [p_index_ag_notr(cost(:, 1), 2005) , p_index_na_notr(cost(:, 2), 2005)] ;

%3. Solving for agricultural consumption shares if preference parameter unchanging
    %from 2000
%for i = 1:30 %30 provinces. Make international ag consumption share unchanged.
%    for k = 1:2 % sectors
%     fun = @(x) log(x) - parameters.epsilon*log(1 - x) - (1-parameters.eta)*log(dP_index(i, 1)/dP_index(i, 2)) - (1-parameters.eta)*(parameters.epsilon - 1)*log(dGpw(i, k)/dP_index(i, 2)) - log(co_obj.Agspend_2000(i, k)) + parameters.epsilon*log(1 - co_obj.Agspend_2000(i, k)) ;
%     Agshare(i, k) = fzero(fun, [0.001, .999], options) ; 
%   end
%end

%4. Trade shares, market clearing and excess demand
    %Trade shares
temp_cost = repelem([(cost(:, 1)').^(-parameters.theta_a) , (cost(:, 2)').^(-parameters.theta_n)], N, 1) ; %matrix of unit costs, same unit costs within columns
temp_tc = [tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002; tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002].^(-1) ; %matrix of changes in trade costs by region. 
temp_tf = [tradef.ag_2002, tradef.na_2002; tradef.ag_2002, tradef.na_2002] ; %Matrix of 2002 trade flows
temp_pi = [repelem(dP_index(:, 1).^(-parameters.theta_a), 1 , N/2), repelem(dP_index(:, 2).^(-parameters.theta_n), 1 , N/2) ; repelem(dP_index(:, 1).^(-parameters.theta_a), 1 , N/2), repelem(dP_index(:, 2).^(-parameters.theta_n), 1 , N/2)] ; %matrix of changes in the price index. 
Tradeshare = (temp_cost.*temp_tc.*temp_tf)./temp_pi ;
%TEST: do tradeshares sum to 2 (sum to 1 for each half row?)
test2 = sum(Tradeshare, 2) ; %test passed 
%Calculating payments to each sector location
temp_Y = repelem([G(:, 1) ; G(:, 2)], 1, N) ; %Matrix of gross output in 2005
temp_agsp = [repelem(Agshare(:, 1), 1, N/2), ones(N/2, N/2) - repelem(Agshare(:, 1), 1, N/2) ; repelem(Agshare(:, 2), 1, N/2), ones(N/2, N/2) - repelem(Agshare(:, 2), 1, N/2) ] ; %matrix of updated ag spending shares
temp_sa = [repelem(parameters.phi_aa, N/2, N/2), repelem(parameters.phi_na, N/2, N/2) ; repelem(parameters.phi_an, N/2, N/2), repelem(parameters.phi_nn, N/2, N/2)] ; %matrix of production network parameters
temp_va = [repelem(parameters.vashare_a, N/2, N) ; repelem(parameters.vashare_n, N/2, N)] ; %matrix of value added shares

MarketClear = ((temp_agsp.*temp_va.*temp_Y + temp_sa.*temp_Y).*Tradeshare) ; %total payment matrix
MarketClear = sum(MarketClear, 1) ; %Summing total payments made to each sector location
ExcessDemand = (MarketClear - [Gpw(:, 1)'.*Lab(:, 1)' , Gpw(:, 2)'.*Lab(:, 2)'])./([Gpw(:, 1)', Gpw(:, 2)']) ; %excess demand function
%Test: ExcessDemand must sum to 0
test3 = sum(ExcessDemand.*[Gpw(:, 1)', Gpw(:, 2)']) ; %=0 to machine precision

%Calculating tattonement function. Fixed point is a goods market clearing
%equilibrium
Gpw_new = ([Gpw(:, 1)', Gpw(:, 2)']).*(ones(1, N) + (1/2)*ExcessDemand./[Lab(:, 1)' , Lab(:, 2)']) ;
Gpw_new = Gpw_new/geomean([parameters.vashare_a*Gpw_new(1, [1:N/2-1]), parameters.vashare_n*Gpw_new(1, [N/2+1:N-1])]) ; %normalizing so geometric average Value added = 1 in china

Gnorm = norm(Gpw_new - [Gpw(:, 1)', Gpw(:, 2)']) ;

%Updating values
Gpw(:, 1) = Gpw_new(1, [1:N/2])' ;
Gpw(:, 2) = Gpw_new(1, [N/2+1:N])' ;
dGpw = Gpw./(co_obj.nomY_balanced_2000./co_obj.emp_lab_2000) ;
G = Gpw.*Lab ;
end
end

%Calculating preference parameter that rationalizes this change in
%equilibrium
co_obj.CalibratedPrefP_2000 = log(Agshare) - parameters.epsilon*log(ones(N/2, 2) - Agshare) - (1-parameters.eta)*log(repelem(dP_index(:, 1), 1, 2)./repelem(dP_index(:, 2), 1, 2)) - (1-parameters.eta)*(parameters.epsilon - 1)*log(dGpw./repelem(dP_index(:, 2), 1, 2)) ;


%% Part 4: calculating migration frictions associated with rationalizing 2000 employment distribution 

%1. Compensating variation associated with the equilibrium
dCompvariation = zeros(N/2, 2) ; 

dCompvariation(:, 1) = (dGpw(:, 1))./((((co_obj.Agspend_2000(:, 1).*(dP_index(:, 1).^(1-parameters.eta)) + (ones(N/2, 1) - co_obj.Agspend_2000(:, 1)).*(dP_index(:, 2).^(1 - parameters.eta))).^(1/(1-parameters.eta))).^(1-parameters.nu)).*(dRental_rate(:, 1)).^(parameters.nu)) ;
dCompvariation(:, 2) = (dGpw(:, 2))./((((co_obj.Agspend_2000(:, 2).*(dP_index(:, 1).^(1-parameters.eta)) + (ones(N/2, 1) - co_obj.Agspend_2000(:, 2)).*(dP_index(:, 2).^(1 - parameters.eta))).^(1/(1-parameters.eta))).^(1-parameters.nu)).*(dRental_rate(:, 2)).^(parameters.nu)) ;

%Omitting international (Compensating variation not interpretable) 
dCompvariation = dCompvariation([1:N/2-1], :) ;

%Reshapingc comp variation so it is in the format [ag_1, na_1, ag_2,
%na_2... etc]


temp_dCompvariation = reshape(dCompvariation([1:30], :)', [ N - 2, 1]) ;

%Changes in migration costs
dMigrationCost = zeros(N-2, N-2) ;

for i = 1:(N-2)
   for j = 1:(N-2)
       dMigrationCost(i, j) = (temp_dCompvariation(j)/temp_dCompvariation(i)) ;
   end
end

migc.c2000 = dMigrationCost ; %Note: this is the CHANGE IN MIGRATION COSTS to rationalized balanced trade  + 2000 equilibrium.

cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData\constructed_outputV2
save migrationcosts_struct migc
save counterfactual_objects co_obj