%Date created: November 13th 2021
%Date edited: November 13th 2021


%This file computes an alternative counterfactual in which aggregate GDP
%growth falls to match it observed levels of agricultural spending in 1985, holding total population, trade flows, migration costs/hukou registrants fixed at 2005 levels.  
%The reason why is due to data constraints. 


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
load constructed_outputV2/migrationcosts_struct ;
load constructed_outputV2/migrationflows_struct ;
load constructed_outputV2/counterfactual_objects ;
pref_estimates = readtable('constructed_outputV2/pref_estimates.xlsx') ;

%%
%One: calculating fall in productivity associated with 1985 equilibrium
avg_prodfall_n = pref_estimates.extrapolation_pricefall_nopointest.^(-parameters.vashare_n) ;
avg_prodfall_a = pref_estimates.extrapolation_pricefall_nopointest.^(-parameters.vashare_a) ;

dTFP = [repelem(avg_prodfall_a, N/2, 1), repelem(avg_prodfall_n, N/2, 1)] ;
%%

cd C:\Users\James\Dropbox\SchoolFolder\SYP\modules\V2

%COUNTERFACTUAL
%Intitial values = 2000 equilibrium
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

%Other objects = 2000 equilibrium
dPrefParameter_mat = co_obj.CalibratedPrefP_2000 ;
dM = migc.c2000 ; 
lab_registered = co_obj.registered_lab_2000 ;
pindexa = @p_index_ag_notr ;
pindexn = @p_index_na_notr ;

%%START LOOP HERE
Gnorm = 1 ;
Lnorm = 1 ;
    while Lnorm > 0.0001
        Gnorm = 1 ;
           while Gnorm > 0.0001 %starting goods market clearing process
               
%1: Calculating land allocation implied by the fixed values of dG and
%dLab and perfect land mobility. International land immobile. Change in
%land comes from assume land allocation that equates real rate of return on
%land in 2000
if parameters.land_mobile==1
    dVA_rat = (G(:, 1)./G(:, 2))./(co_obj.nomY_balanced_2000(:, 1)./co_obj.nomY_balanced_2000(:, 2)) ; %Ratio of change in ag/nonag VA in equilibrium
    dCommercial_land([1:N/2-1], 2) = co_obj.landmass([1:N/2-1])./(dVA_rat([1:N/2-1], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat([1:N/2-1], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ; %Change in commercial land
    dResidential_land([1:N/2-1], 2) = dCommercial_land([1:N/2-1], 2) ; %change in residential land = change in commercial land if all workers paid same value added
    dCommercial_land([1:N/2-1], 1) = (dVA_rat([1:30], 1).*co_obj.landmass([1:N/2-1]))./(dVA_rat([1:N/2-1], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat([1:N/2-1], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ;
    dResidential_land([1:N/2-1], 1) = dCommercial_land([1:N/2-1], 1) ; 
    dRental_rate = (G./co_obj.nomY_balanced_2000)./dResidential_land ; %equilibrium change in rental rates
end

if parameters.land_mobile==0
    %Equilibrium changes in rental rates (note: change in rental rate for
    %agricultural productive land = change in rental rates for land in
    %consumption for ag workers 
   dRental_rate = (G./co_obj.nomY_balanced_2000)./dCommercial_land ;
end

%Test: implied change in rental rates equalize except internationally
test1 = dRental_rate(:, 1) - dRental_rate(:, 2) ; %Equal 

%2: calculating changes in unit costs, price indices
cost = ones(N/2, 2)  ;
cost_new = ones(N/2, 2) ; %initial value for costs. 
cnorm = 1 ;

%Calculating implied costs. 
while cnorm > 0.00001 %Margin of error for fixed point in determining marginal costs.  
    cost_new(:, 1) = ((dGpw(:, 1).^(parameters.vashare_a)).*((dLab(:, 1)./dCommercial_land(:, 1)).^(parameters.land_ag - parameters.alpha_a)).*(pindexa(cost(:, 1), 2005).^(parameters.phi_aa)).*(pindexn(cost(:, 2), 2005).^(parameters.phi_na)))./dTFP(:, 1) ;
    cost_new(:, 2) = ((dGpw(:, 2).^(parameters.vashare_n)).*((dLab(:, 2)./dCommercial_land(:, 2)).^(parameters.land_na - parameters.alpha_n)).*(pindexa(cost(:, 1), 2005).^(parameters.phi_an)).*(pindexn(cost(:, 2), 2005).^(parameters.phi_nn)))./dTFP(:, 2) ;
   
    cnorm = max(norm(cost_new(:, 1) - cost(:, 1)), norm(cost_new(:, 2) - cost(:, 2))) ;
    cost = cost_new ; %updating new cost vector
end
dP_index = [pindexa(cost(:, 1), 2005) , pindexn(cost(:, 2), 2005)] ;

%3. Solving for agricultural consumption shares if preference parameter unchanging
    %from 2000
for i = 1:30 %30 provinces. Make international ag consumption share unchanged.
    for k = 1:2 % sectors
     fun = @(x) log(x) - parameters.epsilon*log(1 - x) - (1-parameters.eta)*log(dP_index(i, 1)/dP_index(i, 2)) - (1-parameters.eta)*(parameters.epsilon - 1)*log(dGpw(i, k)/dP_index(i, 2)) - dPrefParameter_mat(i, k) ;
     Agshare(i, k) = fzero(fun, [0.000001, .9999999], options) ; 
   end
end

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
           
           end %Ending goods market clearing process

%______________Labour market clearing______________

%Part 1: Compensating variation in equilibrium 
dCompvariation(:, 1) = (dGpw(:, 1))./((((co_obj.Agspend_2000(:, 1).*(dP_index(:, 1).^(1-parameters.eta)) + (ones(N/2, 1) - co_obj.Agspend_2000(:, 1)).*(dP_index(:, 2).^(1 - parameters.eta))).^(1/(1-parameters.eta))).^(1-parameters.nu)).*(dRental_rate(:, 1)).^(parameters.nu)) ;
dCompvariation(:, 2) = (dGpw(:, 2))./((((co_obj.Agspend_2000(:, 2).*(dP_index(:, 1).^(1-parameters.eta)) + (ones(N/2, 1) - co_obj.Agspend_2000(:, 2)).*(dP_index(:, 2).^(1 - parameters.eta))).^(1/(1-parameters.eta))).^(1-parameters.nu)).*(dRental_rate(:, 2)).^(parameters.nu)) ;

%Part 2: Calculating migration matrix
temp_dcomp = repelem(reshape(dCompvariation([1:N/2-1], :)', [ N - 2, 1])', N - 2, 1) ; %Reshaping compensating variation
Migshare = ((temp_dcomp./dM).^(parameters.kappa)).*migf.f2000 ; %numerator for discrete location choice problem 
Migshare = Migshare./repelem(sum(Migshare, 2), 1, N-2) ; %Correcting by numerator for migration flows
test4 = sum(Migshare, 2) ; % test passed, sum to 1 

%Part 3: Calculating new labour allocation
temp_lab = (Migshare'*(reshape(lab_registered', [N-2, 1]))) ; 
temp_lab = reshape(temp_lab, [2 ,(N-2)/2])' ;  %International change in labour is 1 (normalization)
temp_lab(N/2, [1:2]) = [1, 1] ;
Lab_new = temp_lab ;

%Part 4: Updating norms
Lnorm = norm([Lab_new(:, 1) ; Lab_new(:, 2)] - [Lab(:, 1) ; Lab(:, 2)], 1) 

%Updating labour allocations
Lab = Lab_new ;
dLab = Lab./co_obj.emp_lab_2000 ;

    end %END labour market clearing process

Density = (Lab([1:N/2-1], 1) + Lab([1:N/2-1], 2))./co_obj.landmass ;
Coeff_density = sqrt(((N-2)/2 - 1)/((N-2)/2))*std(Density)/mean(Density) ; %Inputting coefficient into table. Recorrecting for Basel's correction here. 
%Compared to the 1.1 implied by equilibrium.
