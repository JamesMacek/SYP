%Date created: October 16th 2021
%Date edited: October 18th 2021


%This file computes statistics for the main counterfactual in the paper.


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
load constructed_outputV2/counterfactual_objects

%% Part 1: Setting up additional parameters in co_obj

resh_emp_lab_2000 = reshape([master.L_ag_2000([1:N/2-1], 1)' ; master.L_na_2000([1:N/2-1], 1)'], [N-2, 1]) ; %Reshaped employment structure
resh_emp_lab_2005 = reshape([master.L_ag_2005([1:N/2-1], 1)' ; master.L_na_2005([1:N/2-1], 1)'], [N-2, 1]) ;

%Registered workers (Inverting migration matrix) 
co_obj.registered_lab_2000 = (migf.f2000')\resh_emp_lab_2000 ;
co_obj.registered_lab_2005 = (migf.f2005')\resh_emp_lab_2005 ;
co_obj.registered_lab_2000 = reshape(co_obj.registered_lab_2000, [2, (N-2)/2])' ;
co_obj.registered_lab_2005 = reshape(co_obj.registered_lab_2005, [2, (N-2)/2])' ;

%Relative and uniform productivity growth
co_obj.RdTFP_2005 = [co_obj.dTFP_2005(:, 1)/geomean(co_obj.dTFP_2005(:, 1)) , co_obj.dTFP_2005(:, 2)/geomean(co_obj.dTFP_2005(:, 2))] ; %Preserves distribution of productivity but geometric avg=1
co_obj.AdTFP_2005 = [repelem(geomean(co_obj.dTFP_2005(:, 1)), N/2, 1) , repelem(geomean(co_obj.dTFP_2005(:, 2)), N/2, 1)] ;


%% Part 2: Calculate implied equilibrium from entire model.
cd C:\Users\James\Dropbox\SchoolFolder\SYP\modules\V2

%If land assumed entirely mobile 
if parameters.land_mobile==1
     %2 for on, 1 for off
    for rprod = 1:2
    for trc = 1:2
    for mic = 1:2
    for dRegisteredWorkers = 1:2
    for dPreferenceParameter = 1:2
    for aprod = 1:2   
        
        
    %____SETTING UP VALUES FOR EACH COUNTERFACTUAL EQUILIBRIUM
     dTFP = ones(N/2, 2) ; %Change in TFP placeholder
     dM = migc.c2000 ; %change in migration cost if ==1
     lab_registered = co_obj.registered_lab_2000 ;
     dPrefParameter_mat = co_obj.CalibratedPrefP_2000 ;
        
    if aprod == 2 && rprod == 2
       dTFP = co_obj.dTFP_2005 ;
    end
    
    if aprod == 2 && rprod == 1
       dTFP = co_obj.AdTFP_2005 ;
    end
    
             
    if aprod == 1 && rprod == 2
        dTFP = co_obj.RdTFP_2005 ;
    end
    
    if trc == 2
       pindexa = @p_index_ag ;
       pindexn = @p_index_na ;
    else 
       pindexa = @p_index_ag_notr ;
       pindexn = @p_index_na_notr ;
    end
    
    if dRegisteredWorkers == 2
       lab_registered = co_obj.registered_lab_2005 ;
    end
    
    if dPreferenceParameter == 2
        dPrefParameter_mat = co_obj.CalibratedPrefP_2005 ;
    end
    
    if mic == 2
      dM = migc.c2005 ;
    end
%________________________________________________________________________________________________________________________________
%Intitial values = 2005 equilibrium
Lab = co_obj.emp_lab_2005 ;
dLab = co_obj.emp_lab_2005./co_obj.emp_lab_2000 ;
dGpw = (co_obj.nomY_2005./co_obj.emp_lab_2005)./(co_obj.nomY_balanced_2000./co_obj.emp_lab_2000) ; %Equilibrium change in gross output per worker = eq change in VA/worker = eq change in labour wages initial value
Gpw = co_obj.nomY_2005./co_obj.emp_lab_2005 ; %Equilibrium gross output per worker in levels initial value
G = co_obj.nomY_2005 ; % Observed Gross output in levels as initial value for Equilibrium
Agshare = ones(N/2, 2)*0.2 ; %Initial value of agricultural shares. Does not matter as updated right away. 

if dPreferenceParameter == 2 
  Agshare(N/2, [1:2]) = co_obj.Agspend_2005(N/2, [1:2]) ;
else 
  Agshare(N/2, [1:2]) = co_obj.Agspend_2000(N/2, [1:2]) ; %setting international spending shares to change along with dPreferenceParameter.       
end
  %International spending shares remain unchanged (we don't allow for endogenous adjustment)

if dRegisteredWorkers == 1
Lab = co_obj.emp_lab_2000 ;
dLab = co_obj.emp_lab_2000./co_obj.emp_lab_2000 ;
dGpw = (co_obj.nomY_balanced_2000./co_obj.emp_lab_2000)./(co_obj.nomY_balanced_2000./co_obj.emp_lab_2000) ; %Equilibrium change in gross output per worker = eq change in VA/worker = eq change in labour wages initial value
Gpw = co_obj.nomY_balanced_2000./co_obj.emp_lab_2000 ; %Equilibrium gross output per worker in levels initial value
G = co_obj.nomY_balanced_2000 ;
end
% + Other objects
dCommercial_land = ones(N/2, 2) ; %initial values for change in residential + commercial land 
dResidential_land = ones(N/2, 2) ; 
options = optimset('Display','none') ; %options for fsolve

%%START LOOP HERE

Lnorm = 1 ;
    while Lnorm > 0.0001
        Gnorm = 1 ;
           while Gnorm > 0.0001 %starting goods market clearing process
               
%1: Calculating land allocation implied by the fixed values of dG and
%dLab and perfect land mobility. International land immobile. Change in
%land comes from assume land allocation that equates real rate of return on
%land in 2000
dVA_rat = (G(:, 1)./G(:, 2))./(co_obj.nomY_balanced_2000(:, 1)./co_obj.nomY_balanced_2000(:, 2)) ; %Ratio of change in ag/nonag VA in equilibrium
dCommercial_land([1:N/2-1], 2) = co_obj.landmass([1:N/2-1])./(dVA_rat([1:N/2-1], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat([1:N/2-1], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ; %Change in commercial land
dResidential_land([1:N/2-1], 2) = dCommercial_land([1:N/2-1], 2) ; %change in residential land = change in commercial land if all workers paid same value added
dCommercial_land([1:N/2-1], 1) = (dVA_rat([1:30], 1).*co_obj.landmass([1:N/2-1]))./(dVA_rat([1:N/2-1], 1).*co_obj.commercial_land_2000(:, 1) + dVA_rat([1:N/2-1], 1).*co_obj.residential_land_2000(:, 1) + co_obj.commercial_land_2000(:, 2) + co_obj.residential_land_2000(:, 2)) ;
dResidential_land([1:N/2-1], 1) = dCommercial_land([1:N/2-1], 1) ; 
dRental_rate = (G./co_obj.nomY_balanced_2000)./dResidential_land ; %equilibrium change in rental rates

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
     Agshare(i, k) = fzero(fun, [0.001, .999], options) ; 
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

%Calculating the coefficient of variation of population density)
Density = (Lab([1:N/2-1], 1) + Lab([1:N/2-1], 2))./co_obj.landmass ;
Coeff_density(aprod, rprod, trc, mic, dRegisteredWorkers, dPreferenceParameter) = sqrt(((N-2)/2 - 1)/((N-2)/2))*std(Density)/mean(Density) ; %Inputting coefficient into table. Recorrecting for Basel's correction here. 

%effect on agricultural consumption shares
Ag_share_store(aprod, rprod, trc, mic, dRegisteredWorkers, dPreferenceParameter, :, :) = Agshare ;

%Repeating for employment weighted variation in population density
wmeanDensity = sum(((Lab([1:N/2-1], 1) + Lab([1:N/2-1], 2))/sum(Lab([1:N/2-1], :), 'all')).*(Lab([1:N/2-1], 1) + Lab([1:N/2-1], 2))./co_obj.landmass, 'all');
wstdDensity = sqrt(sum(((Lab([1:N/2-1], 1) + Lab([1:N/2-1], 2))/sum(Lab([1:N/2-1], :), 'all')).*((((Lab([1:N/2-1], 1) + Lab([1:N/2-1], 2))./co_obj.landmass) - wmeanDensity*ones(N/2-1, 1)).^2), 'all')) ;

wCoeff_density(aprod, rprod, trc, mic, dRegisteredWorkers, dPreferenceParameter) = wstdDensity/wmeanDensity ;

    end %ENDING ALL COUNTERFACTUAL EQUILIBRIA
    end
    end
    end
    end
    end
%%_________________________________________________________
end

%%
%Calculation of average marginal effects + saving
effect_prod = zeros(1, 4) ;

effect_prod(1) = mean(Coeff_density(2, :, :, :, :) - Coeff_density(1, :, :, :, :), 'all')/(Coeff_density(2, 2, 2, 2, 2) - Coeff_density(1, 1, 1, 1, 1)) ; 
effect_prod(2) = mean(Coeff_density(:, 2, :, :, :) - Coeff_density(:, 1, :, :, :), 'all')/(Coeff_density(2, 2, 2, 2, 2) - Coeff_density(1, 1, 1, 1, 1)) ;
effect_prod(3) = mean(Coeff_density(:, :, 2, :, :) - Coeff_density(:, :, 1, :, :), 'all')/(Coeff_density(2, 2, 2, 2, 2) - Coeff_density(1, 1, 1, 1, 1)) ; 
effect_prod(4) = mean(Coeff_density(:, :, :, 2, :) - Coeff_density(:, :, :, 1, :), 'all')/(Coeff_density(2, 2, 2, 2, 2) - Coeff_density(1, 1, 1, 1, 1)) ;

effect_prod = 100*effect_prod ; %percentage terms ;

xbar = categorical({'Uniform growth', 'Relative growth', 'Trade costs', 'Migration costs'}) ;
xbar = reordercats(xbar, {'Uniform growth', 'Relative growth', 'Trade costs', 'Migration costs'}) ;

b = bar(xbar, effect_prod) ;
ylabel('Contribution (Shapely Value), percent') ;

cd C:\Users\James\Dropbox\SchoolFolder\SYP\Writeups\Drafts
saveas(gcf, 'ShapelyDecomp.png') 

%Plotting bar graph

cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData\constructed_outputV2
save density_dispersion Coeff_density
save wdensity_dispersion wCoeff_density



