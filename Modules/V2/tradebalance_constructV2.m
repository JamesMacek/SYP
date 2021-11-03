
%Date created: October 15th, 2021
%Date edited: October 15th, 2021

%Calculates the (unique) levels of GDP in each province sector to balance
%trade. Used in the counterfactual excercise. 

%Importing data on GDP by sector and province. 
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData

global N parameters co_obj tradec tradef

master_raw =  readtable('master_raw.xlsx') ; %Only need raw data for the exercise
load constructed_outputV2/tradecosts_struct ;
load constructed_outputV2/tradeflows_struct ;
load constructed_outputV2/parameters

N=31*2 ;

%% Setting up parameters
%Gross output
nomGDP = zeros(N, 3) ; 
nomGDP(:, 1) = [master_raw.nomY_ag_2000/parameters.vashare_a ; master_raw.nomY_na_2000/parameters.vashare_n] ;
nomGDP(:, 2) = [master_raw.nomY_ag_2005/parameters.vashare_a ; master_raw.nomY_na_2005/parameters.vashare_n] ;
nomGDP(:, 3) = [master_raw.nomY_ag_2010/parameters.vashare_a ; master_raw.nomY_na_2010/parameters.vashare_n] ;


%Employment (in labour)
L = zeros(N, 1) ;
L(:, 1) = [master_raw.L_ag_2000 ; master_raw.L_na_2000] ; %for normalization only
L(:, 2) = [master_raw.L_ag_2005 ; master_raw.L_na_2005] ;
L(:, 3) = [master_raw.L_ag_2010 ; master_raw.L_na_2010] ;

%Ag spending shares 
ag_share = zeros(N, 3) ; %
ag_share(:, 1) = [master_raw.agspend_ag_2000 ; master_raw.agspend_na_2000] ;
ag_share(:, 2) = [master_raw.agspend_ag_2005 ; master_raw.agspend_na_2005] ;
ag_share(:, 3) = [master_raw.agspend_ag_2010 ; master_raw.agspend_na_2010] ;

%% What does nominal GDP look like if we imposed balanced trade?
%2005
VA_share = [repelem(parameters.vashare_a, N/2, N) ; repelem(parameters.vashare_n, N/2, N)]' ;
prod_network = [repelem(parameters.phi_aa, N/2, N/2), repelem(parameters.phi_an, N/2, N/2) ; repelem(parameters.phi_na, N/2, N/2) , repelem(parameters.phi_nn, N/2, N/2)] ;
Tradeshare_2005 = [tradef.ag_2007, tradef.na_2007 ; tradef.ag_2007, tradef.na_2007]' ; %transposed trade share from data
ag_share_2005 = [repelem(ag_share(:, 2)', N/2, 1) ; repelem(ones(N, 1)' - ag_share(:, 2)', N/2, 1)] ;
payment_matrix_2005 = (ag_share_2005.*VA_share + prod_network).*Tradeshare_2005 ;

%Eigenvector of payment matrix is the GDP that balances trade (recall!)  
[vector, value] = eig(payment_matrix_2005) ; %Extract Perron-Frobenius eigenvector

TrGDP_2005 = real(vector(:, 1)) ; %Sometimes stored computationally as complex with 0 imaginary component
if vector(1, 1) < 0
TrGDP_2005 = real(-vector(:, 1)) ; %Choosing to have strictly positive components. 
end

TrGDP_2005 = mean([nomGDP([1:30], 2) ; nomGDP([32:61], 2)], 'all')*TrGDP_2005/(mean([TrGDP_2005([1:30], 1) ; TrGDP_2005([32:61], 1)], 'all')) ;
scatter(TrGDP_2005, nomGDP(:, 2)) ; %Good correlation!

%2000
Tradeshare_2000 = [tradef.ag_2002, tradef.na_2002 ; tradef.ag_2002, tradef.na_2002]' ; 
ag_share_2000 = [repelem(ag_share(:, 1)', N/2, 1) ; repelem(ones(N, 1)' - ag_share(:, 1)', N/2, 1)] ;
payment_matrix_2000 = (ag_share_2000.*VA_share + prod_network).*Tradeshare_2000 ;

[vector, value] = eig(payment_matrix_2000) ;

TrGDP_2000 = real(vector(:, 1)) ;
if vector(1, 1) < 0
TrGDP_2000 = real(-vector(:, 1)) ; 
end

TrGDP_2000 = mean([nomGDP([1:30], 1) ; nomGDP([32:61], 1)], 'all')*TrGDP_2000/(mean([TrGDP_2000([1:30], 1) ; TrGDP_2000([32:61], 1)], 'all')) ;
scatter(TrGDP_2000, nomGDP(:, 1)) ;


%%Normalizing so that geometric mean of value added per worker in China is on average 1 in all years
TrGDP_2000 = TrGDP_2000/(geomean([parameters.vashare_a*TrGDP_2000([1:30], 1)./L([1:30], 1) , parameters.vashare_n*TrGDP_2000([32:61], 1)./L([32:61], 1)], 'all')) ;
TrGDP_2005 = TrGDP_2005/(geomean([parameters.vashare_a*TrGDP_2005([1:30], 1)./L([1:30], 2) , parameters.vashare_n*TrGDP_2005([32:61], 1)./L([32:61], 2)], 'all')) ;

%Testing: does the geometric average of VA/worker = 1 over chinese
%provinces?
test = geomean([parameters.vashare_a*TrGDP_2000([1:30], 1)./L([1:30], 1) , parameters.vashare_n*TrGDP_2000([32:61], 1)./L([32:61], 1)], 'all') ;

cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData\constructed_outputV2
Output_ag =  TrGDP_2000([1:31], 1) ;
Output_na = TrGDP_2000([32:62], 1) ;
Output = table(master_raw.province, Output_ag, Output_na) ;
Output.Properties.VariableNames = {'province', 'nomY_ag' 'nomY_na'} ;

%OUTPUT IS IN GROSS TERMS.
writetable(Output, "Calibrated_GDP_balanced_2000.xlsx") 
