
% Date created: July 9th, 2021
% Date edited: July 11th, 2021 

%Calculates the (unique) levels of GDP in each province sector to balance
%trade. Used in the counterfactual excercise. 

%Importing data on GDP by sector and province. 
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData

master_raw =  readtable('master_raw.xlsx') ;
master_raw_stacked = readtable('master_raw_stacked.xlsx') ;
%master_raw_stacked = readtable('master_raw_stacked.xlsx') ;
tau_ag_2002 = table2array(readtable('constructed_output/HRtau_ag_2002'));
tau_ag_2007 = table2array(readtable('constructed_output/HRtau_ag_2007'));
tau_ag_2012 = table2array(readtable('constructed_output/HRtau_ag_2012'));
tau_na_2002 = table2array(readtable('constructed_output/HRtau_na_2002'));
tau_na_2007 = table2array(readtable('constructed_output/HRtau_na_2007'));
tau_na_2012 = table2array(readtable('constructed_output/HRtau_na_2012'));

ag_tradeflows_2002 = readtable('2002_ag_tradeflows.xlsx');
ag_tradeflows_2002 = table2array(removevars(ag_tradeflows_2002, 'Var1'));
ag_tradeflows_2007 = readtable('2007_ag_tradeflows.xlsx');
ag_tradeflows_2007 = table2array(removevars(ag_tradeflows_2007, 'Var1'));
ag_tradeflows_2012 = readtable('2012_ag_tradeflows.xlsx');
ag_tradeflows_2012 = table2array(removevars(ag_tradeflows_2012, 'Var1'));
na_tradeflows_2002 = readtable('2002_na_tradeflows.xlsx');
na_tradeflows_2002 = table2array(removevars(na_tradeflows_2002, 'Var1'));
na_tradeflows_2007 = readtable('2007_na_tradeflows.xlsx');
na_tradeflows_2007 = table2array(removevars(na_tradeflows_2007, 'Var1'));
na_tradeflows_2012 = readtable('2012_na_tradeflows.xlsx');
na_tradeflows_2012 = table2array(removevars(na_tradeflows_2012, 'Var1'));


cd C:\Users\James\Dropbox\SchoolFolder\SYP\modules

N=31*2; %Number of sector location pairs. Will need to change this if 
        %disaggregating the data. This is for province level

%IO network from Hao et. al (2012)
phi_an = 0.04 ;
phi_aa = 0.16 ;
phi_na = 0.25 ;
phi_nn = 0.61 ;
vashare_n = 1 - phi_an - phi_nn ;
vashare_a = 1 - phi_aa - phi_na ;
%Constructing vectors of length 62 that give size of sector for each.
%Columns will be year, rows will be sector-location pairs. 3 columns means
%2000, 2005 and 2010, while 2 will mean 2005 and 2010 (where we perform hat
%algebra). 
%Trade flows will be an array, with the third dimension being the time
%period.

%Setting data in vector format
%gross output:
nomGDP = zeros(N, 3) ; 
nomGDP(:, 1) = [master_raw.nomY_ag_2000/vashare_a ; master_raw.nomY_na_2000/vashare_n] ;
nomGDP(:, 2) = [master_raw.nomY_ag_2005/vashare_a ; master_raw.nomY_na_2005/vashare_n] ;
nomGDP(:, 3) = [master_raw.nomY_ag_2010/vashare_a ; master_raw.nomY_na_2010/vashare_n] ;

%Employment (in labour)
L = zeros(N, 1) ;
L(:, 1) = [master_raw.L_ag_2000 ; master_raw.L_na_2000] ; %for normalization only
L(:, 2) = [master_raw.L_ag_2005 ; master_raw.L_na_2005] ;
L(:, 3) = [master_raw.L_ag_2010 ; master_raw.L_na_2010] ;

ag_share = zeros(N, 3) ; %
ag_share(:, 1) = [master_raw.agspend_ag_2000 ; master_raw.agspend_na_2000] ;
ag_share(:, 2) = [master_raw.agspend_ag_2005 ; master_raw.agspend_na_2005] ;
ag_share(:, 3) = [master_raw.agspend_ag_2010 ; master_raw.agspend_na_2010] ;

cost = ones(N, 2) ; %Initial value (doesn't matter for the algorithm outcome at all) 


%% What does nominal GDP look like if we imposed balanced trade?
%2005
VA = [repelem(vashare_a, N/2, N) ; repelem(vashare_n, N/2, N)]' ;
prod_network = [repelem(phi_aa, N/2, N/2), repelem(phi_an, N/2, N/2) ; repelem(phi_na, N/2, N/2) , repelem(phi_nn, N/2, N/2)] ;

Tradeshare_2005 = [ag_tradeflows_2007, na_tradeflows_2007 ; ag_tradeflows_2007, na_tradeflows_2007]' ; 
ag_share_2005 = [repelem(ag_share(:, 2)', N/2, 1) ; repelem(ones(N, 1)' - ag_share(:, 2)', N/2, 1)] ;
payment_matrix_2005 = (ag_share_2005.*VA + prod_network).*Tradeshare_2005 ;

[vector, value] = eig(payment_matrix_2005) ; %Extract Perron-Frobenius eigenvector

TrGDP_2005 = real(vector(:, 1)) ; % Sometimes eigenvector stored as complex even though no imaginary component
if vector(1, 1) < 0
TrGDP_2005 = real(-vector(:, 1)) ; %Choose eigenvector to have strictly positive components. 
end

%Renormalizing solution
TrGDP_2005 = mean([nomGDP([1:30], 2) ;nomGDP([32:61], 2)], 'all')*TrGDP_2005/(mean([TrGDP_2005([1:30], 1) ; TrGDP_2005([32:61], 1)], 'all')) ;
scatter(TrGDP_2005, nomGDP(:, 2)) ;

%2010
Tradeshare_2010 = [ag_tradeflows_2012, na_tradeflows_2012 ; ag_tradeflows_2012, na_tradeflows_2012]' ; 
ag_share_2010 = [repelem(ag_share(:, 3)', N/2, 1) ; repelem(ones(N, 1)' - ag_share(:, 3)', N/2, 1)] ;
payment_matrix_2010 = (ag_share_2010.*VA + prod_network).*Tradeshare_2010 ;

[vector, value] = eig(payment_matrix_2010) ; 

TrGDP_2010 = real(vector(:, 1)) ;
if vector(1, 1) < 0
TrGDP_2010 = real(-vector(:, 1)) ; 
end

TrGDP_2010 = mean([nomGDP([1:30], 3) ; nomGDP([32:61], 3)], 'all')*TrGDP_2010/(mean([TrGDP_2010([1:30], 1) ; TrGDP_2010([32:61], 1)], 'all')) ;
scatter(TrGDP_2010, nomGDP(:, 3)) ;

%2000
Tradeshare_2000 = [ag_tradeflows_2002, na_tradeflows_2002 ; ag_tradeflows_2002, na_tradeflows_2002]' ; 
ag_share_2000 = [repelem(ag_share(:, 1)', N/2, 1) ; repelem(ones(N, 1)' - ag_share(:, 1)', N/2, 1)] ;
payment_matrix_2000 = (ag_share_2000.*VA + prod_network).*Tradeshare_2000 ;

[vector, value] = eig(payment_matrix_2000) ;

TrGDP_2000 = real(vector(:, 1)) ;
if vector(1, 1) < 0
TrGDP_2000 = real(-vector(:, 1)) ; 
end

TrGDP_2000 = mean([nomGDP([1:30], 1) ; nomGDP([32:61], 1)], 'all')*TrGDP_2000/(mean([TrGDP_2000([1:30], 1) ; TrGDP_2000([32:61], 1)], 'all')) ;
scatter(TrGDP_2000, nomGDP(:, 1)) ;


%%Normalizing so that value added per worker in China is on average 1 in all years
TrGDP_2000 = TrGDP_2000/(mean([vashare_a*TrGDP_2000([1:30], 1)./L([1:30], 1) , vashare_n*TrGDP_2000([32:61], 1)./L([32:61], 1)], 'all')) ;
TrGDP_2005 = TrGDP_2005/(mean([vashare_a*TrGDP_2005([1:30], 1)./L([1:30], 2) , vashare_n*TrGDP_2005([32:61], 1)./L([32:61], 2)], 'all')) ;
TrGDP_2010 = TrGDP_2010/(mean([vashare_a*TrGDP_2010([1:30], 1)./L([1:30], 3) , vashare_n*TrGDP_2010([32:61], 1)./L([32:61], 3)], 'all')) ;

cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData\constructed_output
stacked_GDP = [TrGDP_2000([1:31], 1) ; TrGDP_2005([1:31], 1) ;  TrGDP_2010([1:31], 1) ; TrGDP_2000([32:62], 1) ; TrGDP_2005([32:62], 1) ;  TrGDP_2010([32:62], 1)] ;
Output = table(master_raw_stacked.province, master_raw_stacked.sector, master_raw_stacked.year, stacked_GDP) ;
Output.Properties.VariableNames = {'province', 'sector', 'year', 'nomY'} ;

writetable(Output, "Calibrated_GDP_balanced.xlsx") 