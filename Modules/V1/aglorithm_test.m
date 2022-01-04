%Cost solution algorithm testing.


%Importing data on GDP by sector and province. 
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData

parameters = struct() ;
tradec = struct() ;
tradef = struct() ;

parameters = struct() ;
tradec = struct() ;
tradef = struct() ;

master_raw =  readtable('master_raw.xlsx') ;
master_raw_stacked = readtable('master_raw_stacked.xlsx') ;
tradec.ag_2002 = table2array(readtable('constructed_output/HRtau_ag_2002'));
tradec.ag_2007 = table2array(readtable('constructed_output/HRtau_ag_2007'));
tradec.ag_2012 = table2array(readtable('constructed_output/HRtau_ag_2012'));
tradec.na_2002 = table2array(readtable('constructed_output/HRtau_na_2002'));
tradec.na_2007 = table2array(readtable('constructed_output/HRtau_na_2007'));
tradec.na_2012 = table2array(readtable('constructed_output/HRtau_na_2012'));

tradef.ag_2002 = readtable('2002_ag_tradeflows.xlsx');
tradef.ag_2002 = table2array(removevars(tradef.ag_2002, 'Var1'));
tradef.ag_2007 = readtable('2007_ag_tradeflows.xlsx');
tradef.ag_2007 = table2array(removevars(tradef.ag_2007, 'Var1'));
tradef.ag_2012 = readtable('2012_ag_tradeflows.xlsx');
tradef.ag_2012 = table2array(removevars(tradef.ag_2012, 'Var1'));
tradef.na_2002 = readtable('2002_na_tradeflows.xlsx');
tradef.na_2002 = table2array(removevars(tradef.na_2002, 'Var1'));
tradef.na_2007 = readtable('2007_na_tradeflows.xlsx');
tradef.na_2007 = table2array(removevars(tradef.na_2007, 'Var1'));
tradef.na_2012 = readtable('2012_na_tradeflows.xlsx');
tradef.na_2012 = table2array(removevars(tradef.na_2012, 'Var1'));


N=31*2; %Number of sector location pairs.

cd C:\Users\James\Dropbox\SchoolFolder\SYP\modules

%Trade elasticity: Simonovska and Waugh
parameters.theta = 4 ;
        
%IO network from Hao et. al (2012)
parameters.phi_an = 0.04 ;
parameters.phi_aa = 0.16 ;
parameters.phi_na = 0.25 ;
parameters.phi_nn = 0.61 ;
parameters.vashare_n = 1 - parameters.phi_an - parameters.phi_nn ;
parameters.vashare_a = 1 - parameters.phi_aa - parameters.phi_na ;
%Constructing vectors of length 62 that give size of sector for each.
%Columns will be year, rows will be sector-location pairs. 3 columns means
%2000, 2005 and 2010, while 2 will mean 2005 and 2010 (where we perform hat
%algebra). 
%Trade flows will be an array, with the third dimension being the time
%period.

%Setting data in vector format
%gross output:
nomGDP = zeros(N, 3) ; 
nomGDP(:, 1) = [master_raw.nomY_ag_2000/parameters.vashare_a ; master_raw.nomY_na_2000/parameters.vashare_n] ;
nomGDP(:, 2) = [master_raw.nomY_ag_2005/parameters.vashare_a ; master_raw.nomY_na_2005/parameters.vashare_n] ;
nomGDP(:, 3) = [master_raw.nomY_ag_2010/parameters.vashare_a ; master_raw.nomY_na_2010/parameters.vashare_n] ;

%Employment (in labour)
L = zeros(N, 1) ;
L(:, 1) = [master_raw.L_ag_2000 ; master_raw.L_na_2000] ; %for normalization only
L(:, 2) = [master_raw.L_ag_2005 ; master_raw.L_na_2005] ;
L(:, 3) = [master_raw.L_ag_2010 ; master_raw.L_na_2010] ;

ag_share = zeros(N, 3) ; %
ag_share(:, 1) = [master_raw.agspend_ag_2000 ; master_raw.agspend_na_2000] ;
ag_share(:, 2) = [master_raw.agspend_ag_2005 ; master_raw.agspend_na_2005] ;
ag_share(:, 3) = [master_raw.agspend_ag_2010 ; master_raw.agspend_na_2010] ;


%%
cost = lognrnd(0, 1, N, 1) ;
cost = cost/sum(cost, 1) ; %normalizing to unit simplex. keep shuffling until you find initial condition that gets you the interior solution. Note: this algorithm is NOT ALWAYS ERGODIC even with gross substitutes. This is NOT an excess demand function!
cost_init = cost; %Initial condition appears to not matter (generate random initial condition)
cost_new = ones(N, 1) ;
cnorm2005 = 1 ;
i=1 ;
%Billateral spending matrix. Rows will be sourcer province/sectors, columns will
%be province/sectors sourced from. Column sum will be total payments to province
%(i) (stacked with sector k). This is the same matrix structure used in the
%main counterfactual. 

%Out of loop. dTrade cost and flow matrices, value added, production
%network and gross output

%eta parameter >1 make this arbitrarily close to 1 to approximate true
%equilibrium at observed spending shares. Normalize all average
%productivities within sectors to 1 to get at an equilibrium that makes
%sense when eta = 1 -- then check how close it is to minimizing excess
%demand in the "true" demand system. 
parameters.eta = 1.5 ; %eta = 1.5 appears to have the best performance for this problem! Take note. 

%Time invariant
sa_matrix = [repelem(parameters.phi_aa, N/2, N/2), repelem(parameters.phi_na, N/2, N/2) ; repelem(parameters.phi_an, N/2, N/2), repelem(parameters.phi_nn, N/2, N/2)] ; %matrix of production network parameters, time invariant
va_matrix = [repelem(parameters.vashare_a, N/2, N) ; repelem(parameters.vashare_n, N/2, N)] ; %matrix of value added shares
k_adjustment = 1/50 ;

%%__2005__%%
tc_matrix_2005 = [tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002; tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002].^(-1) ;
tf_matrix_2005 = [tradef.ag_2002, tradef.na_2002; tradef.ag_2002, tradef.na_2002] ;
nomGDP_matrix_2005 = repelem(nomGDP(:, 2), 1, N) ; %matrix of sourcer GDP relative to sourced GDP


while cnorm2005 > 0.000000001 %&& i < 1000 %Stop if taking longer than 1000 iterations
cost_matrix_2005 = (repelem(cost(:, 1)', N, 1)).^(-parameters.theta) ; 
%Column of price indices stacked [ag; na] in each location
p_index_2005 = [((tf_matrix_2005([1:N/2], [1:N/2]).*((tc_matrix_2005([1:N/2], [1:N/2]))))*(cost([1:N/2], 1).^(-parameters.theta))).^(-(1/parameters.theta)) ; ((tf_matrix_2005([1:N/2], [(N/2+1):N]).*((tc_matrix_2005([1:N/2], [(N/2+1):N]))))*(cost([(N/2+1):N], 1).^(-parameters.theta))).^(-(1/parameters.theta))] ;

pi_matrix_2005 = [repelem(p_index_2005([1:N/2], 1), 1 , N/2), repelem(p_index_2005([(N/2+1):N], 1), 1 , N/2) ; repelem(p_index_2005([1:N/2], 1), 1 , N/2), repelem(p_index_2005([(N/2+1):N], 1), 1 , N/2)].^(-parameters.theta) ;
%matrix of denominators for trade shares (price index^{-theta})

%Constructing new ag spending share matrix given price index data
ag_share_denom_ag = ag_share([1:N/2], 1).*(p_index_2005([1:N/2], 1).^(1-parameters.eta)) + (ones(N/2, 1) -  ag_share([1:N/2], 1)).*(p_index_2005([N/2+1:N], 1).^(1-parameters.eta)) ;
ag_share_denom_na = ag_share([N/2+1:N], 1).*(p_index_2005([1:N/2], 1).^(1-parameters.eta)) + (ones(N/2, 1) -  ag_share([N/2+1:N], 1)).*(p_index_2005([N/2+1:N], 1).^(1-parameters.eta)) ;
ag_share_new = [(ag_share([1:N/2], 2).*(p_index_2005([1:N/2], 1).^(1-parameters.eta)))./ag_share_denom_ag; ag_share([N/2+1:N], 2).*(p_index_2005([1:N/2], 1).^(1-parameters.eta))./ag_share_denom_na] ;
agsp_matrix_2005 = [repelem(ag_share_new([1:N/2], 1), 1, N/2), ones(N/2, N/2) - repelem(ag_share_new([1:N/2], 1), 1, N/2) ; repelem(ag_share_new([(N/2+1):N], 1), 1, N/2), ones(N/2, N/2) - repelem(ag_share_new([(N/2+1):N], 1), 1, N/2) ] ; %matrix of updated ag spending shares 
test0 = agsp_matrix_2005(:, 1) + agsp_matrix_2005(:, N/2+1) ;

%Trade share matrix (net of cost)
Tradeshare_mat_2005 = (cost_matrix_2005.*tc_matrix_2005.*tf_matrix_2005)./pi_matrix_2005 ;

%TEST: sum of each tradeshare matrix in each sector to 1 
test1 = sum(Tradeshare_mat_2005, 2) ; %Sums to 2! Test passed

%Payments to sourced province/sectors
MarketClear_mat = (agsp_matrix_2005.*va_matrix.*nomGDP_matrix_2005 + sa_matrix.*nomGDP_matrix_2005).*Tradeshare_mat_2005;
MarketClear = sum(MarketClear_mat, 1)' ; 

%Also calcualting implied excess demand
ExcessDemand = (MarketClear - nomGDP(:, 2)) ; %Excess demand as a column vector
ExcessDemand_max = max(ExcessDemand, zeros(N, 1)); %Adjusting all costs with excess demand

%TEST: Excess demand must sum to zero even out of equilibrium

test2 = sum(ExcessDemand) ; %very very close to 0 (to machine precision)


cost_new = (cost + k_adjustment*ExcessDemand_max)/(sum(cost + k_adjustment*ExcessDemand_max, 1)) ; %Renormalizing to unit simplex

%norms
cnorm2005 = norm(cost_new - cost)  

%vector

Residual = cost_new(:, 1) - cost(:, 1) ; 

%updating 
cost(:, 1) = cost_new(:, 1) ; %updating cost 
i=i+1 
end