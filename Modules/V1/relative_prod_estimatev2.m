
%Date created: October 7th 2021
%Date edited: October 7th 2021

global parameters tradec tradef N 
global sa_matrix va_matrix tc_matrix_2005 tf_matrix_2005 nomGDP_matrix_2005 agsp_matrix_2005 nomGDP 
%This file estimates marginal costs using the market clearing condition,
%assuming exogenous (calibrated) deficits. Outputs the value of these deficits and 
%outputs value of (NORMALIZED) marginal costs to original
%data for use with other modules in the project.
%Also outputs (NORMALIZED, up to scale factor) price indicies.

%Importing data on GDP by sector and province. 
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData

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


cd C:\Users\James\Dropbox\SchoolFolder\SYP\modules

N=31*2; %Number of sector location pairs.

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


%% Part One: solve in changes from 2000-2005 and 2005-2010
%%Columns of cost vector == year, stacked [ag ; na] within years

cost = lognrnd(0, 1, N, 1) ; %Initial condition appears to not matter (generate random initial condition)
cnorm2005 = 1 ;
cnorm2010 = 1 ;
cost_new = ones(N, 2) ;

%Billateral spending matrix. Rows will be sourcer province/sectors, columns will
%be province/sectors sourced from. Column sum will be total payments to province
%(i) (stacked with sector k). This is the same matrix structure used in the
%main counterfactual. 

%Out of loop. dTrade cost and flow matrices, value added, production
%network and gross output

%Time invariant
sa_matrix = [repelem(parameters.phi_aa, N/2, N/2), repelem(parameters.phi_na, N/2, N/2) ; repelem(parameters.phi_an, N/2, N/2), repelem(parameters.phi_nn, N/2, N/2)] ; %matrix of production network parameters, time invariant
va_matrix = [repelem(parameters.vashare_a, N/2, N) ; repelem(parameters.vashare_n, N/2, N)] ; %matrix of value added shares
K_adjustment = 1/5 ;

%%__2005__%%
tc_matrix_2005 = [tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002; tradec.ag_2007./tradec.ag_2002, tradec.na_2007./tradec.na_2002].^(-1) ;
tf_matrix_2005 = [tradef.ag_2002, tradef.na_2002; tradef.ag_2002, tradef.na_2002] ;
agsp_matrix_2005 = [repelem(ag_share([1:N/2], 2), 1, N/2), ones(N/2, N/2) - repelem(ag_share([1:N/2], 2), 1, N/2) ; repelem(ag_share([(N/2+1):N], 2), 1, N/2), ones(N/2, N/2) - repelem(ag_share([(N/2+1):N], 2), 1, N/2) ] ; %matrix of updated ag spending shares
nomGDP_matrix_2005 = repelem(nomGDP(:, 2), 1, N)./repelem(nomGDP(:, 2)', N, 1) ; %matrix of sourcer GDP relative to sourced GDP

%______________In 2005 solution___________________________
%NOTE: there appear to be  computational issues probably stemming
%from the unconnected trade matrix (very close to 0 i.e. singularity)?
%Remember: the following algorithm can be shown to be ergodic
%mathematically.

while cnorm2005 > 0.0000000001
cost_matrix_2005 = (repelem(cost(:, 1)', N, 1)).^(-parameters.theta) ; 
%Column of price indices stacked [ag; na] in each location
p_index_2005 = [p_index_ag(cost([1:N/2], 1), 2005) ; p_index_na(cost([(N/2+1):N], 1), 2005)] ;
pi_matrix_2005 = [repelem(p_index_2005([1:N/2], 1), 1 , N/2), repelem(p_index_2005([(N/2+1):N], 1), 1 , N/2) ; repelem(p_index_2005([1:N/2], 1), 1 , N/2), repelem(p_index_2005([(N/2+1):N], 1), 1 , N/2)].^(-parameters.theta) ;
%matrix of denominators for trade shares (price index^{-theta})

%Trade share matrix (net of cost)
Tradeshare_mat_2005 = (tc_matrix_2005.*tf_matrix_2005)./pi_matrix_2005 ;

%TEST: sum of each tradeshare matrix in each sector to 1, so sum over both
%sectors should be 2.
test1 = sum(cost_matrix_2005.*Tradeshare_mat_2005, 2) ; %Sums to 2! Test passed

%Payments to sourced province/sectors
MarketClear_mat = (agsp_matrix_2005.*va_matrix.*nomGDP_matrix_2005 + sa_matrix.*nomGDP_matrix_2005).*Tradeshare_mat_2005;
MarketClear = (sum(MarketClear_mat, 1).^(1/parameters.theta))' ; 
%Also calcualting implied excess demand
ExcessDemand = (sum(cost_matrix_2005.*MarketClear_mat.*repelem(nomGDP(:, 2)', N, 1), 1)' - nomGDP(:, 2)) ; %Excess demand as a column vector

%TEST: Excess demand must sum to zero even out of equilibrium
test2 = sum(ExcessDemand) ; %very very close to 0 (to machine precision)

%Updating cost
cost_new([1:N/2], 1) = MarketClear([1:N/2], 1)/mean(MarketClear([1:N/2], 1)) ; 
cost_new([N/2+1:N], 1) = MarketClear([N/2+1:N], 1)/mean(MarketClear([N/2+1:N], 1)) ; 


%norms
cnorm2005_ag = norm(cost_new([1:N/2], 1) - cost([1:N/2], 1), 1) ;
cnorm2005_na =  norm(cost_new([N/2+1:N], 1) - cost([N/2+1:N], 1), 1) ; 
cnorm2005 = max(cnorm2005_ag, cnorm2005_na) 

%vector

Residual = cost_new(:, 1) - cost(:, 1) ; %very close to 0, but excess demand very far away as a % of GDP-- mostly in the ag sector. make note. Probably due to sparsity of trade flow matrix?

%updating 
cost(:, 1) = cost_new(:, 1) ; %updating cost 
end

%Updating cost 



%%
%Putting into data frame and stacked data frame to save for use with other projects. 

master_raw.dcost_na_2005 = cost((N/2 + 1):N, 1) ;
master_raw.dcost_na_2010 = cost((N/2 + 1):N, 2) ;
master_raw.dcost_ag_2005 = cost(1:N/2, 1)  ;
master_raw.dcost_ag_2010 = cost(1:N/2, 2)  ;

master_raw_stacked.dcost = NaN(N*3, 1) ;
master_raw_stacked.dcost((N/2+1):(N/2 + N), 1)  = [cost(1:N/2, 1) ; cost(1:N/2, 2)] ;
master_raw_stacked.dcost((2*N + 1):N*3, 1)  = [cost((N/2+1):N, 1) ; cost((N/2+1):N, 2)] ;

%Price index growth (to the power of the trade elasticity.)

master_raw.dpindex_na_2005 = (p_index_matrix(1:N/2, N/2 + 1, 1)).^(-1) ; 
master_raw.dpindex_ag_2005 = (p_index_matrix(1:N/2, 1, 1)).^(-1) ;
master_raw.dpindex_ag_2010 = (p_index_matrix(1:N/2, 1, 2)).^(-1) ;
master_raw.dpindex_na_2010 = (p_index_matrix(1:N/2, N/2 + 1, 2)).^(-1) ;

master_raw_stacked.dpindex_na = NaN(N*3, 1) ;
master_raw_stacked.dpindex_na((N/2+1):(N/2 + N), 1)  = [(p_index_matrix(1:N/2, N/2 + 1, 1)).^(-1) ; (p_index_matrix(1:N/2, N/2 + 1, 2)).^(-1)] ;
master_raw_stacked.dpindex_na((2*N + 1):N*3, 1)  = [(p_index_matrix(1:N/2, N/2 + 1, 1)).^(-1) ; (p_index_matrix(1:N/2, N/2 + 1, 2)).^(-1)] ;

master_raw_stacked.dpindex_ag =  NaN(N*3, 1) ;
master_raw_stacked.dpindex_ag((N/2+1):(N/2 + N), 1)  = [(p_index_matrix(1:N/2, 1, 1)).^(-1) ; (p_index_matrix(1:N/2, 1, 2)).^(-1)] ;
master_raw_stacked.dpindex_ag((2*N + 1):N*3, 1)  = [(p_index_matrix(1:N/2, 1, 1)).^(-1) ; (p_index_matrix(1:N/2, 1 , 2)).^(-1)] ;

cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData
writetable(master_raw, "constructed_output/master.xlsx")
writetable(master_raw_stacked, "constructed_output/master_stacked.xlsx")
cd C:\Users\James\Dropbox\SchoolFolder\SYP\modules


%% Part 3: Solving for productivity in levels in 2000 for preference estimation.
%%Absorb preference parameters (i.e. demand shifters by location-specific
%%good) into productivity term. Used only to estimate preference parameters
%%and nothing else.

cost = ones(N, 1) ;

%Constructing income matrix 
t_exp_matrix = zeros(N, N, 1);

for i = 1:N
    vashare_temp = vashare_a ;
    phi_as = phi_aa ;
    phi_ns = phi_na ;
    
    if i > N/2
        vashare_temp = vashare_n;
        phi_as = phi_an ;
        phi_ns = phi_nn ;
    
    end
    
temp1 = repelem(ag_share(i, 1)*vashare_temp*nomGDP(i, 1)* + phi_as*nomGDP(i, 1), N/2) ;
temp2 = repelem((1-ag_share(i, 1))*vashare_temp*nomGDP(i, 1) + phi_ns*nomGDP(i, 1), N/2) ;

t_exp_matrix(i, :, 1) =  [temp1, temp2] ;

end

%2) constructing numerator of trade shares over sourced GDP
tradeflow_num_matrix = zeros(N, N, 1) ;
for i = 1:N
    for j = 1:N
        
        if i <= N/2
            if j<= N/2
                tradeflow_num_matrix(i, j, 1) = (1/tau_ag_2002(i, j))/(nomGDP(j, 1)) ;
            
            end
            if j> N/2
                tradeflow_num_matrix(i, j, 1) = (1/tau_na_2002(i, j-N/2))/(nomGDP(j, 1)) ;
            
            end
        end
        
        if i > N/2
            if j<= N/2
                tradeflow_num_matrix(i, j, 1) = (1/tau_ag_2002(i-N/2, j))/(nomGDP(j, 1)) ;
                
            end
            if j> N/2 
                tradeflow_num_matrix(i, j, 1) = (1/tau_na_2002(i-N/2, j-N/2))/(nomGDP(j, 1)) ;
               
            end
        end
    end
end


%%%%
p_index_matrix = zeros(N, N, 1) ;
tempva = zeros(1, N/2) ;
tempvn = zeros(1, N/2) ;
total_flow_matrix = zeros(N, N, 2);

cnorm = 1 ;
cost_new = zeros(N, 2) ;

while cnorm > 0.001
%Constructing total trade flow matrix in `i' loop
for i=1:N
   %construct vector with price index info using productivity
    for j=1:N
       if i<= N/2
        if j<= N/2
               tempva(1, j) = (1/tau_ag_2002(i, j))/cost(j, 1) ;
               
        end
       
        if j> N/2
               tempvn(1, j-N/2) = (1/tau_na_2002(i, j-N/2))/cost(j, 1) ;
              
        end
       end
       
       if i > N/2
         if j<= N/2
               tempva(1, j) = (1/tau_ag_2002(i - N/2, j))/cost(j, 1) ;
               
         end
       
         if j> N/2
               tempvn(1, j-N/2) = (1/tau_na_2002(i - N/2, j-N/2))/cost(j, 1) ;
        
         end
       end
           
    end
    
    temp1 = repelem(sum(tempva), N/2) ;
    temp2 = repelem(sum(tempvn), N/2) ;
    p_index_matrix(i, :, 1) = [temp1, temp2] ;
 
end

total_flow_matrix(:, :, 1) = t_exp_matrix(:, :, 1).*tradeflow_num_matrix(:, :, 1)./p_index_matrix(:, :, 1) ;
cost_new(:, 1) = transpose(sum(total_flow_matrix(:, :, 1), 1)) ;
cost_new(1:N/2, 1) = cost_new(1:N/2, 1)/mean(cost_new(1:(N/2-1), 1)) ;  %%normalizing so mean in chinese provinces is 1 (for now)
cost_new((N/2 + 1):N, 1) = cost_new((N/2 + 1):N, 1)/mean(cost_new((N/2 + 1):(N-1), 1)); 
cnorm = max(norm(cost_new(1:N/2, 1) - cost(1:N/2, 1)), norm(cost_new((N/2 + 1):N, 1) - cost((N/2 + 1):N, 1))) ; %updating norm for while loop condition
cost(:, 1) = cost_new(: , 1);

%%%
end

%Writing data to master data

master_raw.cost_na_2000 = cost((N/2 + 1):N, 1) ;
master_raw.cost_ag_2000 = cost(1:N/2, 1)  ;

master_raw_stacked.cost = NaN(N*3, 1) ;
master_raw_stacked.cost(1:N/2, 1) = cost(1:N/2, 1) ;
master_raw_stacked.cost((N + N/2 + 1):(2*N), 1) = cost((N/2 + 1):N, 1) ;

master_raw.pindex_na_2000 = (p_index_matrix(1:N/2, N/2 + 1, 1)).^(-1) ; 
master_raw.pindex_ag_2000 = (p_index_matrix(1:N/2, N/2 + 1, 1)).^(-1) ;

master_raw_stacked.pindex_na = NaN(N*3, 1) ;
master_raw_stacked.pindex_na(1:N/2, 1) = (p_index_matrix(1:N/2, N/2 + 1, 1)).^(-1) ;
master_raw_stacked.pindex_na((N + N/2 + 1):(2*N), 1) =  (p_index_matrix(1:N/2, N/2 + 1, 1)).^(-1) ;

master_raw_stacked.pindex_ag = NaN(N*3, 1) ;
master_raw_stacked.pindex_ag(1:N/2, 1) = (p_index_matrix(1:N/2, 1, 1)).^(-1) ;
master_raw_stacked.pindex_ag((N + N/2 + 1):(2*N), 1) =  (p_index_matrix(1:N/2, 1, 1)).^(-1) ;


%Writing
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData
writetable(master_raw, "constructed_output/master.xlsx")
writetable(master_raw_stacked, "constructed_output/master_stacked.xlsx")
