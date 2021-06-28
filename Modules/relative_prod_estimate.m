
%Date created: May 11th 2021
%Date edited: June 25th 2021

%This file estimates marginal costs using the market clearing condition,
%assuming exogenous (calibrated) deficits. Outputs the value of these deficits and 
%outputs value of inverse (NORMALIZED) marginal costs (to the power of the trade elasticity) to original
%data for use with other modules in the project.
%Also outputs (NORMALIZED) price indicies to the power of the trade elasticity 
%Marginal costs are normalized so that chinese provinces have a geometric
%mean of one.

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


%% Part Two: solve in changes from 2000-2005 and 2005-2010

%1) %Matrix of total expenditures on sector s goods by (j, k). Rows will be sourcers,
%columns will be where goods are sourced from
t_exp_matrix = zeros(N, N, 2);

for i = 1:N
    vashare_temp = vashare_a ;
    phi_as = phi_aa ;
    phi_ns = phi_na ;
    
    if i > N/2
        vashare_temp = vashare_n;
        phi_as = phi_an ;
        phi_ns = phi_nn ;
    
    end
    
temp1 = repelem(ag_share(i, 2)*vashare_temp*nomGDP(i, 2)* + phi_as*nomGDP(i, 2), N/2) ;
temp2 = repelem((1-ag_share(i, 2))*vashare_temp*nomGDP(i, 2) + phi_ns*nomGDP(i, 2), N/2) ;

t_exp_matrix(i, :, 1) =  [temp1, temp2] ;

temp1 = repelem(ag_share(i, 3)*vashare_temp*nomGDP(i, 3)* + phi_as*nomGDP(i, 3), N/2) ;
temp2 = repelem((1-ag_share(i, 3))*vashare_temp*nomGDP(i, 3) + phi_ns*nomGDP(i, 3), N/2) ;

t_exp_matrix(i, :, 2) = [temp1, temp2] ;
end

%2) constructing numerator of trade shares over sourced GDP
tradeflow_num_matrix = zeros(N, N, 2) ;
for i = 1:N
    for j = 1:N
        
        if i <= N/2
            if j<= N/2
                tradeflow_num_matrix(i, j, 1) = ag_tradeflows_2002(i, j)*(tau_ag_2002(i, j)/tau_ag_2007(i, j))/(nomGDP(j, 2)) ;
                tradeflow_num_matrix(i, j, 2) = ag_tradeflows_2007(i, j)*(tau_ag_2007(i, j)/tau_ag_2012(i, j))/(nomGDP(j, 3)) ;
            end
            if j> N/2
                tradeflow_num_matrix(i, j, 1) = na_tradeflows_2002(i, j-N/2)*(tau_na_2002(i, j-N/2)/tau_na_2007(i, j-N/2))/(nomGDP(j, 2)) ;
                tradeflow_num_matrix(i, j, 2) = na_tradeflows_2007(i, j-N/2)*(tau_na_2007(i, j-N/2)/tau_na_2012(i, j-N/2))/(nomGDP(j, 3)) ;
            end
        end
        
        if i > N/2
            if j<= N/2
                tradeflow_num_matrix(i, j, 1) = ag_tradeflows_2002(i-N/2, j)*(tau_ag_2002(i-N/2, j)/tau_ag_2007(i-N/2, j))/(nomGDP(j, 2)) ;
                tradeflow_num_matrix(i, j, 2) = ag_tradeflows_2007(i-N/2, j)*(tau_ag_2007(i-N/2, j)/tau_ag_2012(i-N/2, j))/(nomGDP(j, 3)) ;
            end
            if j> N/2 
                tradeflow_num_matrix(i, j, 1) = na_tradeflows_2002(i-N/2, j-N/2)*(tau_na_2002(i-N/2, j-N/2)/tau_na_2007(i-N/2, j-N/2))/(nomGDP(j, 2)) ;
                tradeflow_num_matrix(i, j, 2) = na_tradeflows_2007(i-N/2, j-N/2)*(tau_na_2007(i-N/2, j-N/2)/tau_na_2012(i-N/2, j-N/2))/(nomGDP(j, 3)) ;
            end
         end
    end
end


%%Function convergence stuff goes here
p_index_matrix = zeros(N, N, 2) ;
tempva2005 = zeros(1, N/2) ;
tempvn2005 = zeros(1, N/2) ;
tempva2010 = zeros(1, N/2) ;
tempvn2010 = zeros(1, N/2) ;
total_flow_matrix = zeros(N, N, 2);

cnorm2005 = 1 ;
cnorm2010 = 1 ;
cost_new = zeros(N, 2) ;
while cnorm2005 > 0.001 || cnorm2010 > 0.001

for i=1:N
   %construct vector with price index info using productivity
    for j=1:N
       if i<= N/2
        if j<= N/2
               tempva2005(1, j) = ag_tradeflows_2002(i, j)*(tau_ag_2002(i, j)/tau_ag_2007(i, j))/cost(j, 1) ;
               tempva2010(1, j) = ag_tradeflows_2007(i, j)*(tau_ag_2007(i, j)/tau_ag_2012(i, j))/cost(j, 2) ;
        end
       
        if j> N/2
               tempvn2005(1, j-N/2) = na_tradeflows_2002(i, j-N/2)*(tau_na_2002(i, j-N/2)/tau_na_2007(i, j-N/2))/cost(j, 1) ;
               tempvn2010(1, j-N/2) = na_tradeflows_2007(i, j-N/2)*(tau_na_2007(i, j-N/2)/tau_na_2012(i, j-N/2))/cost(j, 2) ;
        end
       end
       
       if i > N/2
         if j<= N/2
               tempva2005(1, j) = ag_tradeflows_2002(i - N/2, j)*(tau_ag_2002(i - N/2, j)/tau_ag_2007(i - N/2, j))/cost(j, 1) ;
               tempva2010(1, j) = ag_tradeflows_2007(i - N/2, j)*(tau_ag_2007(i - N/2, j)/tau_ag_2012(i - N/2, j))/cost(j, 2) ;
         end
       
         if j> N/2
               tempvn2005(1, j-N/2) = na_tradeflows_2002(i - N/2, j-N/2)*(tau_na_2002(i - N/2, j-N/2)/tau_na_2007(i - N/2, j-N/2))/cost(j, 1) ;
               tempvn2010(1, j-N/2) = na_tradeflows_2007(i - N/2, j-N/2)*(tau_na_2007(i - N/2, j-N/2)/tau_na_2012(i - N/2, j-N/2))/cost(j, 2) ;
         end
       end
           
    end
    
    temp1 = repelem(sum(tempva2005), N/2) ;
    temp2 = repelem(sum(tempvn2005), N/2) ;
    p_index_matrix(i, :, 1) = [temp1, temp2] ;
    
    temp1 = repelem(sum(tempva2010), N/2) ;
    temp2 = repelem(sum(tempvn2010), N/2) ;
    p_index_matrix(i, :, 2) = [temp1, temp2] ;
end

total_flow_matrix(:, :, 1) = t_exp_matrix(:, :, 1).*tradeflow_num_matrix(:, :, 1)./p_index_matrix(:, :, 1) ;
cost_new(:, 1) = transpose(sum(total_flow_matrix(:, :, 1), 1)) ;
cost_new(1:N/2, 1) = cost_new(1:N/2, 1)/mean(cost_new(1:(N/2-1), 1)) ;  %normalizing so average cost in China is 1 (for now)
cost_new((N/2 + 1):N, 1) = cost_new((N/2 + 1):N, 1)/mean(cost_new((N/2 + 1):(N-1), 1));
cnorm2005 = max(norm(cost_new(1:N/2, 1) - cost(1:N/2, 1)), norm(cost_new((N/2 + 1):N, 1) - cost((N/2 + 1):N, 1))) ; 
cost(:, 1) = cost_new(: , 1); %Updating

total_flow_matrix(:, :, 2) = t_exp_matrix(:, :, 2).*tradeflow_num_matrix(:, :, 2)./p_index_matrix(:, :, 2) ;
cost_new(:, 2) = transpose(sum(total_flow_matrix(:, :, 2), 1)) ; %new value%
cost_new(1:N/2, 2) = cost_new(1:N/2, 2)/mean(cost_new(1:(N/2-1), 2)) ; 
cost_new((N/2 + 1):N, 2) = cost_new((N/2 + 1):N, 2)/mean(cost_new((N/2 + 1):(N-1), 2));
cnorm2010 = max(norm(cost_new(1:N/2, 2) - cost(1:N/2, 2)), norm(cost_new((N/2 + 1):N, 2) - cost((N/2 + 1):N, 2))) ;
cost(:, 2) = cost_new(: , 2); %Updating

end

%RENORMALIZATION SO THAT COSTS ARE CONSISTENT WITH CHOICE OF NUMERAIRE



%Putting into data frame and stacked data frame to save 

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


%% Part 3: Solving for productivity in levels in 2000 for estimation. 
%%Normalization: spatial geometric mean of average cost will be 1
%%Absorb preference parameters (i.e. demand shifters by location-specific
%%good) into productivity term.
%%Assume it's time invariant. 


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
master_raw.pindex_ag_2000 = (p_index_matrix(1:N/2, 1, 1)).^(-1) ;

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
