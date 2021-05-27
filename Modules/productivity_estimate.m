%Date created: May 11th 2021
%Date edited: May 24th 2021

%This file estimates marginal costs using the market clearing condition.
%Outputs value of inverse marginal costs (to the power of the trade elasticity) to original
%data for use with other programs. 

%Importing data on GDP by sector and province. 
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData

master_raw =  readtable('master_raw.xlsx') ;
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
na_tradeflows_2002 = readtable('2002_na_tradeflows.xlsx');
na_tradeflows_2002 = table2array(removevars(na_tradeflows_2002, 'Var1'));
na_tradeflows_2007 = readtable('2007_na_tradeflows.xlsx');
na_tradeflows_2007 = table2array(removevars(na_tradeflows_2007, 'Var1'));


N=31*2; %Number of sector location pairs. 


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

nomGDP = zeros(N, 3) ; 
nomGDP(:, 1) = [master_raw.Ag_nomY_2000 ; master_raw.Na_nomY_2000] ;
nomGDP(:, 2) = [master_raw.Ag_nomY_2005 ; master_raw.Na_nomY_2005] ;
nomGDP(:, 3) = [master_raw.Ag_nomY_2010 ; master_raw.Na_nomY_2010] ;

ag_share = zeros(N, 3) ; %
ag_share(:, 1) = [master_raw.Rural_spend_2000 ; master_raw.Urban_spend_2000] ;
ag_share(:, 2) = [master_raw.Rural_spend_2005 ; master_raw.Urban_spend_2005] ;
ag_share(:, 3) = [master_raw.Rural_spend_2010 ; master_raw.Urban_spend_2010] ;

cost = (1/N)*ones(N, 2) ;

%Normalize so that the sum of all growth factors is one (i.e. "cost" will have a
%1-norm of 1)


%Generic-- do for 2000 first. 
%Constructing N sized square matrix that gives trade flows to
%workers/firms in (i,s) from workers, firms in (j, k)
 

%1) %Matrix of total expenditures on sector s goods by (j, k). Rows will be sourcers,
%columns will be the sourced.
t_exp_matrix = zeros(N, N, 2);

for i = 1:N
    vashare_temp = vashare_a ;
    phi_as = phi_aa ;
    phi_ns = phi_na ;
    
    if i > 31
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

%2) constructing numerator of trade shares (net of productivity!)
tradeflow_num_matrix = zeros(N, N, 2) ;
for i = 1:N
    for j = 1:N
        
        if i <= 31
            if j<=31
                tradeflow_num_matrix(i, j, 1) = ag_tradeflows_2002(i, j)*(tau_ag_2002(i, j)/tau_ag_2007(i, j))/(nomGDP(j, 2)) ;
                tradeflow_num_matrix(i, j, 2) = ag_tradeflows_2007(i, j)*(tau_ag_2007(i, j)/tau_ag_2012(i, j))/(nomGDP(j, 3)) ;
            end
            if j>31 
                tradeflow_num_matrix(i, j, 1) = na_tradeflows_2002(i, j-31)*(tau_na_2002(i, j-31)/tau_na_2007(i, j-31))/(nomGDP(j, 2)) ;
                tradeflow_num_matrix(i, j, 2) = na_tradeflows_2007(i, j-31)*(tau_na_2007(i, j-31)/tau_na_2012(i, j-31))/(nomGDP(j, 3)) ;
            end
        end
        
        if i>31
            if j<=31
                tradeflow_num_matrix(i, j, 1) = ag_tradeflows_2002(i-31, j)*(tau_ag_2002(i-31, j)/tau_ag_2007(i-31, j))/(nomGDP(j, 2)) ;
                tradeflow_num_matrix(i, j, 2) = ag_tradeflows_2007(i-31, j)*(tau_ag_2007(i-31, j)/tau_ag_2012(i-31, j))/(nomGDP(j, 3)) ;
            end
            if j>31 
                tradeflow_num_matrix(i, j, 1) = na_tradeflows_2002(i-31, j-31)*(tau_na_2002(i-31, j-31)/tau_na_2007(i-31, j-31))/(nomGDP(j, 2)) ;
                tradeflow_num_matrix(i, j, 2) = na_tradeflows_2007(i-31, j-31)*(tau_na_2007(i-31, j-31)/tau_na_2012(i-31, j-31))/(nomGDP(j, 3)) ;
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
       if i<=31
        if j<=31
               tempva2005(1, j) = ag_tradeflows_2002(i, j)*(tau_ag_2002(i, j)/tau_ag_2007(i, j))/cost(j, 1) ;
               tempva2010(1, j) = ag_tradeflows_2007(i, j)*(tau_ag_2007(i, j)/tau_ag_2012(i, j))/cost(j, 2) ;
        end
       
        if j>31
               tempvn2005(1, j-31) = na_tradeflows_2002(i, j-31)*(tau_na_2002(i, j-31)/tau_na_2007(i, j-31))/cost(j, 1) ;
               tempvn2010(1, j-31) = na_tradeflows_2007(i, j-31)*(tau_na_2007(i, j-31)/tau_na_2012(i, j-31))/cost(j, 2) ;
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
cost_new(1:31, 1) = cost_new(1:31, 1)/norm(cost_new(1:31, 1), 1) ; 
cost_new(32:62, 1) = cost_new(32:62, 1)/norm(cost_new(32:62, 1), 1);
cnorm2005 = max(norm(cost_new(1:31, 1) - cost(1:31, 1)), norm(cost_new(32:62, 1) - cost(32:62, 1))) ; 
cost(:, 1) = cost_new(: , 1); %Updating

total_flow_matrix(:, :, 2) = t_exp_matrix(:, :, 2).*tradeflow_num_matrix(:, :, 2)./p_index_matrix(:, :, 2) ;
cost_new(:, 2) = transpose(sum(total_flow_matrix(:, :, 2), 1)) ; %new value%
cost_new(1:31, 2) = cost_new(1:31, 2)/norm(cost_new(1:31, 2), 1) ; 
cost_new(32:62, 2) = cost_new(32:62, 2)/norm(cost_new(32:62, 2), 1);
cnorm2010 = max(norm(cost_new(1:31, 2) - cost(1:31, 2)), norm(cost_new(32:62, 2) - cost(32:62, 2))) ;
cost(:, 2) = cost_new(: , 2); %Updating

end

%Normalizing so that ag productivity growth and nag prod growth have norm 1


master_raw.invcost_na_2005 = ones(31, 1)./cost(32:62, 1) ;
master_raw.invcost_na_2010 = ones(31, 1)./cost(32:62, 2) ;
master_raw.invcost_ag_2005 = ones(31, 1)./cost(1:31, 1)  ;
master_raw.invcost_ag_2010 = ones(31, 1)./cost(1:31, 2)  ;

%Price index growth (not net of trade elasticity)

master_raw.pindex_na_2005 = (p_index_matrix(1:31, 32, 1)).^(-1) ; 
master_raw.pindex_ag_2005 = (p_index_matrix(1:31, 1, 1)).^(-1) ;
master_raw.pindex_ag_2010 = (p_index_matrix(1:31, 1, 2)).^(-1) ;
master_raw.pindex_na_2010 = (p_index_matrix(1:31, 32, 2)).^(-1) ;

writetable(master_raw, "master.xlsx")





