%Date created: October 13th 2021
%Date edited: October 14th 2021

%This file calibrates marginal costs (relative to RoW) and price indicies (Relative to RoW). 
%Uses trade cost output from tradecosts_constructV2.R

global parameters N tradec tradef

tradec = struct() ;
tradef = struct() ;

N=31*2 ; % Number of province/sector pairs. 31/62 are international sectors
%Load data (tradeflows + tradecosts) + parameters
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData
load constructed_outputV2/parameters
master = readtable('constructed_outputV2/master.xlsx') ;
master_stacked = readtable('constructed_outputV2/master_stacked.xlsx') ;
intl_tradecosts = readtable('constructed_outputV2/International_tradecosts.xlsx') ;
intl_tradecosts_array = table2array(intl_tradecosts(:, [1:6])) ;

tradec.ag_2002 = table2array(readtable('constructed_outputV2/HRtau_ag_2002'));
tradec.ag_2007 = table2array(readtable('constructed_outputV2/HRtau_ag_2007'));
tradec.ag_2012 = table2array(readtable('constructed_outputV2/HRtau_ag_2012'));
tradec.na_2002 = table2array(readtable('constructed_outputV2/HRtau_na_2002'));
tradec.na_2007 = table2array(readtable('constructed_outputV2/HRtau_na_2007'));
tradec.na_2012 = table2array(readtable('constructed_outputV2/HRtau_na_2012'));

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

%Relative spending by international on province goods (relative to spending
%on own goods)
int_sp = zeros(N/2, 6) ; 
int_sp(:, 1) = tradef.ag_2002(N/2, :)'/tradef.ag_2002(N/2, N/2) ;
int_sp(:, 2) = tradef.ag_2007(N/2, :)'/tradef.ag_2007(N/2, N/2) ;
int_sp(:, 3) = tradef.ag_2012(N/2, :)'/tradef.ag_2012(N/2, N/2) ;

int_sp(:, 4) = tradef.na_2002(N/2, :)'/tradef.na_2002(N/2, N/2) ;
int_sp(:, 5) = tradef.na_2007(N/2, :)'/tradef.na_2007(N/2, N/2) ;
int_sp(:, 6) = tradef.na_2012(N/2, :)'/tradef.na_2012(N/2, N/2) ;


% Calculating implied costs (Relative to RoW)
%Ag sector
ag_cost = (int_sp(:, [1:3]).*intl_tradecosts_array(:, [1:3])).^(-(1/parameters.theta_a)) ;
%Na sector
na_cost = (int_sp(:, [4:6]).*intl_tradecosts_array(:, [4:6])).^(-(1/parameters.theta_n)) ;

%Calculating implied price indices using self trade shares and these costs.
%
ag_pindex = zeros(N/2, 3) ;
na_pindex = zeros(N/2, 3) ;

ag_pindex(:, 1) = ag_cost(:, 1).*((diag(tradef.ag_2002)).^((1/parameters.theta_a))) ; 
ag_pindex(:, 2) = ag_cost(:, 2).*((diag(tradef.ag_2007)).^((1/parameters.theta_a))) ; 
ag_pindex(:, 3) = ag_cost(:, 3).*((diag(tradef.ag_2012)).^((1/parameters.theta_a))) ; 


na_pindex(:, 1) = na_cost(:, 1).*((diag(tradef.na_2002)).^((1/parameters.theta_n))) ; 
na_pindex(:, 2) = na_cost(:, 2).*((diag(tradef.na_2007)).^((1/parameters.theta_n))) ; 
na_pindex(:, 3) = na_cost(:, 3).*((diag(tradef.na_2012)).^((1/parameters.theta_n))) ; 


%Putting into master.xlsx/master_stacked.xlsx and saving as new output 
master.ag_cost_2002 = ag_cost(:, 1) ;
master.ag_cost_2007 = ag_cost(:, 2) ;
master.ag_cost_2012 = ag_cost(:, 3) ;
master.na_cost_2002 = na_cost(:, 1) ;
master.na_cost_2007 = na_cost(:, 2) ;
master.na_cost_2012 = na_cost(:, 3) ;

%For use with prod_calibrateV2.m
master.dCost_na_2005 = na_cost(:, 2)./na_cost(:, 1) ; %Changes in costs for use with other programs
master.dCost_ag_2005 = ag_cost(:, 2)./ag_cost(:, 1) ;
master.dpindex_ag_2005 = ag_pindex(:, 2)./ag_pindex(:, 1) ;
master.dpindex_na_2005 = na_pindex(:, 2)./na_pindex(:, 1) ;

master.ag_pindex_2002 = ag_pindex(:, 1) ;
master.ag_pindex_2007 = ag_pindex(:, 2) ;
master.ag_pindex_2012 = ag_pindex(:, 3) ;
master.na_pindex_2002 = na_pindex(:, 1) ;
master.na_pindex_2007 = na_pindex(:, 2) ;
master.na_pindex_2012 = na_pindex(:, 3) ;


%for use with pref_estimateV2.R
master_stacked.cost = [ag_cost(:, 1); ag_cost(:, 2); ag_cost(:, 3); na_cost(:, 1) ; na_cost(:, 2) ; na_cost(:, 3)] ;
master_stacked.pindex_ag = [ag_pindex(:, 1); ag_pindex(:, 2); ag_pindex(:, 3); ag_pindex(:, 1); ag_pindex(:, 2); ag_pindex(:, 3)] ;
master_stacked.pindex_na = [na_pindex(:, 1); na_pindex(:, 2); na_pindex(:, 3); na_pindex(:, 1); na_pindex(:, 2); na_pindex(:, 3)] ;
master_stacked.dpindex_ag = [NaN(N/2, 1); ag_pindex(:, 2)./ag_pindex(:, 1); ag_pindex(:, 3)./ag_pindex(:, 2) ; NaN(N/2, 1); ag_pindex(:, 2)./ag_pindex(:, 1); ag_pindex(:, 3)./ag_pindex(:, 2)] ;
master_stacked.dpindex_na = [NaN(N/2, 1); na_pindex(:, 2)./na_pindex(:, 1); na_pindex(:, 3)./na_pindex(:, 2) ; NaN(N/2, 1); na_pindex(:, 2)./na_pindex(:, 1); na_pindex(:, 3)./na_pindex(:, 2)] ;

%%
writetable(master, "constructed_outputV2/master2.xlsx")
writetable(master_stacked, "constructed_outputV2/master_stacked2.xlsx")

cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData\constructed_outputV2
save tradecosts_struct tradec
save tradeflows_struct tradef