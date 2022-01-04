%Date created: June 28th 2021
%Date edited: June 28th 2021

%This file calculates trade imbalance factors using trade data, GDP data
%and consumption shares in agriculture. 

%Production network
phi_an = 0.04 ;
phi_aa = 0.16 ;
phi_na = 0.25 ;
phi_nn = 0.61 ;
vashare_n = 1 - phi_an - phi_nn ;
vashare_a = 1 - phi_aa - phi_na ;

cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData

%Master data and trade flow data
master_raw =  readtable('master_raw.xlsx') ;
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

N=31*2; %Number of province/sector pairs
        
%Gross output/VA data. (I.e. expressing in terms of gross output) 
%nomVA(:, 1) = [master_raw.nomY_ag_2000 ; master_raw.nomY_na_2000] ;
%nomVA(:, 2) = [master_raw.nomY_ag_2005 ; master_raw.nomY_na_2005] ;
%nomVA(:, 3) = [master_raw.nomY_ag_2010 ; master_raw.nomY_na_2010] ;

nomGDP = zeros(N, 3) ; 
nomGDP(:, 1) = [master_raw.nomY_ag_2000/vashare_a ; master_raw.nomY_na_2000/vashare_n] ;
nomGDP(:, 2) = [master_raw.nomY_ag_2005/vashare_a ; master_raw.nomY_na_2005/vashare_n] ;
nomGDP(:, 3) = [master_raw.nomY_ag_2010/vashare_a ; master_raw.nomY_na_2010/vashare_n] ;

%Agricultural consumption share data
ag_share = zeros(N, 3) ; %
ag_share(:, 1) = [master_raw.agspend_ag_2000 ; master_raw.agspend_na_2000] ;
ag_share(:, 2) = [master_raw.agspend_ag_2005 ; master_raw.agspend_na_2005] ;
ag_share(:, 3) = [master_raw.agspend_ag_2010 ; master_raw.agspend_na_2010] ;



%% Calculating
Sa = [repelem(phi_aa, N/2, N/2), repelem(phi_na, N/2, N/2) ; repelem(phi_an, N/2, N/2), repelem(phi_nn, N/2, N/2)] ; %production network matrix

%2000 

Tradeflows_2000 = [ag_tradeflows_2002, na_tradeflows_2002 ; ag_tradeflows_2002, na_tradeflows_2002] ; 
Agspend_2000 = [repelem(ag_share(:, 1), 1, N/2), ones(N, N/2) - repelem(ag_share(:, 1), 1, N/2)] ; 

xi_2000 = calibrate_tradeimb(Tradeflows_2000, Agspend_2000, nomGDP(:, 1), Sa, vashare_a, vashare_n, N) ;




