
%This module controls global parameters for use with other programs.
%Date created: October 8th 2021
%Date edited: November 13th 2021

global parameters 

%output directory
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData\constructed_outputV2

parameters = struct() ;

%Production network from Hao et al (2020) absorbing capital into labour
%share (no capital in this model)
parameters.phi_an = 0.04 ;
parameters.phi_nn = 0.61 ;
parameters.phi_aa = 0.16 ;
parameters.phi_na = 0.25 ;

parameters.vashare_n = 1 - parameters.phi_an - parameters.phi_nn ;
parameters.vashare_a = 1 - parameters.phi_aa - parameters.phi_na ;
parameters.land_na = 0.01 ;
parameters.land_ag = 0.26 ;
parameters.lab_na = parameters.vashare_n - parameters.land_na; %Make sure all sum to 1: CRS from perspective of firm
parameters.lab_ag = parameters.vashare_a - parameters.land_ag ;

%Trade elasticity by sector
parameters.theta_n = 4 ;
parameters.theta_a = 4 ; %Tombe, Tombe and Zhu, Simonovska and Waugh, Donaldson

%Elasticity of substitution ag for nonag/income elasticity (IF NOT USING
%ESTIMATES!!! Must specify in parameter below).
parameters.eta = 0.3 ; %Comin et al, etc. 
parameters.epsilon = 0.1 ; %Comin et. al, etc. 

parameters.pointest_eta_epsilon = 1 ; %0 or 1:  If 1 use estimated values other than what is directly above

%Income elasticity of migration
parameters.kappa = 1.5 ; %Tombe and Zhu (2019) 

%Consumer expenditure share on housing
parameters.nu = 0.13 ; %Tombe and Zhu (2019)

%Agglomeration
parameters.alpha_a = 0.04 ;
parameters.alpha_n = 0.04 ;

%Estimates--Is land perfectly mobile or set to data levels?
parameters.land_mobile = 1 ; %0 or 1. if 0, takes a really long time to run (to compute all equilibria)



save parameters parameters


%% Run master files in order

