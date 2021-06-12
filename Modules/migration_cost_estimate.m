%Date created: June 4th, 2021
%Date modified: June 10th, 2021
global theta epsilon eta Gs_2005 Gn_2005 Gs_2010 Gn_2010
%This module takes the migration elasticity w.r.t. real income and
%calculates what the implied migration costs are. 
%Are expressing migration elasticity with respect to an economic quantity
%index as in Samuelson and Swamy (1974) -- in part because the estimated
%utility parameters change the scale at which real income is measured. 

%Directory
cd C:\Users\James\Dropbox\SchoolFolder\SYP\data\MyData

%Stacked data 
master =  readtable('constructed_output/master_stacked.xlsx') ;
%Reading parameters from pref.estimate.R
parameters = readtable('constructed_output/pref_estimates.xlsx') ;
theta = parameters.Theta(1) ;
epsilon = parameters.Epsilon(1) ;
eta = parameters.Eta(1) ;
Gs_2005 = parameters.Gn_2005(1)*parameters.Gs_Gn_2005(1) ;
Gn_2005 = parameters.Gn_2005(1) ;
Gs_2010 = parameters.Gn_2010*parameters.Gs_Gn_2010(1) ;
Gn_2010 = parameters.Gn_2010 ;


%Constructing measures of compensating variation. (Or some other economic
%quantity index?)