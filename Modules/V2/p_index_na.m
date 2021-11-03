function ret = p_index_na(cost, year) 
global N parameters co_obj tradec tradef migc migf
    if year == 2005 
       tr1 = tradec.na_2002 ; 
       tr2 = tradec.na_2007 ;
       trf = tradef.na_2002 ;
    end

    if year == 2010
       tr1 = tradec.na_2007 ; 
       tr2 = tradec.na_2012 ;
       trf = tradef.na_2007 ;
    end

    theta = parameters.theta_n ;
    
    ret = ((tr1./tr2).*trf)*(cost.^(-theta)) ;
    ret = ret.^(-1/theta) ;
end