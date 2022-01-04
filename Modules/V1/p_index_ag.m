function ret = p_index_ag(cost, year) 
global N parameters co_obj tradec tradef migc migf

    if year == 2005 
       tr1 = tradec.ag_2002 ; 
       tr2 = tradec.ag_2007 ;
       trf = tradef.ag_2002 ;
    end

    if year == 2010
       tr1 = tradec.ag_2007 ; 
       tr2 = tradec.ag_2012 ;
       trf = tradef.ag_2007 ;
    end
    theta = parameters.theta ;
    
    ret = ((tr1./tr2).*trf)*(cost.^(-theta)) ;
    ret = ret.^(-1/theta) ;
end