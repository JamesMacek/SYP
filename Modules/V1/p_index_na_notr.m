function ret = p_index_na_notr(cost, year) 
global N parameters co_obj tradec tradef migc migf
    if year == 2005    
       trf = tradef.na_2002 ;
    end

    if year == 2010 
       trf = tradef.na_2007 ;
    end

    theta = parameters.theta ;
    
    ret = (trf)*(cost.^(-theta)) ;
    ret = ret.^(-1/theta) ;
end