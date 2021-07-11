%This function calculates the trade imbalances using matlab's fsolve.
%Solution may not be unique. 

function ret1 = calibrate_tradeimb(tradeflows, agspend, nomGDP, prod, vashare_a, vashare_n, N)
    nomVA = [vashare_a*nomGDP([1:N/2]) ; vashare_n*nomGDP([(N/2 + 1):N])] ; 
    
    function ret2 = ToSolve(xi) 
       xi_factor = sum(nomVA, 'all')/(sum(nomVA.*xi, 'all')) ; %To ensure market clearing. 
       ret2 = ((agspend.*tradeflows)')*((xi_factor*xi).*nomVA) + ((prod.*tradeflows)')*nomGDP - nomGDP ;
    end
    fun = @ToSolve ;
    ret1 = fsolve(fun, ones(N, 1)) ; %NOTE: Does not search only on unit simplex. 
end