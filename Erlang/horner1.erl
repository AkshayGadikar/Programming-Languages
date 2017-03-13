-module(horner1).
-export([poly_eval/1,calculate/2,calculateaux/3]).


%anonymous function used to return 1 argument function
poly_eval(Coeffs) -> fun(X) -> 
					calculate(Coeffs,X) end.


calculate(Coeffs,X) -> calculateaux(Coeffs,X,0).

calculateaux(Coeffs,_,Acc) when Coeffs =:= [] -> Acc;
calculateaux(Coeffs,X,Acc) ->
		Var = hd(Coeffs),
		Acc1 = Var + X * Acc,
		calculateaux(tl(Coeffs),X,Acc1).