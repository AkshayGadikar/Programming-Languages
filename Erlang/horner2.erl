-module(horner2).
-export([server/1, client/2, loop/1, do_log/2,createlist/2,polyeval/1,calculate/2,calculateaux/3,stopprocess/1]).


server(Coeffs) ->
		%function for creating list of process ID's as number of coefficients
		Pidlist = horner2:createlist(Coeffs,[]),
		Pidlist1 = lists:reverse(Pidlist),
		 spawn(fun() -> loop(Pidlist1) end).
  
%function creates list of PIDs
createlist(Coeffs,List) when Coeffs =:= []  -> List;
createlist(Coeffs,List) ->
	ServerPid = spawn(horner2,polyeval, [hd(Coeffs)]),
	List1 = lists:append([ServerPid], List),
	createlist(tl(Coeffs),List1).
		

%receive input of X from client and pass PID list and X for evaluation
loop(Pidlist) ->
	receive {From, X} ->
	Response = horner2:calculate(Pidlist,X),
	From ! {self(), Response},
	loop(Pidlist);
	stop ->
		stopprocess(Pidlist)
	end.

%function to evaluate polynomial
calculate(Pidlist,X) -> calculateaux(Pidlist,X,0).

calculateaux(Pidlist,_,Acc) when Pidlist =:= [] -> Acc;

calculateaux(Pidlist,X,Acc) ->
	Pid = hd(Pidlist),
	Pid ! {self(), {X,Acc}},
	receive {Pid, Value} ->
			   Acc1 = Value,
			calculateaux(tl(Pidlist),X,Acc1)
	end.

%every process has its own coefficient and evaluates using X and Acc
polyeval(Coef) ->
	receive {From, {X,Acc}} ->
				Acc1 = Coef + X * Acc,
				do_log(Coef,Acc1),
	From! {self(), Acc1},
	polyeval(Coef);
    stop ->
		do_log(Coef,stop),
		true
				end.

stopprocess(Pidlist) when Pidlist =:= [] ->true;
stopprocess(Pidlist) ->
			Pid = hd(Pidlist),
			Pid ! stop,
			stopprocess(tl(Pidlist)).


%client code to pass value of X and get value of evaluated polynomial.
client(PID, X) ->
	PID ! {self(), X}, 
	receive {PID, Response} -> 
				Response 
	end.


do_log(Coeff, Msg) ->
    io:format(standard_error, "coeff = ~w; ~w~n", [Coeff, Msg]).
