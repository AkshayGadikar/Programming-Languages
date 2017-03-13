
-module(roots2).
-export([server/0, client/2]).

-import(roots1, [roots/3]).

server() ->
	spawn(fun loop/0).


%loop recevies 3 coefficients and call roots1:root function to evaluate roots	
loop() -> 
	receive 
		{From, {A,B,C}} -> 
			Response = roots1:roots(A, B, C),
			From ! {self(), Response},
			loop();
			stop -> 
				true 
	end.

	
%client provides coefficients and gets roots response as tuple
client(PID, Coeffs) ->
	PID  ! {self(),Coeffs}, 
	receive {PID, Response} -> 
				Response 
	end.