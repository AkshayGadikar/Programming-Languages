

%function evaluate roots of equation

-module(roots1).

-export([roots/3]).

roots(A, B, C) -> 
	Term1 = 2 * A,
	Term2 = math:sqrt((B*B) - (4*A*C)),
	Root1 = (-B + Term2) / Term1,
	Root2 = (-B - Term2) / Term1,
	{Root1,Root2}.
  	

	