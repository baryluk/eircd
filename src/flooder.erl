-module(flooder).

-compile([export_all]).

flood(N) ->
	flood("locahost", 6667, N).

flood(Host, Port, N) ->
	[spawn(fun () -> start_one(Host, Port, I) end) || I <- lists:seq(1, N)].


start_one(Host, Port, I) ->
	{ok, P} = gen_tcp:connect(Host, Port, [{active, true}]),
	loop(P, I, 0).

loop(P, I, X) ->
	receive
		after 5000 ->
			ok
	end,
	D = "SPING "++integer_to_list(I)++"/"++integer_to_list(X)++"\r\n",
	ok = gen_tcp:send(P, D),
	receive
		{tcp, P, _Data} ->
			loop(P, I, X+1)
	end.
