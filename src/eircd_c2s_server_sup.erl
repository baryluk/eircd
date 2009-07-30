-module(eircd_c2s_server_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Port) when is_integer(Port) ->
	Id_server_sup = list_to_atom("eircd_c2s_server_sup_" ++ integer_to_list(Port)),
	supervisor:start_link({local, Id_server_sup}, eircd_c2s_server_sup, Port).

init(Port) when is_integer(Port) ->
	Id_server_gen = list_to_atom("eircd_c2s_server_gen_" ++ integer_to_list(Port)),
	Id_server_connection_sup = list_to_atom("eircd_c2s_server_connection_sup_" ++ integer_to_list(Port)),
	{ok, {{one_for_one, 1, 60},
		[{Id_server_gen, {eircd_c2s_server_gen, start_link, [Port]},
			permanent, 20000, worker, [eircd_c2s_server_gen]},
		{Id_server_connection_sup, {eircd_c2s_server_connection_sup, start_link, [Port]},
			permanent, infinity, supervisor, [eircd_c2s_server_connection_sup]}]}}.
