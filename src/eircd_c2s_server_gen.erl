-module(eircd_c2s_server_gen).
-author('baryluk@smp.if.uj.edu.pl').

-behavior(gen_tcp_server).

-export([start_link/1]).
-export([init/2]).
-export([handle_connection/4, handle_timeout/3, handle_msg/3]).
-export([code_change/2]).

start_link(Port) when is_integer(Port) ->
	gen_tcp_server:start_link(eircd_c2s_server_gen, Port, 1000, line, 300000, []).

init(_ListenSocket, _Args) ->
	{ok, {0}}.

handle_connection(Port, _ListenSocket, ServerState, _ClienSocket) ->
	Id_server_connection_sup = list_to_atom("eircd_c2s_server_connection_sup_" ++ integer_to_list(Port)),
	{NumerOfConnectionsAllready} = ServerState,
	io:format("~p ", [NumerOfConnectionsAllready]),
	NewServerState = {NumerOfConnectionsAllready+1},
	{spawn_using_supervisor, eircd_c2s_server_connection_gen, Id_server_connection_sup, [], NewServerState}.
	%{spawn_simple, eircd_c2s_server_connection_gen, [], NewServerState}.

handle_timeout(_ListenSocket, ServerState, _Timeout) ->
	{ok, ServerState}.

handle_msg(_ListenSocket, ServerState, _Msg) ->
	{ok, ServerState}.

code_change(_OldVsn, ServerState) ->
	{ok, ServerState}.
