-module(gen_tcp_server).
-author('baryluk@smp.if.uj.edu.pl').
-vsn('1.0').

-export([start_link/6, start_link/5, start_link/4, start_link/3, stop/1]).
-export([init/7]).
-export([behaviour_info/1]).

-export([socket_loop/2]).
-export([update_code_stage1/2, update_code_stage2/2]).

-export([system_continue/3, system_terminate/4]).

-export([write_debug/3]).


-record(gen_tcp_server_state,
	{parent, % pid()
	port, % integer()
	module, % module()
	listensocket, % port()
	accept_ref=none, % pid()
	serverstate, % term()
	timeout, % integer()
	max, % integer()
	activeclients % list()
	% debug
}).

behaviour_info(callbacks) ->
	[{init, 2},
	% =>
	%  {ok, ServerState}
	%  {error, Reason}
	{handle_connection, 4},
	% =>
	%  {ok, Module2, Supervisor2, AdditionalState, ServerState2} ->
	%  {close, ServerState2} ->
	{handle_timeout, 3},
	{handle_msg, 3},
	{handle_close, 3},
	{code_change, 2}];
behaviour_info(_Other) ->
	undefined.

start_link(Module, Port, InitArgs) ->
	start_link(Module, Port, raw, InitArgs).

start_link(Module, Port, Packet, InitArgs) ->
	start_link(Module, Port, 1000, Packet, InitArgs).

start_link(Module, Port, Max, Packet, InitArgs) ->
	start_link(Module, Port, Max, Packet, 60000, InitArgs).

start_link(Module, Port, Max, Packet, Timeout, InitArgs) when is_integer(Port), is_integer(Max), is_integer(Timeout) ->
	Name = port_name(Port),
	case whereis(Name) of
		undefined ->
			proc_lib:start_link(gen_tcp_server, init, [self(), Module, Port, Max, Packet, Timeout, InitArgs]);
		_Pid ->
	    	{error, already_started}
    end.

stop(Port) when is_integer(Port) ->
	Name = port_name(Port),
	case whereis(Name) of
		undefined ->
			not_started;
		Pid ->
			Pid ! stop,
			receive
				{gen_tcp_server, Pid, ok} -> ok
				after 1000 -> ok
			end,
			exit(Pid, kill),
			(catch unregister(Name)),
			stopped
	end.

port_name(Port) when is_integer(Port) ->
	list_to_atom("portServer" ++ integer_to_list(Port)).

write_debug(Dev, Event, Name) ->
	io:format(Dev, "gen_tcp_server: ~p event = ~p~n", [Name, Event]).

init(Parent, Module, Port, Max, Packet, Timeout, InitArgs) ->
	Name = port_name(Port),
	Debug = sys:debug_options([]),

	Debug2 = sys:handle_debug(Debug, {gen_tcp_server, write_debug}, trap_exit, []),
	process_flag(trap_exit, true),
	Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, starting, Port),
	Debug4 = sys:handle_debug(Debug3, {gen_tcp_server, write_debug}, registering, [Name, self()]),
	register(Name, self()),
	Debug5 = sys:handle_debug(Debug4, {gen_tcp_server, write_debug}, listen, Port),
	Opts = [binary,
			% {dontroute, true},
			% {nodelay, true},
			{packet, Packet},
			{reuseaddr, true},
			{active, false}, % active false, becaouse we want to quickly change controlling process
			                 % and need control flow in receving process
			{backlog, 128}],
	{ok, ListenSocket} = case gen_tcp:listen(Port, Opts) of
		{ok, _ListenSocket} = S1 ->
			S1;
		{error, Reason1} ->
			exit(Reason1);
		_Error ->
			exit(tcp_listen_error)
	end,
	Debug6 = sys:handle_debug(Debug5, {gen_tcp_server, write_debug}, init_call, [InitArgs]),
	{ok, ServerState} = case Module:init(ListenSocket, InitArgs) of
		{ok, _ServerState} = S2 ->
			S2;
		{error, Reason2} ->
			exit(Reason2);
		_ ->
			exit(init_call_error)
	end,
	Debug7 = sys:handle_debug(Debug6, {gen_tcp_server, write_debug}, ack, [Parent, self()]),
	proc_lib:init_ack(Parent, {ok, self()}),
	Debug8 = sys:handle_debug(Debug7, {gen_tcp_server, write_debug}, entering_socket_loop, []),
	State = #gen_tcp_server_state{
		parent=Parent,
		port=Port,
		module=Module,
		listensocket=ListenSocket,
		serverstate=ServerState,
		timeout=Timeout,
		max=Max,
		activeclients=[]},
	socket_loop0(State, Debug8).

socket_loop0(State=#gen_tcp_server_state{listensocket=ListenSocket}, Debug) ->
	Debug2 = sys:handle_debug(Debug, {gen_tcp_server, write_debug}, accept, []),
	AcceptRef = case prim_inet:async_accept(ListenSocket, -1) of
		{ok, Ref} ->
			Ref;
		{error, emfile} -> % too many open files
			none
	end,
	socket_loop(State#gen_tcp_server_state{accept_ref=AcceptRef}, Debug2).

socket_loop(
	State=#gen_tcp_server_state{
		parent=Parent,
		module=Module,
		listensocket=ListenSocket,
		serverstate=ServerState,
		timeout=Timeout,
		activeclients=ActiveClients}, Debug2) ->
	receive
		{inet_async, ListenSocket, _Ref2, {ok, ClientSocket}} ->
			{ok, ClientSocket} = accept_opts(ListenSocket, ClientSocket), % what if ClientSocket will be quickly closed by client?
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, accept, [inet:peername(ClientSocket)]),
			possibly_start_accept(State, Debug3, ClientSocket);

		{inet_async, ListenSocket, _Ref3, {error,timeout} = Error3} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, inet_async_timeout, [Error3]),
			socket_loop(State, Debug3);

		{inet_async, ListenSocket, _Ref3, Error3} ->
			_Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, inet_async_error, [Error3]),
			Error3;

		{'EXIT', Parent, Why} ->
			% shutdown procedure
			terminate(State, Why).

		stop ->
			terminate(State, stop).

		{'EXIT', Pid, Why} ->
			io:format("umarlo: pid=~p reason=~p", [Pid, Why]),
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, child_exit, [Pid, Why]),
			NewActiveClients = lists:delete(Pid, ActiveClients),
			socket_loop(State#gen_tcp_server_state{activeclients=NewActiveClients}, Debug3);

		{gen_tcp_server_connection, child, From} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, child_msg, [From]),
			From ! {session_server},
			socket_loop(State, Debug3);

		{system, From, Request} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, system_msg, [From]),
			sys:handle_system_msg(Request, From, Parent, gen_tcp_server, Debug3, State);

		{get_modules, From} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, get_modules_msg, [From]),
			%From ! {modules, [gen_tcp_server]},
			%sys:handle_system_msg(Request, From, Parent, gen_tcp_server, Debug3, State);
			exit({got_get_modules, probably_misconfigured_supervisor_with_dynamic_modules}).

		update_code ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, update_code, []),
			socket_loop(State, Debug3);

		{gen_tcp_server, isstarted, _SupProcess, ClientSocket, {ok, ClientPid}} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, isstarted, [ClientSocket, ClientPid]),
			ActiveClients1 = [ClientPid|ActiveClients],
			Debug4 = sys:handle_debug(Debug3, {gen_tcp_server, write_debug}, changeing_control_process, [ClientSocket, ClientPid]),
			ok = gen_tcp:controlling_process(ClientSocket, ClientPid),
			Debug5 = sys:handle_debug(Debug4, {gen_tcp_server, write_debug}, registering_in_inet_db, []),
			true = inet_db:register_socket(ClientSocket, inet_tcp), % bug in erlang:socket_lookup?
			Debug6 = sys:handle_debug(Debug5, {gen_tcp_server, write_debug}, sending_ack, [ClientSocket, ClientPid]),
			ClientPid ! {socket_given, self(), ClientSocket},
			socket_loop(State#gen_tcp_server_state{activeclients=ActiveClients1}, Debug6);

		{gen_tcp_server, isstarted, _SupProcess, ClientSocket, Error} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, isstarted_error, [ClientSocket, Error]),
			socket_loop(State, Debug3);

		{tcp_closed, ListenSocket} ->
			_Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, tcp_close, []),
			tcp_close;

		{tcp_closed, ClientSocket} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, tcp_close_not_for_us, [ClientSocket]),
			socket_loop(State, Debug3);

		{tcp, ClientSocket, _Data} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, tcp_data_not_for_us, [ClientSocket]),
			socket_loop(State, Debug3);

		{gen_tcp_server, From, ClientSocket, alive, CountTimeout, Timestamp} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, alive, [From, ClientSocket, CountTimeout, Timestamp]),
			socket_loop(State, Debug3);

		{gen_tcp_server, From, ClientSocket, new_controler, NewControler} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, new_controler, [From, ClientSocket, NewControler]),
			socket_loop(State, Debug3);

		{gen_tcp_server, From, state} ->
			From ! {gen_tcp_server, self(), state, State, Debug2},
			socket_loop(State, Debug2);

		Other ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, other_msg, [Other]),
			case Module:handle_msg(ListenSocket, ServerState, Other) of
				{ok, ServerState2} ->
					socket_loop(State#gen_tcp_server_state{serverstate=ServerState2}, Debug3)
			end
		after Timeout ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, timeout, [Timeout]),
			case Module:handle_timeout(ListenSocket, ServerState, Timeout) of
				{ok, ServerState2} ->
					socket_loop(State#gen_tcp_server_state{serverstate=ServerState2}, Debug3)
			end
			% ClientPid ! {gen_tcp_server, self(), ClientSocket, ping, 0, now(), 10000}
	end.

possibly_start_accept(State, Debug, none)  ->
	socket_loop(State, Debug);
possibly_start_accept(State=#gen_tcp_server_state{activeclients=ActiveClients,max=Max}, Debug, ClientSocket) ->
	case length(ActiveClients) of
		N when N < Max ->
			Debug2 = sys:handle_debug(Debug, {gen_tcp_server, write_debug}, accept2, [N, Max]),
			start_accept(State, Debug2, ClientSocket);
		N ->
			Debug2 = sys:handle_debug(Debug, {gen_tcp_server, write_debug}, too_much_clients, [N, Max]),
			gen_tcp:close(ClientSocket),
			socket_loop0(State, Debug2)
	end.

start_accept(State=#gen_tcp_server_state{port=Port,listensocket=ListenSocket,serverstate=ServerState,module=Module}, Debug, ClientSocket) ->
	Debug2 = sys:handle_debug(Debug, {gen_tcp_server, write_debug}, accept3, []),
	case Module:handle_connection(Port, ListenSocket, ServerState, ClientSocket) of
		{spawn_using_supervisor, Module2, Supervisor2, AdditionalState, ServerState2} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, spawn_using_supervisor, [Supervisor2, [Module2, ClientSocket, ServerState2, AdditionalState]]),
			Self = self(),
			spawn(fun () ->
					case supervisor:start_child(Supervisor2, [Module2, ClientSocket, ServerState2, AdditionalState]) of
						{ok, Pid} ->
							Self ! {gen_tcp_server, isstarted, self(), ClientSocket, {ok, Pid}};
						Error ->
							Self ! {gen_tcp_server, isstarted, self(), ClientSocket, Error}
					end
				end
			),
			socket_loop0(State#gen_tcp_server_state{serverstate=ServerState2}, Debug3);
		{spawn_simple, Module2, AdditionalState, ServerState2} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, spawn_simple, []),
			Self = self(),
			spawn(fun () ->
					case apply(Module2, start_link, [Module2, ClientSocket, ServerState2, AdditionalState]) of
						{ok, Pid} ->
							Self ! {gen_tcp_server, isstarted, self(), ClientSocket, {ok, Pid}};
						Error ->
							Self ! {gen_tcp_server, isstarted, self(), ClientSocket, Error}
					end
				end
			),
			socket_loop0(State#gen_tcp_server_state{serverstate=ServerState2}, Debug3);
		{close, ServerState2} ->
			Debug3 = sys:handle_debug(Debug2, {gen_tcp_server, write_debug}, ignoring, []),
			socket_loop0(State#gen_tcp_server_state{serverstate=ServerState2}, Debug3)
	end.

system_continue(Parent, Debug, State) ->
	socket_loop(State#gen_tcp_server_state{parent=Parent}, Debug).

system_terminate(Reason, _Parent, _Debug, _State) ->
	terminate(State, Reason).

terminate(_State, Reason) ->
	exit(Reason).

% Will be called in old module
update_code_stage1(State, Debug) ->
	gen_tcp_server:update_code_stage2(State, Debug).

% Will be called in new module
update_code_stage2(State, Debug) ->
	gen_tcp_server:socket_loop(State, Debug).

%% taken from prim_inet
%% setup options from listen socket on the connected socket
accept_opts(ListenSocket, ClientSocket) when is_port(ListenSocket), is_port(ClientSocket) ->
	case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
		{ok, Opts} ->
			case prim_inet:setopts(ClientSocket, Opts) of
				ok ->
					{ok, ClientSocket};
				Error ->
					gen_tcp:close(ClientSocket),
					Error
			end;
		Error ->
			gen_tcp:close(ClientSocket),
			Error
	end.
