-module(gen_tcp_server_connection).
-author('baryluk@smp.if.uj.edu.pl').

% API

-export([start_link/5]).
-export([init/6]).

-export([behaviour_info/1]).

-export([client_loop/2]).
-export([update_code_stage1/2, update_code_stage2/2]).

-export([system_continue/3, system_terminate/4]).
-export([write_debug/3]).

-export([send_cached/1]).

-export([send_iolist/2]).


-record(gen_tcp_server_connection_state,
	{parent, % pid()
	port, % integer()
	module, % module()
	clientstate, % term
	clientsocket, % port()
	serverstate, % term()
	timeout, % integer()
	async_recv_ref % port()
	% debug
}).


% System exports

% Internal exports

behaviour_info(callbacks) ->
	[{init, 5},
	% init(Port, Module2, ClientSocket, ServerState2, AdditionalState) when is_integer(Port) ->
	% =>
	%  {ok, State}
	%  {ok, State, Timeout}
	%  {close, Reason, State}
	{handle_connection, 3},
	% handle_connection(_ClientState, _ClientSocket, _ServerState) ->
	{handle_data, 4},
	% handle_data(ClientState, _ClientSocket, _ServerState, Data) ->
	% =>
	%  ok
	%  {ok, State}
	%  {ok, State, Timeout}
	%  {send, Data, State}
	%  {close, Reason, State}
	%  update
	{handle_timeout, 4},
	% handle_timeout(ClientState, _ClientSocket, _ServerState, _Timeout) ->
	% =>
	%  ok
	%  {ok, State}
	%  {ok, State, Timeout}
	%  {send, Data, State}
	%  {close, State}
	%  update
	{handle_close, 4},
	% handle_close(_ClientState, _ClientSocket, _ServerState, _Reason) ->
	% =>
	%  ok
	{handle_msg, 4},
	% handle_msg(ClientState, _ClientSocket, _ServerState, _Msg) ->
	% =>
	%  {ok, State}
	%  {ok, State, Timeout}
	%  {send, Data, State}
	%  {close, State}
	%  update
	{code_change, 2}
	% code_change(_OldVsn, ClientState)
	% =>
	%  {ok, State2}
	];
behaviour_info(_Other) ->
	undefined.

start_link(Port, Module, ClientSocket, ServerState, InitArgsConnection) ->
	proc_lib:start_link(gen_tcp_server_connection, init, [self(), Port, Module, ClientSocket, ServerState, InitArgsConnection]).

set_opt(S, Opt) ->
	case prim_inet:setopts(S, [Opt]) of
		ok ->
			{ok, S};
		Error ->
			gen_tcp:close(S),
			Error
	end.

activate(S) when is_port(S) ->
	set_opt(S, {active, true}).

activate_once(S) when is_port(S) ->
	set_opt(S, {active, once}).

deactivate(S) when is_port(S) ->
	set_opt(S, {active, false}).

set_recbuf(S,Size) when is_port(S) ->
	set_opt(S, {recbuf,Size}).

init(Parent, Port, Module, ClientSocket, ServerState, InitArgsConnection) ->
	Debug = dbg_new([]), % [trace]
	Debug2 = dbg(Debug, init, [Parent, Port, Module, ClientSocket, ServerState, InitArgsConnection, inet:peername(ClientSocket)]),
	{ok, ClientState} = Module:init(Port, Module, ClientSocket, ServerState, InitArgsConnection),
	Debug3 = dbg(Debug2, client_state, [ClientState]),
	Debug4 = dbg(Debug3, ack),
	proc_lib:init_ack(Parent, {ok, self()}),
	Debug5 = dbg(Debug4, waiting_for_socket_permissions, [ClientSocket]),
	ok = receive
		{socket_given, _ServerPid, ClientSocket} ->
			ok
		after 100000 ->
			timeout
	end,
	Debug6 = dbg(Debug5, activate, [ClientSocket]),
	%{ok, ClientSocket2} = activate(ClientSocket),
	{ok, ClientSocket2} = deactivate(ClientSocket),
	{ok, ClientSocket} = set_recbuf(ClientSocket,100*1024),
	Debug7 = dbg(Debug6, entering_loop),
	State = #gen_tcp_server_connection_state{
		parent=Parent,
		port=Port,
		module=Module,
		clientstate=ClientState,
		clientsocket=ClientSocket2,
		serverstate=ServerState,
		timeout=360000,
		async_recv_ref=none}, % 6 minutes
	client_loop_start(State, Debug7).

client_loop_start(State=#gen_tcp_server_connection_state{clientstate=ClientState,clientsocket=ClientSocket,serverstate=ServerState,module=Module}, Debug) ->
	Debug2 = dbg(Debug, handle_connection),
	R = try
		Module:handle_connection(ClientState, ClientSocket, ServerState)
	catch
		error:{badmatch, {error, closed}} ->
			{close, {exception, error, socket_closed, erlang:get_stacktrace()}, ClientState};
		EClass:EValue ->
			{close, {exception, EClass, EValue, erlang:get_stacktrace()}, ClientState}
	end,
	client_process_return(R, State, Debug2).

% Linux specific way to prevent sending already buffered partial frames
% /usr/include/netinet/tcp.h
cork(TcpSocket) ->
	inet:setopts(TcpSocket,[{raw,6,3,<<1:32/native>>}]).
uncork(TcpSocket) ->
	inet:setopts(TcpSocket,[{raw,6,3,<<0:32/native>>}]).

send_iolist(TcpSocket, MyIoList) ->
	IoList = myiolist_to_iolist(MyIoList),
	Binary = iolist_to_binary(IoList),
	ok = gen_tcp:send(TcpSocket, Binary).

myiolist_to_iolist(X) when is_binary(X) ->
	X;
myiolist_to_iolist([C|_CTail] = X) when is_integer(C), C >= $\000, C =< $\377 ->
	X;
myiolist_to_iolist([X|Tail]) ->
	[myiolist_to_iolist(X)|myiolist_to_iolist(Tail)]; % not-tail recurisve!
myiolist_to_iolist(crlf) ->
	<<"\r\n">>;
myiolist_to_iolist([]) ->
	[].

send_iolist_real(TcpSocket, X) ->
	send_iolist_real(TcpSocket, X, notcork).


send_iolist_real(TcpSocket, X, _) when is_binary(X) ->
	ok = gen_tcp:send(TcpSocket, X);
send_iolist_real(TcpSocket, [C|_CTail] = X, _) when is_integer(C), C >= $\000, C =< $\377 ->
	ok = gen_tcp:send(TcpSocket, X);
send_iolist_real(TcpSocket, [X|Tail], Cork) ->
	ok = send_iolist_real(TcpSocket, X, Cork),
	send_iolist_real(TcpSocket, Tail, Cork);
send_iolist_real(TcpSocket, crlf, cork) ->
	ok = gen_tcp:send(TcpSocket, <<"\r\n">>),
	uncork(TcpSocket),
	cork(TcpSocket);
send_iolist_real(TcpSocket, crlf, _) ->
	ok = gen_tcp:send(TcpSocket, <<"\r\n">>);
send_iolist_real(_TcpSocket, [], _) ->
	ok.


send_cached(Data) ->
	self() ! {send_cached, Data}.

%%% Start async recv
%% This is undocumented. Can we send to tcp socket when we are in async_recv ?

start_async_recv(ClientSocket, Debug) ->
	%activate_once(ClientSocket),

	Length = 0,
	Time = -1,

	Debug2 = dbg(Debug, start_async_recv),

	% some undocumented code from prim_inet
	case prim_inet:async_recv(ClientSocket, Length, Time) of
		{ok, Ref} ->
			Debug3 = dbg(Debug2, start_async_recv_ok),
			{ok, Ref, Debug3};
		Error ->
			Debug3 = dbg(Debug2, start_async_recv_error, [Error]),
			{errro, Error, Debug3}
	end.


client_loop_recv(State=#gen_tcp_server_connection_state{clientsocket=ClientSocket}, Debug) ->
	Debug2 = dbg(Debug, client_loop_recv),
	{ok, AsyncRecvRef, Debug3} = start_async_recv(ClientSocket, Debug2),
	client_loop(State#gen_tcp_server_connection_state{async_recv_ref=AsyncRecvRef}, Debug3).

%%% main loop

%% start async recv if not already started and continue 

client_loop(State=#gen_tcp_server_connection_state{async_recv_ref=none}, Debug) ->
	client_loop_recv(State, Debug);

%% is async recv started wait for message from someone (ie. async recv or other process), clear async recv status, the process it

client_loop(State=#gen_tcp_server_connection_state{async_recv_ref=AsyncRecvRef,clientsocket=ClientSocket,timeout=Timeout}, Debug) ->
	Debug2 = dbg(Debug, client_loop_receive1),
	{HandleMsg, NewAsyncRecvRef, Debug3} = receive
		% some undocumented code from prim_inet
		{inet_async, ClientSocket, AsyncRecvRef, Status} ->
			Debug2a = dbg(Debug2, loop2a_recv),
			case Status of
				{ok, Data} ->
					Debug2b = dbg(Debug2a, loop2a_recv_data),
					% if some one want, one can already start recv here. this will probebly don't leed to Deniel of Service (only one recv will given)
					%{ok, NewAsyncRecvRef, Debug3} = start_async_recv(ClientSocket, Debug2b),
					{{tcp, ClientSocket, Data}, none, Debug2b};
				Error ->
					Debug2b = dbg(Debug2a, loop2a_recv_error, [Error]),
					{{tcp_closed, ClientSocket}, none, Debug2b}
					%{{tcp_error, ClientSocket}, none};
			end;
		% some undocumented code from prim_inet
		{'EXIT', ClientSocket, _Reason} ->
			Debug2a = dbg(Debug2, loop2b_err, [_Reason]),
			{{error, closed}, AsyncRecvRef, Debug2a};
			%{{tcp_closed, ClientSocket}, AsyncRecvRef, Debug2a};
			%{{tcp_error, ClientSocket}, AsyncRecvRef, Debug2a};
		Other ->
			Debug2a = dbg(Debug2, loop2b_other, [Other]),
			{Other, AsyncRecvRef, Debug2a}
		after Timeout ->
			Debug2a = dbg(Debug2, loop2b_timeout),
			{timeout, AsyncRecvRef, Debug2a}
	end,

	client_loop_handle(State#gen_tcp_server_connection_state{async_recv_ref=NewAsyncRecvRef}, Debug3, HandleMsg).

% handle received message

client_loop_handle(State=#gen_tcp_server_connection_state{clientstate=ClientState,clientsocket=ClientSocket,serverstate=ServerState,timeout=Timeout}, Debug, HandleMsg) ->
	Debug2 = dbg(Debug, client_loop_handle, [HandleMsg]),

	case HandleMsg of
		{channel, _, _, _, _, _, _} = Msg when Debug == [] ->
			client_msg(State, Debug2, Msg);

		{tcp, ClientSocket, Data} when Debug == [] ->
			client_data(State, Debug2, Data);

		{tcp, ClientSocket, Data} ->
			Debug3 = dbg(Debug2, data, [Data]),
			client_data(State, Debug3, Data);

		{send_cached, Data} ->
			Debug3 = dbg(Debug2, send_cached, [Data]),
			ok = gen_tcp:send(ClientSocket, Data),
			client_loop(State, Debug3);

		{tcp_closed, ClientSocket} ->
			Debug3 = dbg(Debug2, tcp_close),
			client_close(State, Debug3, {error, tcp_close});

		{tcp_error, ClientSocket} ->
			Debug3 = dbg(Debug2, close),
			client_close(State, Debug3, {error, connection_lost});

		{gen_tcp_server, From, msg, Msg} ->
			Debug3 = dbg(Debug2, server_msg, [From, Msg]),
			client_msg(State, Debug3, {server_msg, Msg});

		{gen_tcp_server, From, ClientSocket, ping, CountTimeouts, _Timestamp, _PingTimeout} ->
			From ! {gen_tcp_server, self(), ClientSocket, alive, CountTimeouts, now()},
			client_loop(State, Debug2);

		{gen_tcp_server, From, ClientSocket, close} ->
			client_close(State, Debug2, {server_request, From});

		{gen_tcp_server, _From, new_server, _NewServerPid} ->
			client_loop(State, Debug2);

		{gen_tcp_server, _From, new_serverstate, ServerState2} ->
			client_loop(State#gen_tcp_server_connection_state{serverstate=ServerState2}, Debug2);

		{gen_tcp_server, From, client_state} ->
			Module = State#gen_tcp_server_connection_state.module,
			From ! {gen_tcp_server, self(), client_state, Module, ClientState, ClientSocket, ServerState, Debug2, Timeout},
			client_loop(State, Debug2);

		{system, From, Request} ->
			Parent = State#gen_tcp_server_connection_state.parent,
			SysDebug = dbg_tosys(Debug2),
			sys:handle_system_msg(Request, From, Parent, gen_tcp_server_connection, SysDebug, State);

		update_code ->
			update_code_stage1(State, Debug2);

		timeout ->
			Debug3 = dbg(Debug2, timeout),
			client_timeout(State, Debug3);

		% Profilled/traced message
		{mpprof_msg_v1, OrgMsg, _, _, _} = MPMsg ->
			Debug3 = dbg(Debug2, mpprof_msg, [MPMsg]),
			client_loop_handle(State, Debug3, OrgMsg);

		Msg ->
			Debug3 = dbg(Debug2, msg, [Msg]),
			client_msg(State, Debug3, Msg)
	end.

client_data(State=#gen_tcp_server_connection_state{clientstate=ClientState,clientsocket=ClientSocket,serverstate=ServerState,module=Module}, Debug, Data) ->
	R = try
		% use timer:tc
		Now1 = now(),
		RR = Module:handle_data(ClientState, ClientSocket, ServerState, Data),
		Now2 = now(),
		io:format("handle_date end. Time ~p us~n", [timer:now_diff(Now2, Now1)]),
		RR
	catch
		error:{badmatch, {error, closed}} ->
			{close, {exception, error, socket_closed, erlang:get_stacktrace()}, ClientState};
		EClass:EValue ->
			{close, {exception, EClass, EValue, erlang:get_stacktrace()}, ClientState}
	end,
	client_process_return(R, State, Debug).

client_msg(State=#gen_tcp_server_connection_state{clientstate=ClientState,clientsocket=ClientSocket,serverstate=ServerState,module=Module}, Debug, Msg) ->
	R = try
		Now1 = now(),
		RR = Module:handle_msg(ClientState, ClientSocket, ServerState, Msg),
		Now2 = now(),
		io:format("handle_msg end. Time ~p us~n", [timer:now_diff(Now2, Now1)]),
		% use timer:tc
		RR
	catch
		error:{badmatch, {error, closed}} ->
			{close, {exception, error, socket_closed, erlang:get_stacktrace()}, ClientState};
		EClass:EValue ->
			{close, {exception, EClass, EValue, erlang:get_stacktrace()}, ClientState}
	end,
	client_process_return(R, State, Debug).

client_process_return(R, State=#gen_tcp_server_connection_state{clientsocket=ClientSocket}, Debug) ->
	case R of
		{send, Data, ClientState2} ->
			Debug2 = dbg(Debug, send_reply, [ClientSocket, Data]),
			% use timer:tc
			Now1 = now(),
			ok = send_iolist(ClientSocket, Data),
			Now2 = now(),
			io:format("client_process_return end. Time ~p us~n", [timer:now_diff(Now2, Now1)]),
			NewState = State#gen_tcp_server_connection_state{clientstate=ClientState2},
			client_loop(NewState, Debug2);

		{ok, ClientState2} ->
			NewState = State#gen_tcp_server_connection_state{clientstate=ClientState2},
			client_loop(NewState, Debug);

		{ok, ClientState2, Timeout2} ->
			NewState = State#gen_tcp_server_connection_state{clientstate=ClientState2,timeout=Timeout2},
			client_loop(NewState, Debug);

		{send, Data, ClientState2, Timeout2} ->
			Debug2 = dbg(Debug, send_reply, [Data]),
			ok = send_iolist(ClientSocket, Data),
			NewState = State#gen_tcp_server_connection_state{clientstate=ClientState2,timeout=Timeout2},
			client_loop(NewState, Debug2);

		{close, Reason, ClientState2} ->
			NewState = State#gen_tcp_server_connection_state{clientstate=ClientState2},
			client_close(NewState, Debug, {handler, Reason});

		{send_close, Data, Reason, ClientState2} ->
			Debug2 = dbg(Debug, send_close, [Data]),
			ok = send_iolist(ClientSocket, Data),
			NewState = State#gen_tcp_server_connection_state{clientstate=ClientState2},
			client_close(NewState, Debug2, {handler, Reason});

		update_code ->
			update_code_stage1(State, Debug);

		{change_module, Module2, ClientState2} ->
			NewState = State#gen_tcp_server_connection_state{clientstate=ClientState2,module=Module2},
			client_loop(NewState, Debug);

		Error ->
			exit(Error)
	end.

client_timeout(State=#gen_tcp_server_connection_state{clientstate=ClientState,clientsocket=ClientSocket,serverstate=ServerState,timeout=Timeout,module=Module}, Debug) ->
	Debug2 = dbg(Debug, timeout, [Timeout]),
	case Module:handle_timeout(ClientState, ClientSocket, ServerState, Timeout) of
		{ok, ClientState2} ->
			NewState = State#gen_tcp_server_connection_state{clientstate=ClientState2},
			client_loop(NewState, Debug2);
		{send_close, Data, Reason, ClientState2} ->
			Debug3 = dbg(Debug2, send_close, [Data]),
			ok = send_iolist(ClientSocket, Data),
			NewState = State#gen_tcp_server_connection_state{clientstate=ClientState2},
			client_close(NewState, Debug3, {handler, timeout_close, Reason});
		{close, Reason, ClientState3} ->
			NewState = State#gen_tcp_server_connection_state{clientstate=ClientState3},
			client_close(NewState, Debug2, {handler, timeout_close, Reason})
	end.

client_close(_State=#gen_tcp_server_connection_state{clientstate=ClientState,clientsocket=ClientSocket,serverstate=ServerState,module=Module}, Debug, Reason) ->
	_Debug2 = dbg(Debug, close, [Reason]),
	case Module:handle_close(ClientState, ClientSocket, ServerState, Reason) of
		{ok, Reason2} ->
			ok = gen_tcp:close(ClientSocket),
			Reason2;
		Error ->
			ok = gen_tcp:close(ClientSocket),
			exit(Error)
	end.

% Will be called in old module
update_code_stage1(State, Debug) ->
	gen_tcp_server_connection:update_code_stage2(State, Debug).

% Will be called in new module
update_code_stage2(State, Debug) ->
	gen_tcp_server_connection:client_loop(State, Debug).

% for sys:
system_continue(Parent, SysDebug, State) ->
	Debug = dbg_fromsys(SysDebug),
	client_loop(State#gen_tcp_server_connection_state{parent=Parent}, Debug).

% for sys:
system_terminate(Reason, Parent, SysDebug, State) ->
	Debug = dbg_fromsys(SysDebug),
	R = client_close(State#gen_tcp_server_connection_state{parent=Parent}, Debug, {terminate, Reason}),
	exit(R).

% for sys:
write_debug(Dev, Event, Name) ->
	Now = now(),
	io:format(Dev, "~p gen_tcp_server_connection: ~p event = ~p~n", [Now, Name, Event]).

dbg(Debug, Tag) ->
	dbg(Debug, Tag, []).

dbg({mydbg, SysDebug, OldTag, OldNow}, Tag, Args) ->
	StartNow = now(),
	NewSysDebug = sys:handle_debug(SysDebug, {gen_tcp_server_connection, write_debug}, Tag, Args),
	DiffMicroseconds = timer:now_diff(StartNow, OldNow),
	io:format("latencyprofiler: ~p-~p took ~p us~n", [OldTag, Tag, DiffMicroseconds]),
	NewNow = now(),
	{mydbg, NewSysDebug, Tag, NewNow};

dbg(SysDebug, Tag, Args) ->
	io:format("latencyprofiler: warning dbg with old Debug~n"),
	dbg(dbg_fromsys(SysDebug), Tag, Args).

dbg_new(Opts) ->
	SysDebug = sys:debug_options(Opts),
	{mydbg, SysDebug, dbg_new, now()}.

dbg_tosys({mydbg, SysDebug, _, _}) ->
	SysDebug.

dbg_fromsys(SysDebug) ->
	{mydbg, SysDebug, none, now()}.
