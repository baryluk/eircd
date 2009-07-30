-module(eircd_nicks).
-author('baryluk@smp.if.uj.edu.pl').

-behaviour(gen_server).

-export([start_link/0]).
-export([stop/1]).
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([new/1]).
-export([register_link/4, unregister_link/4, change/5,
	find_by_pid/2, find_by_nick/2, find_by_fullnick/2,
	rehash/1]).
-export([nicks/1]).

-include("eircd_nicks.hrl").

% external API

% Can be usefull later, as per process cache, and for round robin, prefered subserver, etc.
new(Opts) when is_list(Opts) ->
	{eircd_nicks}.

% {ok, _}, {exists, _}
register_link({UseServer} = New, Nick, FullNick, Pid) when is_pid(Pid) ->
	{gen_server:call(UseServer, {register_link, Nick, FullNick, Pid}), New}.

% {ok, _}, {notexists, _}
unregister_link({UseServer} = New, Nick, FullNick, Pid) when is_pid(Pid) ->
	{gen_server:call(UseServer, {unregister, Nick, FullNick, Pid}), New}.

% {ok, _}, {old_notexists, _}, {new_exists, _}
change({UseServer} = New, Nick, FullNick, NewNick, Pid) when is_pid(Pid) ->
	{gen_server:call(UseServer, {change, Nick, FullNick, NewNick, Pid}), New}.

% {[...], _}
nicks({UseServer} = New) ->
	{gen_server:call(UseServer, nicks), New}.

% {nick, _}
find_by_pid({UseServer} = New, Pid) when is_pid(Pid) ->
	{gen_server:call(UseServer, {find_by_pid, Pid}), New}.

% {<Pid>, _}
find_by_nick({UseServer} = New, Nick) when is_list(Nick) ->
	{gen_server:call(UseServer, {find_by_nick, Nick}), New}.

find_by_fullnick({UseServer} = New, FullNick) ->
	{gen_server:call(UseServer, {find_by_fullnick, FullNick}), New}.

rehash({UseServer} = New) ->
	{gen_server:call(UseServer, rehash), New}.

% gen_server callbacks


start_link() ->
	gen_server:start_link({local, eircd_nicks}, eircd_nicks, [], []).

stop(Reason) ->
	gen_server:call(eircd_nicks, {stop, Reason}).

init([]) ->
	process_flag(trap_exit, true),
	RegisterNicks = dict:new(), % nick -> pid maping
	RegisterPids = dict:new(), % pid -> [nick] maping
	{ok, #nicks_state{register_nicks=RegisterNicks,register_pids=RegisterPids}}.

%% internal API

add(State=#nicks_state{register_nicks=RegisterNicks,register_pids=RegisterPids}, {Nick, ClientPid}) ->
	case dict:is_key(Nick, RegisterNicks) of
		true ->
			{exists, State};
		false ->
			NewRegisterNicks = dict:store(Nick, ClientPid, RegisterNicks),
			NewRegisterPids = dict:append(ClientPid, Nick, RegisterPids),
			{ok, State#nicks_state{register_nicks=NewRegisterNicks,register_pids=NewRegisterPids}}
	end.

get_by_nick(_State=#nicks_state{register_nicks=RegisterNicks}, Nick) ->
	dict:find(Nick, RegisterNicks).

get_by_pid(_State=#nicks_state{register_pids=RegisterPids}, Pid) ->
	dict:find(Pid, RegisterPids).

remove(State=#nicks_state{register_nicks=RegisterNicks,register_pids=RegisterPids}, {Nick, ClientPid}) ->
	case dict:is_key(Nick, RegisterNicks) of
		true ->
			NewRegisterNicks = dict:erase(Nick, RegisterNicks),
			NewRegisterPids = dict:erase(ClientPid, RegisterPids),
			{ok, State#nicks_state{register_nicks=NewRegisterNicks,register_pids=NewRegisterPids}};
		false ->
			{notexists, State}
	end.

change_nick(State=#nicks_state{register_nicks=RegisterNicks,register_pids=RegisterPids}, {OldNick, NewNick}) ->
	case dict:find(OldNick, RegisterNicks) of
		{ok, Pid} ->
			case dict:is_key(NewNick, RegisterNicks) of
				true ->
					{new_exists, State};
				false ->
					NewRegisterNicks = dict:store(NewNick, Pid, RegisterNicks),
					NewRegisterNicks2 = dict:erase(OldNick, NewRegisterNicks),

					NewRegisterPids = dict:update(Pid,
						fun (OldNicksForPid) ->
							lists:append(NewNick, lists:delete(OldNick, OldNicksForPid))
						end,
					RegisterPids),

					{ok, State#nicks_state{register_nicks=NewRegisterNicks2,register_pids=NewRegisterPids}}
			end;
		_ ->
			{old_notexists, State}
	end.

get_nicks(_State=#nicks_state{register_nicks=RegisterNicks}) ->
	dict:fetch_keys(RegisterNicks).


pid_to_nicks(_State=#nicks_state{register_pids=RegisterPids}, Pid) when is_pid(Pid) ->
	case dict:find(Pid, RegisterPids) of
		error ->
			error;
		{ok, Nicks} ->
			{ok, Nicks}
	end.

% server API

handle_call({register_link, Nick, _FullNick, ClientPid}, _FromPid, State) ->
	case add(State, {Nick, ClientPid}) of
		{ok, NewState} ->
			io:format("eircd_nicks: Registered nick ~p with linked pid ~p~n", [Nick, ClientPid]),
			link(ClientPid), % to clean in case of crash of client
			{reply, ok, NewState};
		{exists, NewState} ->
			{reply, exists, NewState}
	end;

handle_call({unregister_link, Nick, _FullNick, ClientPid}, _FromPid, State) ->
	case remove(State, {Nick, ClientPid}) of
		{ok, NewState} ->
			unlink(ClientPid),
			{reply, ok, NewState};
		{notexists, NewState} ->
			{reply, notexists, NewState}
	end;

handle_call({change, OldNick, _OldFullNick, NewNick, _ClientPid}, _FromPid, State) ->
	case change_nick(State, {OldNick, NewNick}) of
		{ok, NewState} ->
			{reply, ok, NewState};
		{new_exists, NewState} ->
			{reply, new_exists, NewState};
		{old_notexists, NewState} ->
			{reply, old_notexists, NewState}
	end;

handle_call(nicks, _FromPid, State) ->
	{reply, get_nicks(State), State};

handle_call({find_by_nick, Nick}, _FromPid, State) when is_list(Nick) ->
	{reply, get_by_nick(State, Nick), State};

handle_call({find_by_pid, Pid}, _FromPid, State) when is_pid(Pid) ->
	{reply, get_by_pid(State, Pid), State};

handle_call({stop, Reason}, _FromPid, State) ->
	{stop, Reason, State};

handle_call(rehash, _FromPid, State) ->
	% check dead processes (by re-link/1'ing), and remove them
	{replay, ok, State};

handle_call(Request, _FromPid, State) ->
	io:format("eircd_nicks received unknown call: ~p~n", [Request]),
	{noreply, State}. % client will timeout


%%%%

handle_cast(Request, State) ->
	io:format("eircd_nicks received unknown cast: ~p~n", [Request]),
	{noreply, State}.


%%%%

handle_info({'EXIT', Pid, _Reason}, State) ->
	case pid_to_nicks(State, Pid) of
		{ok, Nicks} ->
			NewState = lists:foldl(
					fun(Nick, PrevState) ->
						io:format("eircd_nicks: Removing nick ~p because linked ~p died~n", [Nick, Pid]),
						case remove(PrevState, {Nick, Pid}) of
							{ok, NewState1} ->
								NewState1;
							{notexists, NewState1} ->
								io:format("eircd_nicks: Not removing nick ~p (it is already removed)~n", [Nick]),
								NewState1
						end
					end,
				State,
				Nicks),
			{noreply, NewState};
		_ ->
			{noreply, State}
	end;

handle_info(Msg, State) ->
	io:format("eircd_nicks received unknown message: ~p~n", [Msg]),
	{noreply, State}.


%%%%

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	io:format("code_change~n"),
	{ok, State}.
