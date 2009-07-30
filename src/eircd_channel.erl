-module(eircd_channel).
-author('baryluk@smp.if.uj.edu.pl').

-behaviour(gen_server).

-export([start_link/2]).
-export([stop/2]).
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([channel_pid/1]).

-export([broadcast_msg/2, broadcast_msg/3, topic/4]).
-export([state/1, topic/1, members/1]).
-export([join/4, part/4, nick_change/5]).
-export([kick/4, ban/3, unban/3, banlist/1]).
%-export([op/3, deop/3]).
%-export([voice/3, devoice/3]).
%-export([invite/3]).


-export([decode_what/1,
	decode_generic/2,
	decode_part/2, decode_join/2,
	decode_nick/2, decode_topic/2,
	decode_msg/2, decode_kick/2,
	decode_op/2, decode_deop/2,
	decode_voice/2, decode_devoice/2,
	decode_stop/2, decode_members/2,
	decode_justsend/2]).

-include("eircd_channel.hrl").

% helper functions

channel_id(ChannelName) when is_list(ChannelName) ->
	list_to_atom("eircd_channel$" ++ ChannelName ++ "$").

channel_pid(ChannelName) when is_list(ChannelName) ->
	whereis(channel_id(ChannelName)).


% calling API

%% brodcast_msg, topic

broadcast_msg(ChannelPid, Msg) when is_pid(ChannelPid)->
	gen_server:cast(ChannelPid, {broadcast_msg, Msg});
broadcast_msg(ChannelName, Msg) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:cast(Id, {broadcast_msg, Msg}).

broadcast_msg(ChannelPid, {_FromNick, _FromPid} = From, Msg) when is_pid(ChannelPid)->
	gen_server:cast(ChannelPid, {broadcast_msg, From, Msg});
broadcast_msg(ChannelName, {_FromNick, _FromPid} = From, Msg) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:cast(Id, {broadcast_msg, From, Msg}).

topic(ChannelPid, TopicText, Nick, Pid) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, {topic, TopicText, Nick, Pid});
topic(ChannelName, TopicText, Nick, Pid) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, {topic, TopicText, Nick, Pid}).

%% Kick, Ban, Unban, Banlist

kick(ChannelPid, WhoNick, {_FromNick, _FromPid} = By, KickReason) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, {kick, WhoNick, By, KickReason});
kick(ChannelName, WhoNick, {_FromNick, _FromPid} = By, KickReason) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, {kick,  WhoNick, By, KickReason}).

ban(ChannelPid, Mask, By) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, {ban, Mask, By});
ban(ChannelName, Mask, By) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, {ban, Mask, By}).

unban(ChannelPid, Mask, By) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, {unban, Mask, By});
unban(ChannelName, Mask, By) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, {unban, Mask, By}).

banlist(ChannelPid) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, banlist);
banlist(ChannelName) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, banlist).

% state, topic, members

state(ChannelPid) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, state);
state(ChannelName) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, state).

topic(ChannelPid) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, topic);
topic(ChannelName) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, topic).

members(ChannelPid) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, members);
members(ChannelName) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, members).

% join, part, nick_change

join(ChannelPid, Nick, FullNick, ClientPid) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, {join, Nick, FullNick, ClientPid});
join(ChannelName, Nick, FullNick, ClientPid) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, {join, Nick, FullNick, ClientPid}).

part(ChannelPid, Nick, FullNick, PartReason) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, {part, Nick, FullNick, self(), PartReason});
part(ChannelName, Nick, FullNick, PartReason) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, {part, Nick, FullNick, self(), PartReason}).

nick_change(ChannelPid, OldNick, OldFullNick, NewNick, NewFullNick) when is_pid(ChannelPid) ->
	gen_server:call(ChannelPid, {nick_change, OldNick, OldFullNick, NewNick, NewFullNick});
nick_change(ChannelName, OldNick, OldFullNick, NewNick, NewFullNick) when is_list(ChannelName) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, {nick_change, OldNick, OldFullNick, NewNick, NewFullNick}).

% message decoding API

decode_what({channel, _Version, _TimeStamp, _ChannelName, _ChannelPid, _From, Rest}) ->
	decode_what1(Rest).

decode_what1({justsend, _From, _IoList}) ->
	justsend;
decode_what1({msg, _Text}) ->
	msg;
decode_what1({join, {_FullNick, _ClientPid}}) ->
	join;
decode_what1({part, {_FullNick, _ClientPid}, _Reason}) ->
	part;
decode_what1({kick, {_FullNick, _ClientPid}, _Reason, {_ByFullNick, _ByClientPid}}) ->
	kick;
decode_what1({nick, {_OldFullNick, _ClientPid}, _NewNick}) ->
	nick;
decode_what1({topic, _TopicText, {_ByFullNick, _ByClientPid}, _AtDateTime}) ->
	topic;
decode_what1({op, {_FullNick, _ClientPid}, {_ByFullNick, _ByClientPid}}) ->
	op;
decode_what1({deop, {_FullNick, _ClientPid}, {_ByFullNick, _ByClientPid}}) ->
	deop;
decode_what1({voice, {_FullNick, _ClientPid}, {_ByFullNick, _ByClientPid}}) ->
	voice;
decode_what1({devoice, {_FullNick, _ClientPid}, {_ByFullNick, _ByClientPid}}) ->
	devoice;
decode_what1({stop, _Reason}) ->
	stop;
decode_what1({members, _List}) ->
	members.

decode_nickpart(unknown, fullnick) -> unknown;
decode_nickpart(unknown, pid) -> unknown;
decode_nickpart({FullNick, _Pid}, fullnick) -> FullNick;
decode_nickpart({_FullNick, Pid}, pid) -> Pid;
decode_nickpart(Pid, fullnick) when is_pid(Pid) -> unknown;
decode_nickpart(Pid, pid) when is_pid(Pid) -> Pid;
decode_nickpart(FullNick, fullnick) -> FullNick;
decode_nickpart(_FullNick, pid) -> unknown.


decode_generic({channel, _Version, _TimeStamp, ChannelName, ChannelPid, From, _Msg}, What) ->
	case What of
		channelname -> ChannelName;
		channelpid -> ChannelPid;
		from_fullnick -> decode_nickpart(From, fullnick);
		from_pid -> decode_nickpart(From, pid);
		from -> From;
		_ -> error
	end.


decode_msg({channel, _, _, _, _, _, {msg, Text}} = Whole, What) ->
	case What of
		text -> Text;
		from_fullnick -> decode_generic(Whole, from_fullnick);
		from_clientpid -> decode_generic(Whole, from_pid);
		_ -> decode_generic(Whole, What)
	end.

decode_join({channel, _, _, _, _, _, {join, Who}} = Whole, What) ->
	case What of
		fullnick -> decode_nickpart(Who, fullnick);
		clientpid -> decode_nickpart(Who, pid);
		_ -> decode_generic(Whole, What)
	end.

decode_part({channel, _, _, _, _, _, {part, Who, Reason}} = Whole, What) ->
	case What of
		reason -> Reason;
		fullnick -> decode_nickpart(Who, fullnick);
		clientpid -> decode_nickpart(Who, pid);
		_ -> decode_generic(Whole, What)
	end.

decode_kick({channel, _, _, _, _, _, {kick, Who, Reason, By}} = Whole, What) ->
	case What of
		who_fullnick -> decode_nickpart(Who, fullnick);
		who_clientpid -> decode_nickpart(Who, pid);
		by_fullnick -> decode_nickpart(By, fullnick);
		by_clientpid -> decode_nickpart(By, pid);
		reason -> Reason;
		_ -> decode_generic(Whole, What)
	end.

decode_nick({channel, _, _, _, _, _, {nick, Who, NewNick}} = Whole, What) ->
	case What of
		old_fullnick -> decode_nickpart(Who, fullnick);
		new_nick -> NewNick;
		clientpid -> decode_nickpart(Who, pid);
		_ -> decode_generic(Whole, What)
	end.

decode_justsend({channel, _, _, _, _, _, {justsend, _From, IoList}} = Whole, What) ->
	case What of
		iolist -> IoList;
		%from_fullnick -> decode_generic(From, from_fullnick);
		%from_clientpid -> decode_generic(From, from_pid);
		from_fullnick -> decode_generic(Whole, from_fullnick);
		from_clientpid -> decode_generic(Whole, from_pid);
		_ -> decode_generic(Whole, What)
	end.

decode_topic({channel, _, _, _, _, _, {topic, Text, By, AtDateTime}} = Whole, What) ->
	case What of
		text -> Text;
		by_fullnick -> decode_nickpart(By, fullnick);
		by_clientpid -> decode_nickpart(By, pid);
		atdatetime -> AtDateTime;
		_ -> decode_generic(Whole, What)
	end.

decode_op({channel, _, _, _, _, _, {op, _Who, _By}} = Whole, What) ->
	decode_mode_changes(Whole, What).

decode_deop({channel, _, _, _, _, _, {deop, _Who, _By}} = Whole, What) ->
	decode_mode_changes(Whole, What).

decode_voice({channel, _, _, _, _, _, {voice, _Who, _By}} = Whole, What) ->
	decode_mode_changes(Whole, What).

decode_devoice({channel, _, _, _, _, _, {devoice, _Who, _By}} = Whole, What) ->
	decode_mode_changes(Whole, What).

decode_mode_changes({channel, _, _, _, _, _, {_, {_FullNick, _ClientPid} = Who, {_ByFullNick, _ByClientPid} = By}} = Whole, What) ->
	case What of
		fullnick -> decode_nickpart(Who, fullnick);
		clientpid -> decode_nickpart(Who, pid);
		by_fullnick -> decode_nickpart(By, fullnick);
		by_clientpid -> decode_nickpart(By, pid);
		_ -> decode_generic(Whole, What)
	end.

decode_stop({channel, _, _, _, _, _, {stop, Reason}} = Whole, What) ->
	case What of
		reason -> Reason;
		_ -> decode_generic(Whole, What)
	end.

decode_members({channel, _, _, _, _, _, {members, List}} = Whole, What) ->
	case What of
		list -> List;
		_ -> decode_generic(Whole, What)
	end.


% gen_server callbacks



start_link([$#|_] = ChannelName, Owner) ->
	Id = channel_id(ChannelName),
	gen_server:start_link({local, Id}, eircd_channel, {ChannelName, Owner}, []).

stop(ChannelName, Reason) ->
	Id = channel_id(ChannelName),
	gen_server:call(Id, {stop, Reason}).

init({ChannelName, Owner}) ->
	process_flag(trap_exit, true),
	MembersNicks = dict:new(), % nick -> pid maping
	MembersPids = dict:new(), % pid -> nicks maping
	{ok, #channel_state{name=ChannelName,members_nicks=MembersNicks,members_pids=MembersPids,owner=Owner}}.

%% internal


%% sending

compose_msg(_State=#channel_state{name=ChannelName}, From, Msg) ->
	{channel, v1, now(), ChannelName, self(), From, Msg}.

send(State, Pid, From, Msg) ->
	Pid ! compose_msg(State, From, Msg).

% From  - pid(), or nick() or {pid(), nick()}
% Msg - iolist()
send_to_all(State=#channel_state{members_pids=MembersPids}, From, Msg) ->
	dict:map(
		fun(Pid, _Nicks) ->
			send(State, Pid, From, Msg)
		end,
		MembersPids),
	{ok, State}.

send_to_all_except(State=#channel_state{members_pids=MembersPids}, From, Msg, ExceptPid) ->
	dict:map(
		fun(Pid, _Nicks) ->
			if
			Pid =/= ExceptPid ->
				send(State, Pid, From, Msg);
			true ->
				none
			end
		end,
		MembersPids),
	{ok, State}.



add(State=#channel_state{members_nicks=MembersNicks,members_pids=MembersPids}, {Nick, ClientPid}) ->
	NewMembersNicks = dict:store(Nick, ClientPid, MembersNicks),
	NewMembersPids = dict:append(ClientPid, Nick, MembersPids),
	{ok, State#channel_state{members_nicks=NewMembersNicks,members_pids=NewMembersPids}}.

pid_to_nicks(_State=#channel_state{members_pids=MembersPids}, Pid) when is_pid(Pid) ->
	case dict:find(Pid, MembersPids) of
		error ->
			error;
		{ok, Nicks} ->
			{ok, Nicks}
	end.

nick_to_pid(_State=#channel_state{members_nicks=MembersNicks}, Nick) ->
	case dict:find(Nick, MembersNicks) of
		error ->
			error;
		{ok, Pid} ->
			{ok, Nick, Pid}
	end.

remove(State=#channel_state{members_nicks=MembersNicks,members_pids=MembersPids}, {Nick, ClientPid}) ->
	NewMembersNicks = dict:erase(Nick, MembersNicks),
	NewMembersPids = dict:erase(ClientPid, MembersPids),
	% if NewMembers lenght == 0, destroy channel?
	{ok, State#channel_state{members_nicks=NewMembersNicks,members_pids=NewMembersPids}}.

nicks(_State = #channel_state{members_nicks=MembersNicks}) ->
	dict:fetch_keys(MembersNicks).

% server API

handle_call({join, Nick, FullNick, ClientPid}, _FromPid, State) ->
	case add(State, {Nick, ClientPid}) of
		{ok, NewState} ->
			link(ClientPid), % to clean in case of crash of client
			{ok, NewState2} = send_to_all(NewState, {FullNick, ClientPid}, {join, {FullNick, ClientPid}}),
			{reply, ok, NewState2};
		{exists, NewState} ->
			{reply, already_joined, NewState}
	end;

handle_call(members, _FromPid, State) ->
	{reply, nicks(State), State};

handle_call(topic, _FromPid, State=#channel_state{topic=TopicText,topic_changer=TopicChangerFullNick,topic_changed_at=TopicChangedAt}) ->
	{reply, {TopicText, TopicChangerFullNick, TopicChangedAt}, State};

handle_call({topic, TopicText, Nick, Pid}, _FromPid, State) ->
	ChangedAt=now(),
	X = compose_msg(State, Pid, {topic, TopicText, {Nick, Pid}, ChangedAt}),
	IoList = eircd_messages2:handle_msg_topic(eircd_channel:decode_topic(X, channelname), eircd_channel:decode_topic(X, by_fullnick), eircd_channel:decode_topic(X, text)),
	{ok, NewState2} = send_to_all(State, {Nick, Pid}, {justsend, Pid, IoList}), % do in background?
	{reply, ok, NewState2#channel_state{topic=TopicText,topic_changer=Nick,topic_changed_at=ChangedAt}};

handle_call({ping, Payload}, _FromPid, State) ->
	{reply, {pong, Payload, {state, State}}, State};

handle_call(state, _FromPid, State) ->
	{reply, {state, State}, State};

handle_call({part, Nick, FullNick, ClientPid, Reason}, FromPid, State) ->
	{ok, NewState} = remove(State, {Nick, ClientPid}),
	{ok, NewState2} = send_to_all(NewState, {FullNick, FromPid}, {part, {FullNick, ClientPid}, Reason}),
	unlink(ClientPid),
	{reply, ok, NewState2};

handle_call({nick_change, OldNick, OldFullNick, NewNick, _NewFullNick}, FromPid, State) ->
	{ok, OldNick, ClientPid} = nick_to_pid(State, OldNick),
	{ok, NewState} = remove(State, {OldNick, ClientPid}),
	{ok, NewState2} = add(NewState, {NewNick, ClientPid}),
	send_to_all(NewState, {OldFullNick, FromPid}, {nick, {OldFullNick, ClientPid}, NewNick}),
	unlink(ClientPid),
	{reply, ok, NewState2};

handle_call({kick, WhoNick, {ByNick, _ByPid} = _By, KickReason}, FromPid, State) ->
	ByClientPid = FromPid,
	% check ACL
	case nick_to_pid(State, WhoNick) of
		{ok, WhoNick, WhoClientPid} ->
			send(State, WhoClientPid, {ByNick, FromPid}, {kick, {WhoNick, WhoClientPid}, KickReason, {ByNick, ByClientPid}}),
			{ok, NewState} = remove(State, {WhoNick, WhoClientPid}),
			{ok, NewState2} = send_to_all(NewState, {ByNick, FromPid}, {kick, {WhoNick, WhoClientPid}, KickReason, {ByNick, ByClientPid}}),
			unlink(WhoClientPid),
			{reply, ok, NewState2};
		error ->
			{reply, nosuchnick, State}
	end;

handle_call({stop, Reason}, FromPid, State) ->
	{ok, NewState} = send_to_all(State, {"system", FromPid}, {stop, Reason}),
	{stop, Reason, NewState};

handle_call(Request, _FromPid, State) ->
	io:format("eircd_channel received unknown call: ~p~n", [Request]),
	{noreply, State}.


% or to_iolist?
to_justsend(From, X) ->
	case decode_what(X) of
		msg ->
			IoList = eircd_messages2:handle_msg_msg(
				decode_msg(X, channelname),
				decode_msg(X, from_fullnick),
				decode_msg(X, text)),
			FromPid = decode_msg(X, from_clientpid),
			{{justsend, From, IoList}, FromPid};

		% @todo refactore, because this lines are very similar

		op ->
			IoList = eircd_messages2:handle_msg_oped(
				decode_op(X, channelname),
				decode_op(X, by_fullnick),
					decode_op(X, fullnick)),
			FromPid = decode_op(X, from_clientpid),
			{{justsend, From, IoList}, FromPid};
		deop ->
			IoList = eircd_messages2:handle_msg_deoped(
				decode_deop(X, channelname),
				decode_deop(X, by_fullnick),
				decode_deop(X, fullnick)),
			FromPid = decode_deop(X, from_clientpid),
			{{justsend, From, IoList}, FromPid};
		voice ->
			IoList = eircd_messages2:handle_msg_voiced(
				decode_voice(X, channelname),
				decode_voice(X, by_fullnick),
				decode_voice(X, fullnick)),
			FromPid = decode_voice(X, from_clientpid),
			{{justsend, From, IoList}, FromPid};
		devoice ->
			IoList = eircd_messages2:handle_msg_devoiced(
				decode_devoice(X, channelname),
				decode_devoice(X, by_fullnick),
				decode_devoice(X, fullnick)),
			FromPid = decode_devoice(X, from_clientpid),
			{{justsend, From, IoList}, FromPid}
	end.

%%%%

handle_cast({broadcast_msg, From, Text}, State) ->
	X = compose_msg(State, From, {msg, Text}),
	{IoList, FromPid} = to_justsend(From, X),
	FromPid = decode_msg(X, from_clientpid),
	{ok, NewState} = send_to_all_except(State, From, {justsend, From, IoList}, FromPid),
	%{ok, NewState} = send_to_all(State, From, {msg, Text}),
	{noreply, NewState};

handle_cast({broadcast_msg, Text}, State) ->
	{ok, NewState} = send_to_all(State, unknown, {msg, Text}),
	{noreply, NewState};

handle_cast(Request, State) ->
	io:format("eircd_channel received unknown cast: ~p~n", [Request]),
	{noreply, State}.


%%%%

handle_info({'EXIT', Pid, _Reason}, State) ->
	case pid_to_nicks(State, Pid) of
		{ok, Nicks} ->
			NewState = lists:foldl(
					fun(Nick, PrevState) ->
						{ok, NewState1} = remove(PrevState, {Nick, Pid}),
						{ok, NewState2} = send_to_all(NewState1, {Nick, Pid}, {part, {Nick, Pid}, "c2s process quit"}),
						NewState2
					end,
				State,
				Nicks),
			{noreply, NewState};
		_ ->
			{noreply, State}
	end;

handle_info(Msg, State) ->
	io:format("eircd_channel received unknown message: ~p~n", [Msg]),
	{noreply, State}.


%%%%

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	io:format("code_change~n"),
	{ok, State}.
