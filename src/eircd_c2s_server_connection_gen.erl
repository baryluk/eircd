-module(eircd_c2s_server_connection_gen).
-author('baryluk@smp.if.uj.edu.pl').

-behaviour(gen_tcp_server_connection).

-export([start_link/5]).

-export([init/5]).
-export([handle_connection/3, handle_data/4, handle_timeout/4, handle_msg/4, handle_close/4]).
-export([code_change/2]).

-compile([export_all]).

-include("eircd_c2s_server_connection_gen.hrl").
-include("eircd_errorcodes.hrl").


start_link(Port, eircd_c2s_server_connection_gen, ClientSocket, ServerState, AdditionalState) when is_integer(Port) ->
	gen_tcp_server_connection:start_link(Port, eircd_c2s_server_connection_gen, ClientSocket, ServerState, AdditionalState).

enable_buffering(ClientSocket) ->
	case prim_inet:setopts(ClientSocket, [{delay_send, true}, {sndbuf, 1024}]) of
		ok ->
			{ok, ClientSocket};
		Error ->
			gen_tcp:close(ClientSocket),
			Error
	end.

init(_Port, eircd_c2s_server_connection_gen, ClientSocket, _ServerState, _AdditionalState) when is_integer(_Port) ->
	process_flag(trap_exit, true),
	%{ok, ClientSocket} = enable_buffering(ClientSocket),
	{ok, {HostAddr, Port}} = inet:peername(ClientSocket),
	{ok, {hostent, HostName, [], inet, _IPV46, _IPAddresseList}} = inet:gethostbyaddr(HostAddr),
	Channels = dict:new(), % ChannelName -> ChannelPid mapping
	ClientState = #client_state{state=notlogged, hostaddr=HostAddr, port=Port, hostname=HostName, channels=Channels},
	{ok, ClientState}.

fqdn() ->
	%"tsk.ch.uj.edu.pl".
	"localhost".

handle_connection(ClientState, _ClientSocket, _ServerState) ->
	{send, eircd_messages:msg_banner(ClientState), ClientState}.

% cache?
get_channel(ClientState, ChannelName) ->
	{ClientState, ChannelName}.

% cache?
get_nick(ClientState, Nick) ->
	{ClientState, Nick}.

% handy shortcut
o(A,B,C,D) ->
	eircd_messages:output(A,B,C,D).

handle_data_eircd(ClientState, _ClientSocket, _ServerState, ParsedInput) ->
	case ParsedInput of
		{eircd, ping, Rest} ->
			{send, [<<"_EIRCD PONG ">>, Rest, crlf], ClientState};
		{eircd, trace, enable} ->
			Self = self(),
			spawn(fun() ->
				receive after 100 -> go end,
				sys:trace(Self, true) end),
			{send, [<<"_EIRCD TRACE ">>, "enabling", crlf], ClientState};
		{eircd, trace, disable} ->
			Self = self(),
			spawn(fun() ->
				receive after 100 -> go end,
				sys:trace(Self, false) end),
			{send, [<<"_EIRCD TRACE ">>, "disabling", crlf], ClientState};
		{eircd, timeout, Timeout} ->
			{send, [<<"_EIRCD TIMEOUT ">>, integer_to_list(Timeout), crlf], ClientState, Timeout};
		_ ->
			{ok, ClientState}
	end.


handle_data(ClientState=#client_state{state=notlogged,hostname=HostName,username=UserName,nick=Nick}, ClientSocket, ServerState, Data) ->
	ParsedInput = process_input(Data),
	case ParsedInput of



		% RFC2812 3.1.1 Password message
		% Command: PASS
		% Parameters: <password>
		% ERR_NEEDMOREPARAMS
		% ERR_ALREADYREGISTRED
		{pass, _Password} ->
			{ni, ClientState};

		% RFC2812 3.1.6 Service message
		% Command: SERVICE
		% Parameters: <nickname> <reserved> <distribution> <type>
		%				<reserved> <info>
		% NR:
		% ERR_ALREADYREGISTRED
		% ERR_NEEDMOREPARAMS
		% ERR_ERRONEUSNICKNAME
		% RPL_YOURESERVICE
		% RPL_YOURHOST
		% RPL_MYINFO
		{service, _Service, _Nickname, _Reserver, _Distribution, _Type, _Reserved, _Info} ->
			{ni, ClientState};

		% RFC2812 3.1.4 Oper message
		% Command: OPER
		% Parameters: <name> <password>
		% NR:
		% ERR_NEEDMOREPARAMS
		% RPL_YOUREOPER
		% ERR_NOOPERHOST
		% ERR_PASSWDMISMATCH
		{oper, _UserName0, _Password0} ->
			{ni, ClientState};

		% RFC2812 3.1.3 User message
		% Command: USER
		% Parameters: <user> <mode> <unused> <realname>
		% NR:
		% ERR_NEEDMOREPARAMS
		% ERR_ALREADYREGISTRED
		{user, UserName0, HostName0, ServerName0, RealName0} ->
			Nick = ClientState#client_state.nick,
			FullNick = case Nick of
				undefined -> "!" ++ UserName0 ++ "@" ++ HostName;
				_ -> Nick ++ "!" ++ UserName0 ++ "@" ++ HostName
			end,
			NewClientState = ClientState#client_state{
				servername=ServerName0,
				username=UserName0,
				realname=RealName0,
				hostname0=HostName0,
				fullnick=FullNick},
			case {Nick, UserName0} of
				{undefined, _} ->
					{ok, NewClientState};
				{_, _} ->
					NewClientState2 = NewClientState#client_state{state=logged},
					{send, eircd_messages:msg_after_login(NewClientState2), NewClientState2}
			end;

		% RFC2812 3.1.2 Nick message
		% Command: NICK
		% Parameters: <nickname>
		% NR:
		% ERR_NONICKNAMEGIVEN
		% ERR_ERRONEUSNICKNAME
		% ERR_NICKNAMEINUSE
		% ERR_NICKCOLLISION
		% ERR_UNAVAILRESOURCE
		% ERR_RESTRICTED
		{nick, NewNick} ->
			
			% TODO: check for bad charracters, not allowed !,
			% TODO: check length
			% TODO: {send, o("432", Nick, [NewNick], [<<"Erroneous Nickname">>]), ClientState}

% dodatkowe prefiksy username (nieoficjalne rozszerzenie), gdzie user!username@hostname
% noprefix: I line with ident
% ^: I line with OTHER type ident
% ~: I line, no ident
% +: i line with ident
% =: i line with OTHER type ident
% -: i line, no ident

			NicksHandler = eircd_nicks:new([]),

			% todo: if already registerd change nick!

			% TODO: case insensitive test please

			{RegisterNickReplay, _NicksHandler2} = eircd_nicks:register_link(NicksHandler, NewNick, "", self()),
			NewNick2 = case RegisterNickReplay of
				exists ->
					undefined;
				ok ->
					NewNick
			end,

			case {NewNick2, UserName} of
				{undefined, _} ->
					Nick2 = case Nick of
						undefined -> "*";
						N -> N
					end,
					{send, o("433", Nick2, [NewNick], [<<"Nickname is already in use.">>]), ClientState};
				{_, undefined} ->
					NewClientState = ClientState#client_state{
						nick=NewNick2},
					{ok, NewClientState};
				{_, _} ->
					NewFullNick2 = NewNick2 ++ "!" ++ UserName ++ "@" ++ HostName,
					NewClientState = ClientState#client_state{
						state=logged,
						nick=NewNick2,
						fullnick=NewFullNick2},
					{send, eircd_messages:msg_after_login(NewClientState), NewClientState}
			end;

		% RFC2812 3.1.7 Quit
		% Command: QUIT
		% Parameters: [ <Quit Message> ]
		% NR:
		% None.
		{quit, Reason} ->
			Reason2 = case Reason of
				noreason -> "";
				R -> R
			end,
			Nick2 = case Nick of
				undefined -> "";
				N ->
					NicksHandler = eircd_nicks:new([]),
					{_, _NicksHandler2} = eircd_nicks:unregister_link(NicksHandler, Nick, "", self()),
					N
			end,
			UserName2 = case UserName of
				undefined -> "";
				U -> U
			end,
			NewClientState = ClientState#client_state{state=quiting,reason=Reason2},
			{send_close, [<<"ERROR :Closing Link: ">>, Nick2, <<"[">>, UserName2, <<"@">>, HostName, <<"] (\"">>, Reason2, <<"\"">>, crlf], quit, NewClientState};

		{unknown, _Line, [First|_]} ->
			Nick2 = case Nick of
				undefined -> "*";
				N -> N
			end,
			{send, o("421", Nick2, [First], [<<"Unknown command">>]), ClientState};
			% most of servers ignore anything other than NICK, USER in authorization phase

		{eircd, _, _} ->
			handle_data_eircd(ClientState, ClientSocket, ServerState, ParsedInput);

		_ ->
			{ok, ClientState}
	end;

handle_data(ClientState=#client_state{state=logged,hostname=HostName,nick=Nick,fullnick=FullNick,channels=Channels}, ClientSocket, ServerState, Data) ->
	ParsedInput = process_input(Data),
	io:format("input=~p~n", [ParsedInput]),
	Fqdn = fqdn(),
	
	case ParsedInput of

		% RFC2812 3.3.1 Private messages
		% Command: PRIVMSG
		% Parameters: <msgtarget> <text to be sent>
		% NR:
		% ERR_NORECIPIENT
		% ERR_NOTEXTTOSEND
		% ERR_CANNOTSENDTOCHAN
		% ERR_NOTOPLEVEL
		% ERR_WILDTOPLEVEL
		% ERR_TOOMANYTARGETS
		% ERR_NOSUCHNICK
		% RPL_AWAY
		% Note: There are special extension $<mask> and #<mask> for operators
		% TODO: There can be only one Receiver, why this is list?
		{privmsg, [Receiver], Text} ->
			case Receiver of
				[$#|_] = Channel -> % kanal
					%{NewClientState, Channel} = get_channel(ClientState, Receiver),
					%{ok, NewClientState};
					ok = eircd_channel:broadcast_msg(Channel, {Nick, self()}, Text),
					{ok, ClientState};
				_ToNick ->
					{send, ["notimplemented", crlf], ClientState}
			end;

		% 3.3.2 Notice
		% Command: NOTICE
		% Parameters: <msgtarget> <text>
		% Same as PRIVMSG, but automatic replies MUST NEVER be sent in response.
		% NR:
		% Same ase PRIVMSG

		% prefixy: &(local) #(zwykly) +(bez channel modes) !(safe channel) (trzy ostatnie znane tylko dla serwerow w channel mask)

		% RFC2812 3.2.1 Join message
		% Command: JOIN
		% Parameters: ( <channel> *( "," <channel> ) [ <key> *( "," <key> ) ] )
		% 			/ "0"
		% NR:
		% ERR_NEEDMOREPARAMS
		% ERR_BANNEDFROMCHAN
		% ERR_INVITEONLYCHAN
		% ERR_BADCHANNELKEY
		% ERR_CHANNELISFULL
		% ERR_BADCHANMASK
		% ERR_NOSUCHCHANNEL
		% ERR_TOOMANYCHANNELS
		% ERR_TOOMANYTARGETS
		% ERR_UNAVAILRESOURCE
		% RPL_TOPIC

		{join, "0", []} ->
			% part all channels
			{ok, ClientState};

		{join, ChannelsNames, _Keys} ->
			% todo: in ChannelsName should be no dupplicates (but it is safe)
			NewChannels = lists:foldl(
							fun(ChannelName, Acc) ->
								case dict:find(ChannelName, Channels) of
									error ->
										{ok, ChannelPid} = case eircd_channel:channel_pid(ChannelName) of
											ChannelPid2 when is_pid(ChannelPid2) -> % join
												{ok, ChannelPid2};
											undefined -> % create
												{ok, ChannelPid2} = eircd_channel_sup:start_channel(ChannelName, {Nick, self()}),
												{ok, ChannelPid2}
										end,
										ok = eircd_channel:join(ChannelPid, Nick, FullNick, self()),
										link(ChannelPid), % to clean up in case of channel crash
										NewAcc = dict:store(ChannelName, ChannelPid, Acc),
										Members = eircd_channel:members(ChannelPid),
										ChannelMembers = case Members of
											[First|Rest] ->
												[First| lists:map(fun(Member) -> [$\ | Member] end, Rest)];
											First ->
												[First]
										end,
										TopicResponse = case eircd_channel:topic(ChannelPid) of
												{undefined, _, _} ->
													[];
												{Topic, TopicChangerFullNick, TopicChangedAt} ->
													{A, B, _} = TopicChangedAt,
													TopicChangedAt2 = [integer_to_list(A), integer_to_list(B)],
													[
													o("332", Nick, [ChannelName], [Topic]),
													o("333", Nick, [ChannelName, TopicChangerFullNick, TopicChangedAt2], [])
													]
										end,
										Response = [
											[":", FullNick, <<" JOIN :">>, ChannelName, crlf],
											TopicResponse,
											% todo: split ChannelMembers into small portions to not overflow line size limit
											o("353", Nick, ["=", ChannelName], [ChannelMembers, " "]),
											o("366", Nick, [ChannelName], [<<"End of NAMES list.">>])
										],
										%io:format("sending=~p~n", [Response]),
										gen_tcp_server_connection:send_iolist(ClientSocket, Response),
										NewAcc;
									{ok, _} ->
										Acc % already there, mayby recheck this
								end
							end,
						Channels,
						ChannelsNames), % iterate over all channelnames
			NewClientState = ClientState#client_state{channels=NewChannels},
			{ok, NewClientState};

		% RFC2812 3.2.8 Kick command
		% Command: KICK
		% Parameters: <channel> *( "," <channel> ) <user> *( "," <user> )
		% 			[<comment>]
		% For the message to be syntactically correct, there MUST be
		% either one channel parameter and multiple user parameter, or as many
		% channel parameters as there are user parameters.
		% NR:
		% ERR_NEEDMOREPARAMS
		% ERR_NOSUCHCHANNEL
		% ERR_BADCHANMASK
		% ERR_CHANOPRIVSNEEDED
		% ERR_USERNOTINCHANNEL
		% ERR_NOTONCHANNEL
		{kick, ChannelName, KickNick, KickReason} ->  % todo: {kick, [ChannelName], [KickNick], KickReason}
			% not nacassarly ok (ie. no souch nick)
			ok = eircd_channel:kick(ChannelName, KickNick, {Nick, self()}, KickReason),
			{ok, ClientState};

		% RFC2812 3.2.2 Part message
		% Command: PART
		% Parameters: <channel> *( "," <channel> ) [ <Part Message> ]
		% NR:
		% ERR_NEEDMOREPARAMS
		% ERR_NOSUCHCHANNEL
		% ERR_NOTONCHANNEL
		{part, ChannelsNames, PartReason} ->
			PartReason2 = case PartReason of
				noreason ->
					[];
				RealPartReason ->
					[" :", RealPartReason]
			end,
			% todo: in ChannelsName should be no dupplicates (but it is safe)
			NewChannels = lists:foldl(
							fun([$#|_]=ChannelName, Acc) ->
								case dict:find(ChannelName, Channels) of
									{ok, ChannelPid} when is_pid(ChannelPid) ->
										% not nacassarly ok (already part, or restart of channel, or kicked in parallel)
										ok = eircd_channel:part(ChannelPid, Nick, FullNick, PartReason),
										NewChannels = dict:erase(ChannelName, Acc),
										unlink(ChannelPid),
										gen_tcp_server_connection:send_iolist(ClientSocket,
												[":", FullNick, <<" PART ">>, ChannelName, PartReason2, crlf]),
										NewChannels;
									error ->
										Acc % we are not there, mayby recheck this
								end;
							(_ChannelName, Acc) ->
								Acc
							end,
						Channels,
						ChannelsNames), % iterate over all channelnames
			NewClientState = ClientState#client_state{channels=NewChannels},
			{ok, NewClientState};

		% 3.7.2 Ping message
		% Command: PING
		% Parameters: <server1> [ <server2> ]
		% NR:
		% ERR_NOORIGIN
		% ERR_NOSUCHSERVER

		% 3.7.3 Pong message
		% Command: PONG
		% Parameters: <server> [ <server2> ]
		% NR:
		% ERR_NOORIGIN
		% ERR_NOSUCHSERVER

		{ping, badargs} ->
			{send, o("461", Nick, [<<"PING">>], [<<"Not enough parameters">>]), ClientState};
		{ping, Payload} ->
			{send, [":", Fqdn, <<" PONG ">>, fqdn(), " :", Payload, crlf], ClientState};
		{ping, Payload, Fqdn} ->
			{send, [":", Fqdn, <<" PONG ">>, fqdn(), " :", Payload, crlf], ClientState};
		{ping, _Payload, Server2} ->
			{send, o("402", Nick, [Server2], [<<"No such server">>]), ClientState};

		% RFC2812 3.2.3 Channel mode message
		% Command: MODE
		% Parameters: <channel> *( ( "-" / "+" ) *<modes> *<modeparams> )
		% NR:
		% ERR_NEEDMOREPARAMS
		% ERR_KEYSET
		% ERR_NOCHANMODES
		% ERR_CHANOPRIVSNEEDED
		% ERR_USERNOTINCHANNEL
		% ERR_UNKNOWNMODE
		% RPL_CHANNELMODEIS
		% RPL_BANLIST
		% RPL_ENDOFBANLIST
		% RPL_EXCEPTLIST
		% RPL_ENDOFEXCEPTLIST
		% RPL_INVITELIST
		% RPL_ENDOFINVITELIST
		% RPL_UNIQOPIS

		{mode, [$#|_] = ChannelName} ->
			Result = o("324", Nick, [ChannelName, "+tin", " "], []),
			{send, Result, ClientState};
		{mode, [$#|_] = ChannelName, "b"} ->
			%o("367", Nick, [ChannelName, "*!*@213.186.*.*"], []),
			Result = o("368", Nick, [ChannelName], [<<"End of Channel Ban List">>]),
			{send, Result, ClientState};


		
		% make it secret
		{mode, [$#|_] = ChannelName, "+s"} ->
			Result = o("482", Nick, [ChannelName], [<<"You're not channel operator">>]),
			{send, Result, ClientState};
		% make it known
		{mode, [$#|_] = ChannelName, "-s"} ->
			Result = o("482", Nick, [ChannelName], [<<"You're not channel operator">>]),
			{send, Result, ClientState};

		{mode, [$#|_] = ChannelName, ModeFlags, ModeNick} ->
			ModeFlagsAdd = dict:new(),
			ModeFlagsRemove = dict:new(),
			
			% ModeFlags = ["+o"]
			% ModeFlags = ["-o"]
			% ModeFlags = ["+v"]
			% ModeFlags = ["-v"]
			Result = o("482", Nick, [ChannelName], [<<"You're not channel operator">>]),
			{send, Result, ClientState};

%        O - give "channel creator" status;
%        o - give/take channel operator privilege;
%        v - give/take the voice privilege;
%
%        a - toggle the anonymous channel flag;
%				nie pozwalac na NICK anonymous, or nie generowac QUIT dla nich tylko PART
%        i - toggle the invite-only channel flag;
%        m - toggle the moderated channel;
%        n - toggle the no messages to channel from clients on the
%            outside;
%        q - toggle the quiet channel flag;
%        p - toggle the private channel flag;
%        s - toggle the secret channel flag;
%        r - toggle the server reop channel flag;
%        t - toggle the topic settable by channel operator only flag;
%
%        k - set/remove the channel key (password);
%        l - set/remove the user limit to channel;
%
%        b - set/remove ban mask to keep users out;
%        e - set/remove an exception mask to override a ban mask;
%        I - set/remove an invitation mask to automatically override
%            the invite-only flag;

		
		% RFC2812 3.1.5 User mode message
		% Command: MODE
		% Parameters: <nickname>
		% 				*( ( "+" / "-" ) *( "i" / "w" / "o" / "O" / "r" ) )
		% NR:
		% ERR_NEEDMOREPARAMS
		% ERR_USERSDONTMATCH
		% ERR_UMODEUNKNOWNFLAG
		% RPL_UMODEIS
		{mode, _ModeNick, _ModeFlags} ->
			{ok, ClientState};


		% 3.6.1 Who query
		% Command: WHO
		% Parameters: [ <mask> [ "o" ] ]
		% NR:
		% ERR_NOSUCHSERVER
		% RPL_WHOREPLY
		% RPL_ENDOFWHO

		{who, [$#|_] = ChannelName} ->
			Members = try
				eircd_channel:members(ChannelName)
			catch
				% there can be no such channel
				exit:{noproc,_} ->
					[]
			end,
			R = lists:map(
						fun(Member) ->
							o("352", Nick, [ChannelName, Member, "localhost", "localhost", Member, "H@"], ["0", " ", "616L", " ", "Witold Baryluk"])
						end,
					Members),
			Result = [
				R,
				o("315", Nick, [ChannelName], [<<"End of WHO list.">>])
			],
			{send, Result, ClientState};

		% 3.6.2 Whois query
		% Command: WHOIS
		% Parameters: [ <target> ] <mask> *( "," <mask> )
		% NR:
		% ERR_NOSUCHSERVER
		% ERR_NONICKNAMEGIVEN
		% RPL_WHOISUSER
		% RPL_WHOISCHANNELS
		% RPL_WHOISCHANNELS
		% RPL_WHOISSERVER
		% RPL_AWAY
		% RPL_WHOISOPERATOR
		% RPL_WHOISIDLE
		% ERR_NOSUCHNICK
		% RPL_ENDOFWHOIS
		{whois, AskNick} ->
			AskNickSmall=AskNick, % lower case
			AskNick_HostName="costam",
			AskNick_IP="2.3.4.5",
			AskNick_FullName="Fake User",
			AskNick_Server="localhost",
			AskNick_ServerDesc="Experimental server",
			AskNick_Channels="@#mondello @#champagneria",
			{send, [
				o("311", Nick, [AskNick, AskNick_HostName, AskNick_IP, "*"], [AskNick_FullName]),
				o("319", Nick, [AskNick], [AskNick_Channels]),
				o("312", Nick, [AskNick, AskNick_Server], [AskNick_ServerDesc]),
				o("318", Nick, [AskNickSmall], [<<"End of WHOIS list">>])
				], ClientState};

		% RFC2812 3.2.4 Topic message
		% Command: TOPIC
		% Parameters: <channel> [ <topic> ]
		% NR:
		% ERR_NEEDMOREPARAMS
		% ERR_NOTONCHANNEL
		% RPL_NOTOPIC
		% RPL_TOPIC
		% ERR_CHANOPRIVSNEEDED
		% ERR_NOCHANMODES
		{topic, ChannelName, TopicText} ->
			ok = eircd_channel:topic(ChannelName, TopicText, Nick, self()),
			{ok, ClientState};


		% RFC2812 3.1.2 Nick message
		% Command: NICK
		% Parameters: <nickname>
		% NR:
		% ERR_NONICKNAMEGIVEN
		% ERR_ERRONEUSNICKNAME
		% ERR_NICKNAMEINUSE
		% ERR_NICKCOLLISION
		% ERR_UNAVAILRESOURCE
		% ERR_RESTRICTED

		{nick, Nick} -> % ignore changeing to the same nick
			{ok, ClientState};

		{nick, NewNick} ->
			%	IoList = o("432", Nick, [NewNick], [<<"Erroneous Nickname">>]),
			%	{send, IoList, ClientState}

			NicksHandler = eircd_nicks:new([]),
			{RegisterNickReplay, _NicksHandler2} = eircd_nicks:change(NicksHandler,
					Nick, FullNick, NewNick, self()),
			case RegisterNickReplay of
				new_exists ->
					IoList = o(443, Nick, [NewNick], [<<"Nickname is already in use.">>]),
					{send, IoList, ClientState};
				ok ->
					HostName = ClientState#client_state.hostname,
					UserName = ClientState#client_state.username,
					NewFullNick = NewNick ++ "!" ++ UserName ++ "@" ++ HostName,
					NewClientState = ClientState#client_state{
						nick=NewNick,
						fullnick=NewFullNick},
					% notice all needed channels
					dict:map(fun(ChannelName, _ChannelPid) ->
							eircd_channel:nick_change(ChannelName,
								Nick, FullNick,
								NewNick, NewFullNick)
						end,
						ClientState#client_state.channels
					),
					IoList = [":", FullNick, <<" NICK :">>, NewNick, crlf],
					{send, IoList, NewClientState}
				% old_notexists -> error
			end;

		% RFC2812 3.1.7 Quit
		% Command: QUIT
		% Parameters: [ <Quit Message> ]
		% NR:
		% None.
		{quit, Reason} ->
			Reason2 = case Reason of
				noreason -> "";
				R -> R
			end,

			NicksHandler = eircd_nicks:new([]),
			{_, _NicksHandler2} = eircd_nicks:unregister_link(NicksHandler, Nick, "", self()),

			NewClientState = ClientState#client_state{state=quiting,reason=Reason2},
			{send_close, [<<"ERROR :Closing Link: [">>, Nick, <<"@">>, HostName, <<"] (\"">>,Reason2,<<"\"">>, crlf], quit, NewClientState};

		% RFC2812 3.1.3 User message
		% Command: USER
		% Parameters: <user> <mode> <unused> <realname>
		% NR:
		% ERR_NEEDMOREPARAMS
		% ERR_ALREADYREGISTRED
		{user, _UserName, _HostName0, _ServerName, _RealName} ->
			{send, o("462", Nick, [], [<<"Unauthorized command (already registered)">>]), ClientState};

		% RFC2812 3.2.6 List message
		% Command: LIST
		% Parameters: [ <channel> *( "," <channel> ) [ <target> ] ]
		% NR:
		% ERR_TOOMANYMATCHES
		% ERR_NOSUCHSERVER
		% RPL_LIST
		% RPL_LISTEND
		% List channels and their topic.
		{list, Channels, _Target} ->
			{ni, ClientState};

		% RFC2812 3.2.5 Names message
		% Command: NAMES
		% Parameters: [ <channel> *( "," <channel> ) [ <target> ] ]
		% NR:
		% ERR_TOOMANYMATCHES
		% ERR_NOSUCHSERVER
		% RPL_NAMREPLY
		% RPL_ENDOFNAMES
		% List all nicknames visible to user
		{names, _Channels, _Target} ->
			{ni, ClientState};

		% RFC2812 3.1.8 Squit
		% Command: SQUIT
		% Parameters: <server> <comment>
		% NR:
		% ERR_NOPRIVILEGES
		% ERR_NOSUCHSERVER
		% ERR_NEEDMOREPARAMS
		{squit, _Server, _Comment} ->
			{ni, ClientState};

		% RFC2812 3.2.7 Invite message
		% Command: INVITE
		% Parameters: <nickname> <channel>
		% NR:
		% ERR_NEEDMOREPARAMS
		% ERR_NOSUCHNICK
		% ERR_NOTONCHANNEL
		% ERR_USERONCHANNEL
		% ERR_CHANOPRIVSNEEDED
		% RPL_INVITING
		% RPL_AWAY
		{invite, _InvitedNick, _ChannelName} ->
			{ni, ClientState};

		% RFC2812 3.4.1 Motd message
		% Command: MOTD
		% Parameters: [ <target> ]
		% NR:
		% RPL_MOTDSTART
		% RPL_MOTD
		% RPL_ENDOFMOTD
		% ERR_NOMOTD
		{motd, _Targe} ->
			{ni, ClientState};

		% RFC2812 3.4.2 Lusers message
		% Command: LUSERS
		% Parameters: [ <mask> [ <target> ] ]
		% NR:
		% RPL_LUSERCLIENT
		% RPL_LUSEROP
		% RPL_LUSERUNKOWN
		% RPL_LUSERCHANNELS
		% RPL_LUSERME
		% ERR_NOSUCHSERVER
		{lusers, _Mask, _Target} ->
			{ni, ClientState};

		% RFC2812 3.4.3 Version message
		% Command: VERSION
		% Parameters: [ <target> ]
		% NR:
		% ERR_NOSUCHSERVER
		% RPL_VERSION
		{version, _Target} ->
			{ni, ClientState};

		% RFC2812 3.4.4 Stats message
		% Command: STATS
		% Parameters: [ <query> [ <target> ] ]
		% NR:
		% ERR_NOSUCHSERVER
		% RPL_STATSLINKINFO
		% RPL_STATSUPTIME
		% RPL_STATSCOMMANDS
		% RPL_STATSOLINE
		% RPL_ENDOFSTATS
		{stats, _Query, _Target} ->
			{ni, ClientState};

		% RFC2812 3.4.5 Links message
		% Command: LINKS
		% Parameters: [ [ <remote server> ] <server mask> ]
		% NR:
		% ERR_NOSUCHSERVER
		% RPL_LINKS
		% RPL_ENDOFLINKS
		{links, RemoteServer, ServerMask} ->
			{ni, ClientState};

		% RFC2812 3.4.6 Time message
		% Command: TIME
		% Parameters: [ <target> ]
		% ERR_NOSUCHSERVER
		% NR:
		% RPL_TIME
		{time, _Target} ->
			{ni, ClientState};

		% RFC2812 3.4.7 Connect message
		% Command: CONNECT
		% Parameters: <target server> <port> [ <remote server> ]
		% NR:
		% ERR_NOSUCHSERVER
		% ERR_NOPRIVILEGES
		% ERR_NEEDMOREPARAMS
		{connect, _TargetServer, _TargetServerPort, _RemoteServer} ->
			{ni, ClientState};

		% RFC2812 3.4.8 Trace message
		% Command: TRACE
		% Parameters: [ <target> ]
		% NR:
		% ERR_NOSUCHSERVER
		% RPL_TRACELINK
		% RPL_TRACECONNECTING
		% RPL_TRACEHANDSHAKE
		% RPL_TRACEUNKNOWN
		% RPL_TRACEOPERATOR
		% RPL_TRACEUSER
		% RPL_TRACESERVER
		% RPL_TRACESERVICE
		% RPL_TRACENEWTYPE
		% RPL_TRACECLASS
		% RPL_TRACELOG
		% RPL_TRACEEND
		{trace, _Target} ->
			{ni, ClientState};

		% RFC2812 3.4.9 Admin command
		% Command: ADMIN
		% Parameters: [ <target> ]
		% NR:
		% ERR_NOSUCHSERVER
		% RPL_ADMINME
		% RPL_ADMINLOC1
		% RPL_ADMINLOC2
		% RPL_ADMINEMAIL
		{admin, _Target} ->
			{ni, ClientState};

		% RFC2812 3.4.10 Info command
		% Command: INFO
		% Parameters: [ <target> ]
		% NR:
		% ERR_NOSUCHSERVER
		% RPL_INFO
		% RPL_ENDOFINFO
		{info, _Target} ->
			{ni, ClientState};

		% RFC2812 3.5.1 Servlist message
		% Command: SERVLIST
		% Parameters: [ <mask> [ <type> ] ]
		% NR:
		% RPL_SERVLIST
		% RPL_SERVLISTEND
		{servlist, _Mask, _Type} ->
			{ni, ClientState};

		% 3.5.2 Squery
		% Command: SQUERY
		% Parameters: <servicename> <text>
		% NR: Like PRIVMSG.
		{squery, _ServiceName, _Text} ->
			{ni, ClientState};

		% 3.6.3 Whowas
		% Command: WHOWAS
		% Parameters: <nickname> *( "," <nickname> ) [ <count> [ <target> ] ]
		% NR:
		% ERR_NONICKNAMEGIVEN
		% ERR_WASNOSUCHNICK
		% RPL_WHOWASUSER
		% RPL_WHOISSERVER
		% RPL_ENDOFWHOWAS
		{whowas, _NickNames, _Count, _Target} ->
			{ni, ClientState};

		% 3.7.1 Kill message
		% Command: KILL
		% Parameters: <nickname> <comment>
		% NR:
		% ERR_NOPRIVILEGES
		% ERR_NEEDMOREPARAMS
		% ERR_NOSUCHNICK
		% ERR_CANTKILLSERVER
		{kill, _NickName, _Comment} ->
			{ni, ClientState};

		% 3.7.4 Error
		% Command: ERROR
		% Parameters: <error message>
		% NR:
		% None.
		{error, ErrorMessage} ->
			{error, ClientState, {client_error, ErrorMessage}};

		% 4.1 Away
		% Command: AWAY
		% Parameters: [ <text> ]
		% NR:
		% RPL_UNAWAY
		% RPL_NOWAWAY
		{away, _Text} ->
			{ni, ClientState};

		% 4.2 Rehash message
		% Command: REHASH
		% Parameters: None
		% NR:
		% RPL_REHASHING
		% ERR_NOPRIVILEGES
		rehash ->
			{ni, ClientState};

		% 4.3 Die message
		% Command: DIE
		% Parameters: None
		% ERR_NOPRIVILEGES
		die ->
			{ni, ClientState};

		% 4.4 Restart message
		% Command: RESTART
		% Parameters: None
		% NR:
		% ERR_NOPRIVILEGES
		restart ->
			{ni, ClientState};

		% 4.5 Summon message
		% Command: SUMMON
		% Parameters: <user> [ <target> [ <channel> ] ]
		% NR:
		% ERR_NORECIPIENT
		% ERR_FILEERROR
		% ERR_NOLOGIN
		% ERR_NOSUCHSERVER
		% ERR_SUMMONDISABLED
		% RPL_SUMMONING
		{summon, _User, _Target, _Channel} ->
			{ni, ClientState};

		% 4.6 Users
		% Command: USERS
		% Parameters: [ <target> ]
		% NR:
		% ERR_NOSUCHSERVER
		% ERR_FILEERROR
		% RPL_USERSSTART
		% RPL_USERS
		% RPL_NOUSERS
		% RPL_ENDOFUSERS
		% ERR_USERSDISABLED
		% Disabled Reply:
		% ERR_USERSDISABLED
		{users, _Target} ->
			{ni, ClientState};


		% 4.7 Operwall message
		% Command: WALLOPS
		% Parameters: <Text to be sent>
		% NR:
		% ERR_NEEDMOREPARAMS
		{wallops, _Text} ->
			{ni, ClientState};

		% 4.8 Userhost message
		% Command: USERHOST
		% Parameters: <nickname> *( SPACE <nickname> )
		% NR:
		% RPL_USERHOST
		% ERR_NEEDMOREPARAMS
		{userhost, _NickNames} ->
			{ni, ClientState};

		% 4.9 Ison message
		% Command: ISON
		% Parameters: <nickname> *( SPACE <nickname> )
		% NR:
		% RPL_ISON
		% ERR_NEEDMOREPARAMS
		{ison, _NickNames} ->
			{ni, ClientState};


		{unknown, _Line, [First|_]} ->
			IoList = o("421", Nick, [First], [<<"Unknown command">>]),
			{send, IoList, ClientState};

		{eircd, _, _} ->
			handle_data_eircd(ClientState, ClientSocket,
				ServerState, ParsedInput);
		_ ->
			{ok, ClientState}
	end.

process_input(FullLine) ->
	Len = size(FullLine) - 2, % chomp,
	<<Line:Len/binary, "\r\n">> = FullLine,
	eircd_parser:parse_input(Line).

% badargs

%% Timeout

handle_timeout(ClientState=#client_state{state=notlogged,hostname=HostName},
		_ClientSocket, _ServerState, _Timeout) ->
	IoList = [<<"ERROR :Closing Link: [unknown@">>, HostName, <<"] (Ping timeout)">>, crlf],
	{send_close, IoList, timeout_login, ClientState};
handle_timeout(ClientState=#client_state{hostname=HostName,nick=Nick},
		_ClientSocket, _ServerState, _Timeout) ->
	IoList = [<<"ERROR :Closing Link: [">>, Nick, "@", HostName, <<"] (Ping timeout)">>, crlf],
	{send_close, IoList, timeout, ClientState}.

%% Close

handle_close(_ClientState, _ClientSocket, _ServerState,
		{handler, {exception, error, socket_closed, _Stack}}) ->
	io:format("socket_closed~n"),
	{ok, socket_closed};
handle_close(_ClientState, _ClientSocket, _ServerState,
		{handler, {exception, Error, EValue, Stack}}) ->
	io:format("exception ~p:~p, call stack: ~p~n", [Error, EValue, Stack]),
	{ok, exception};
handle_close(_ClientState, _ClientSocket, _ServerState,
		{handler, Reason}) ->
	{ok, Reason};
handle_close(_ClientState, _ClientSocket, _ServerState,
		{handler, timeout_close, Reason}) ->
	{ok, Reason};
handle_close(_ClientState, _ClientSocket, _ServerState, Reason) ->
	{ok, Reason}.

%% Other messages


% handle_msg function

handle_msg(ClientState, ClientSocket, ServerState,
		{mpprof_msg_v1, _, _, _, _} = X) ->
	handle_msg(ClientState, ClientSocket, ServerState, mpprof:extract(X));

handle_msg(ClientState, _ClientSocket, _ServerState,
		{channel, _, _, ChannelName, _, _, _} = X) ->
	ChannelName = eircd_channel:decode_generic(X, channelname),
	Self = self(),
	case eircd_channel:decode_what(X) of
		justsend ->
			case eircd_channel:decode_justsend(X, from_clientpid) of
%				Self -> % ignore from self
%					{ok, ClientState};
				SomePid when is_pid(SomePid) ->
					IoList = eircd_channel:decode_justsend(X, iolist),
					{send, IoList, ClientState}
			end;
		msg ->
			case eircd_channel:decode_msg(X, from_pid) of
				Self -> % ignore from self
					{ok, ClientState};
				SomePid when is_pid(SomePid) ->
					IoList = eircd_messages2:handle_msg_msg(ChannelName,
						eircd_channel:decode_msg(X, from_fullnick),
						eircd_channel:decode_msg(X, text)),
					{send, IoList, ClientState}
			end;
		join ->
			case eircd_channel:decode_join(X, clientpid) of
				Self -> % ignore from self
					{ok, ClientState};
				SomePid when is_pid(SomePid) ->
					IoList = eircd_messages2:handle_msg_join(ChannelName,
						eircd_channel:decode_join(X, fullnick)),
					{send, IoList, ClientState}
			end;
		part ->
			IoList = eircd_messages2:handle_msg_part(ChannelName,
				eircd_channel:decode_part(X, fullnick),
				eircd_channel:decode_part(X, reason)),
			{send, IoList, ClientState};
		kick ->
			IoList = eircd_messages2:handle_msg_kick(ChannelName,
				eircd_channel:decode_kick(X, who_fullnick),
				eircd_channel:decode_kick(X, reason),
				eircd_channel:decode_kick(X, by_fullnick)),
			{send, IoList, ClientState};
		nick ->
			case eircd_channel:decode_nick(X, clientpid) of
				Self -> % ignore from self
					{ok, ClientState};
				SomePid when is_pid(SomePid) ->
					IoList = eircd_messages2:handle_msg_nick(
						eircd_channel:decode_nick(X, old_fullnick),
						eircd_channel:decode_nick(X, new_nick)),
					{send, IoList, ClientState}
			end;
		topic ->
			IoList = eircd_messages2:handle_msg_topic(ChannelName,
				eircd_channel:decode_topic(X, by_fullnick),
				eircd_channel:decode_topic(X, text)),
			{send, IoList, ClientState};
		stop ->
			IoList = eircd_messages2:handle_msg_stop(ChannelName),
			{send, IoList, ClientState};
		members ->
			IoList = eircd_messages2:handle_msg_members(ChannelName),
			{send, IoList, ClientState};
		_ ->
			{ok, ClientState}
	end;

handle_msg(ClientState, _ClientSocket, _ServerState, {ping, _From}) ->
	{send, [":sys!s@s PRIVMSG #d :cos", crlf], ClientState};

handle_msg(ClientState, _ClientSocket, _ServerState, {'EXIT', Pid, _Why}) ->
	io:format("client: umarl ~p bo ~p~n", [Pid, _Why]),
	{ok, ClientState};

handle_msg(ClientState, _ClientSocket, _ServerState, _Msg) ->
	{ok, ClientState}.


code_change(_OldVsn, ClientState) ->
	{ok, ClientState}.

