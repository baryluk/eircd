-module(eircd_messages).
-author('baryluk@smp.if.uj.edu.pl').

-include("eircd_c2s_server_connection_gen.hrl").
-include("eircd_errorcodes.hrl").

-compile([export_all]).

fqdn() ->
	eircd_c2s_server_connection_gen:fqdn().

output0(From, Code, Nick, Fields, Rest) ->
	[<<":">>, From, <<" ">>, Code, <<" ">>, Nick, <<" ">>, output_fields(Fields), Rest, crlf].

output(Code, Nick, Fields, Rest) ->
	output(fqdn(), Code, Nick, Fields, Rest).

% todo 020 , leading zeros
output(From, Code, Nick, Fields, Rest) when is_integer(Code) ->
	output(From, integer_to_list(Code), Nick, Fields, Rest);
output(From, Code, Nick, Fields, []) ->
	output0(From, Code, Nick, Fields, []);
output(From, Code, Nick, [], Rest) ->
	output0(From, Code, Nick, [], [<<":">>, Rest]);
output(From, Code, Nick, Fields, Rest) ->
	output0(From, Code, Nick, Fields, [<<" :">>, Rest]).

output_fields([]) ->
	[];
output_fields([H]) ->
	H;
output_fields([H|Tail]) ->
	[H, <<" ">>, output_fields(Tail)].

% messages

msg_banner(_State=#client_state{}) ->
	output(<<"020">>, <<"*">>, [], [<<"Please wait while we process your connection.">>]).

msg_welcome(_State=#client_state{nick=Nick,fullnick=FullNick}) ->
	output("001", Nick, [], [<<"Welcome to the Internet Relay ">>, FullNick]).

version() ->
	"0.0.0.1".

msg_hostandversion(_State=#client_state{nick=Nick}) ->
	output("002", Nick, [], [<<"Your host is ">>, fqdn(), <<", running version ">>, version()]).

starttime() ->
	"wto lip 15 2008 at 13:37:45 CEST".

msg_uptime(_State=#client_state{nick=Nick}) ->
	output("003", Nick, [], [<<"This server was created ">>, starttime()]).

msg_support0(_State=#client_state{nick=Nick}) ->
	output("004", Nick, [fqdn(), version(), "aoOirw", "abeiIklmnoOpqrRstv"], []).
msg_support(_State=#client_state{nick=Nick}) ->
	[output("005", Nick, ["RFC2812", "PREFIX=(ov)@+", "CHANTYPES=#&!+", "MODES=3", "CHANLIMIT=#&!+:21", "NICKLEN=15", "TOPICLEN=255", "KICKLEN=255", "MAXLIST=beIR:64", "CHANNELLEN=50", "IDCHAN=!:5", "CHANMODES=beIR,k,l,imnpstaqr"], [<<"are supported by this server">>]),
	output("005", Nick, ["PENALTY", "FNC", "EXCEPTS=e", "INVEX=I", "CASEMAPPING=ascii", "NETWORK=IRCnet"], [<<"are supported by this server">>])].

unique_id() ->
	"616LAETTB".
msg_unique_id(_State=#client_state{nick=Nick}) ->
	UniqueId = unique_id(),
	output("042", Nick, [UniqueId], [<<"your unique ID">>]).

stats_general() ->
	[{users, 95376}, {services, 7}, {servers, 35}].
msg_stats_general(_State=#client_state{nick=Nick}) ->
	[{users, A0},
	{services, B0},
	{servers, C0}] = stats_general(),
	A = integer_to_list(A0),
	B = integer_to_list(B0),
	C = integer_to_list(C0),
	output("251", Nick, [], [<<"There are ">>, A ,<<" users and ">>, B, <<" services on ">>, C, <<" servers">>]).

stats_operators() ->
	157.
msg_stats_operators(_State=#client_state{nick=Nick}) ->
	A = integer_to_list(stats_operators()),
	output("252", Nick, [A], [<<"operators online">>]).

stats_unknown() ->
	24.
msg_stats_unknown(_State=#client_state{nick=Nick}) ->
	A = integer_to_list(stats_unknown()),
	output("253", Nick, [A], [<<"unknown connections">>]).

stats_channels() ->
	51235.
msg_stats_channels(_State=#client_state{nick=Nick}) ->
	A = integer_to_list(stats_channels()),
	output("254", Nick, [A], [<<"channels formed">>]).

local_users() ->
	100.
local_users_max() ->
	1000.
global_users() ->
	200.
global_users_max() ->
	2000.

msg_local0(_State=#client_state{nick=Nick}) ->
	A = integer_to_list(local_users()),
	output("255", Nick, [], [<<"I have ">>, A, <<" users, 0 services and 1 servers">>]).

msg_local(_State=#client_state{nick=Nick}) ->
	A = integer_to_list(local_users()),
	B = integer_to_list(local_users_max()),
	output("265", Nick, [A, B], [<<"Current local users ">>, A, <<", max ">>, B]).
msg_global(_State=#client_state{nick=Nick}) ->
	A = integer_to_list(global_users()),
	B = integer_to_list(global_users_max()),
	output("266", Nick, [A, B], [<<"Current global users ">>, A, <<", max ">>, B]).


motd_text() ->
	[
	<<"/-------------------------------------------------------------\\">>,
	<<"| Witamy na serwerze IRCNET, Krakow, POLSKA porty 6667 i 6668 |">>,
	<<">-------------------------------------------------------------<">>,
	<<"|                 Inne serwery irc w Polsce:                  |">>,
	<<"|                krakow.irc.pl  lublin.irc.pl                 |">>,
	<<">-------------------------------------------------------------<">>,
	<<"|   To jest EKSPERYMENTALNY serwer IRC oparty na Erlangu!     |">>,
	<<"|   W kazdej chwili mozesz byc rozlaczony bez ostrzezenia!    |">>,
	<<">-------------------------------------------------------------<">>,
	<<"|   Administrator i glowny developer: Witold Baryluk          |">>,
	<<">-------------------------------------------------------------<">>,
	<<"|   To jest EKSPERYMENTALNY serwer IRC oparty na Erlangu!     |">>,
	<<"| Admin zastrzega sobie prawo do usuniecia z serwera (K:line) |">>,
	<<"| dowolnej sesji bez podania przyczyn. Jesli Ci sie to bardzo |">>,
	<<"| nie podoba, to mozesz zmienic serwer, z ktorego korzystasz. |">>,
	<<"\\-------------------------------------------------------------/">>
	].

msg_motd(_State=#client_state{nick=Nick}) ->
	[
	output(375, Nick, [<<":-">>, fqdn(), <<" Message of the Day -">>], []),
	lists:map(fun(Line) ->
			output(372, Nick, [<<":-">>, Line], [])
		end,
	motd_text()),
	output(376, Nick, [], [<<"End of MOTD command.">>])
	].


msg_after_login(State) ->
	[
	msg_welcome(State),
	msg_hostandversion(State),
	msg_uptime(State),
	msg_support0(State),
	msg_support(State),
	msg_unique_id(State),
	msg_stats_general(State),
	msg_stats_operators(State),
	msg_stats_unknown(State),
	msg_stats_channels(State),
	msg_local0(State),
	msg_local(State),
	msg_global(State),
	msg_motd(State)
	].


