% RFC 1459

% Replies

% Errors

-define(ERR_NOSUCHNICK, 401).
% "<nickname> :No such nick/channel"
-define(ERR_NOSUCHSERVER, 402).
% "<server name> :No such server"
-define(ERR_NOSUCHCHANNEL, 403).
% "<channel name> :No such channel"
-define(ERR_CANNOTSENDTOCHAN, 404).
% "<channel name> :Cannot send to channel"
-define(ERR_TOOMANYCHANNELS, 405).
% "<channel name> :You have joined too many channels"
-define(ERR_WASNOSUCHNICK, 406).
% "<nickname> :There was no such nickname"
-define(ERR_TOOMANYTARGETS, 407).
% "<target> :Duplicate recipients. No message delivered"
-define(ERR_NOORIGIN, 409).
% ":No origin specified"
-define(ERR_NORECIPIENT, 411).
% ":No recipient given (<command>)"
-define(ERR_NOTEXTTOSEND, 412).
% ":No text to send"
-define(ERR_NOTOPLEVEL, 413).
% "<mask> :No toplevel domain specified"
-define(ERR_WILDTOPLEVEL, 414).
% "<mask> :Wildcard in toplevel domain"
-define(ERR_UNKNOWNCOMMAND, 421).
% "<command> :Unknown command"
-define(ERR_NOMOTD, 422).
% ":MOTD File is missing"
-define(ERR_NOADMININFO, 423).
% "<server> :No administrative info available"
-define(ERR_FILEERROR, 424).
% ":File error doing <file op> on <file>"
-define(ERR_NONICKNAMEGIVEN, 431).
% ":No nickname given"
-define(ERR_ERRONEUSNICKNAME, 432).
% "<nick> :Erroneus nickname"
-define(ERR_NICKNAMEINUSE, 433).
% "<nick> :Nickname is already in use"
-define(ERR_NICKCOLLISION, 436).
% "<nick> :Nickname collision KILL"
-define(ERR_USERNOTINCHANNEL, 441).
% "<nick> <channel> :They aren't on that channel"
-define(ERR_NOTONCHANNEL, 442).
% "<channel> :You're not on that channel"
-define(ERR_USERONCHANNEL, 443).
% "<user> <channel> :is already on channel"
-define(ERR_NOLOGIN, 444).
% "<user> :User not logged in"
-define(ERR_SUMMONDISABLED, 445).
% ":SUMMON has been disabled"
-define(ERR_USERSDISABLED, 446).
% ":USERS has been disabled"
-define(ERR_NOTREGISTERED, 451).
% ":You have not registered"
-define(ERR_NEEDMOREPARAMS, 461).
% "<command> :Not enough parameters"
-define(ERR_ALREADYREGISTRED, 462).
% ":You may not reregister"
-define(ERR_NOPERMFORHOST, 463).
% ":Your host isn't among the privileged"
-define(ERR_PASSWDMISMATCH, 464).
% ":Password incorrect"
-define(ERR_YOUREBANNEDCREEP, 465).
% ":You are banned from this server"
-define(ERR_KEYSET, 467).
% "<channel> :Channel key already set"
-define(ERR_CHANNELISFULL, 471).
% "<channel> :Cannot join channel (+l)"
-define(ERR_UNKNOWNMODE, 472).
% "<char> :is unknown mode char to me"
-define(ERR_INVITEONLYCHAN, 473).
% "<channel> :Cannot join channel (+i)"
-define(ERR_BANNEDFROMCHAN, 474).
% "<channel> :Cannot join channel (+b)"
-define(ERR_BADCHANNELKEY, 475).
% "<channel> :Cannot join channel (+k)"
-define(ERR_NOPRIVILEGES, 481).
% ":Permission Denied- You're not an IRC operator"
-define(ERR_CHANOPRIVSNEEDED, 482).
% "<channel> :You're not channel operator"
-define(ERR_CANTKILLSERVER, 483).
% ":You cant kill a server!"
-define(ERR_NOOPERHOST, 491).
% ":No O-lines for your host"

-define(ERR_UMODEUNKNOWNFLAG, 501).
% ":Unknown MODE flag"
-define(ERR_USERSDONTMATCH, 502).
% ":Cant change mode for other users"

% Commands response
-define(RPL_NONE, 300).
-define(RPL_USERHOST, 302).
% ":[<reply>{<space><reply>}]"
-define(RPL_ISON, 303).
% ":[<nick> {<space><nick>}]"
-define(RPL_AWAY, 301).
% "<nick> :<away message>"
-define(RPL_UNAWAY, 305).
% ":You are no longer marked as being away"
-define(RPL_NOWAWAY, 306).
% ":You have been marked as being away"
-define(RPL_WHOISUSER, 311).
% "<nick> <user> <host> * :<real name>"
-define(RPL_WHOISSERVER, 312).
% "<nick> <server> :<server info>"
-define(RPL_WHOISOPERATOR, 313).
% "<nick> :is an IRC operator"
-define(RPL_WHOISIDLE, 317).
% "<nick> <integer> :seconds idle"
-define(RPL_ENDOFWHOIS, 318).
% "<nick> :End of /WHOIS list"
-define(RPL_WHOISCHANNELS, 319).
% "<nick> :{[@|+]<channel><space>}"
-define(RPL_WHOWASUSER, 314).
% "<nick> <user> <host> * :<real name>"
-define(RPL_ENDOFWHOWAS, 369).
% "<nick> :End of WHOWAS"
-define(RPL_LISTSTART, 321).
% "Channel :Users Name"
-define(RPL_LIST, 322).
% "<channel> <# visible> :<topic>"
-define(RPL_LISTEND, 323).
% ":End of /LIST"
-define(RPL_CHANNELMODEIS, 324).
% "<channel> <mode> <mode params>"
-define(RPL_NOTOPIC, 331).
% "<channel> :No topic is set"
-define(RPL_TOPIC, 332).
% "<channel> :<topic>"
-define(RPL_INVITING, 341).
% "<channel> <nick>"
-define(RPL_SUMMONING, 342).
% "<user> :Summoning user to IRC"
-define(RPL_VERSION, 351).
% "<version>.<debuglevel> <server> :<comments>"
-define(RPL_WHOREPLY, 352).
% "<channel> <user> <host> <server> <nick> <H|G>[*][@|+] :<hopcount> <real name>"
-define(RPL_ENDOFWHO, 315).
% "<name> :End of /WHO list"
-define(RPL_NAMREPLY, 353).
% "<channel> :[[@|+]<nick> [[@|+]<nick> [...]]]"
-define(RPL_ENDOFNAMES, 366).
% "<channel> :End of /NAMES list"
-define(RPL_LINKS, 364).
% "<mask> <server> :<hopcount> <server info>"
-define(RPL_ENDOFLINKS, 365).
% "<mask> :End of /LINKS list"
-define(RPL_BANLIST, 367).
% "<channel> <banid>"
-define(RPL_ENDOFBANLIST, 368).
% "<channel> :End of channel ban list"
-define(RPL_INFO, 371).
% ":<string>"
-define(RPL_ENDOFINFO, 374).
% ":End of /INFO list"
-define(RPL_MOTDSTART, 375).
% ":- <server> Message of the day - "
-define(RPL_MOTD, 372).
% ":- <text>"
-define(RPL_ENDOFMOTD, 376).
% ":End of /MOTD command"
-define(RPL_YOUREOPER, 381).
% ":You are now an IRC operator"
-define(RPL_REHASHING, 382).
% "<config file> :Rehashing"
-define(RPL_TIME, 391).
% "<server> :<string showing server's local time>"
-define(RPL_USERSSTART, 392).
% ":UserID Terminal Host"
-define(RPL_USERS, 393).
% ":%-8s %-9s %-8s"
-define(RPL_ENDOFUSERS, 394).
% ":End of users"
-define(RPL_NOUSERS, 395).
% ":Nobody logged in"

-define(RPL_TRACELINK, 200).
% "Link <version & debug level> <destination> <next server>"
-define(RPL_TRACECONNECTING, 201).
% "Try. <class> <server>"
-define(RPL_TRACEHANDSHAKE, 202).
% "H.S. <class> <server>"
-define(RPL_TRACEUNKNOWN, 203).
% "???? <class> [<client IP address in dot form>]"
-define(RPL_TRACEOPERATOR, 204).
% "Oper <class> <nick>"
-define(RPL_TRACEUSER, 205).
% "User <class> <nick>"
-define(RPL_TRACESERVER, 206).
% "Serv <class> <int>S <int>C <server> <nick!user|*!*>@<host|server>"
-define(RPL_TRACENEWTYPE, 208).
% "<newtype> 0 <client name>"
-define(RPL_TRACELOG, 261).
% "File <logfile> <debug level>"
-define(RPL_STATSLINKINFO, 211).
% "<linkname> <sendq> <sent messages> <sent bytes> <received messages> <received bytes> <time open>"
-define(RPL_STATSCOMMANDS, 212).
% "<command> <count>"
-define(RPL_STATSCLINE, 213).
% "C <host> * <name> <port> <class>"
-define(RPL_STATSNLINE, 214).
% "N <host> * <name> <port> <class>"
-define(RPL_STATSILINE, 215).
% "I <host> * <host> <port> <class>"
-define(RPL_STATSKLINE, 216).
% "K <host> * <username> <port> <class>"
-define(RPL_STATSYLINE, 218).
% "Y <class> <ping frequency> <connect frequency> <max sendq>"
-define(RPL_ENDOFSTATS, 219).
% "<stats letter> :End of /STATS report"
-define(RPL_STATSLLINE, 241).
% "L <hostmask> * <servername> <maxdepth>"
-define(RPL_STATSUPTIME, 242).
% ":Server Up %d days %d:%02d:%02d"
-define(RPL_STATSOLINE, 243).
% "O <hostmask> * <name>"
-define(RPL_STATSHLINE, 244).
% "H <hostmask> * <servername>"
-define(RPL_UMODEIS, 221).
% "<user mode string>"
-define(RPL_LUSERCLIENT, 251).
% ":There are <integer> users and <integer> invisible on <integer> servers"
-define(RPL_LUSEROP, 252).
% "<integer> :operator(s) online"
-define(RPL_LUSERUNKNOWN, 253).
% "<integer> :unknown connection(s)"
-define(RPL_LUSERCHANNELS, 254).
% "<integer> :channels formed"
-define(RPL_LUSERME, 255).
% ":I have <integer> clients and <integer> servers"
-define(RPL_ADMINME, 256).
% "<server> :Administrative info"
-define(RPL_ADMINLOC1, 257).
% ":<admin info>"
-define(RPL_ADMINLOC2, 258).
% ":<admin info>"
-define(RPL_ADMINEMAIL, 259).
% ":<admin info>"

% Reserved
-define(RPL_TRACECLASS, 209).
-define(RPL_STATSQLINE, 217).
-define(RPL_SERVICEINFO, 231).
-define(RPL_ENDOFSERVICES, 232).
-define(RPL_SERVICE, 233).
-define(RPL_SERVLIST, 234).
-define(RPL_SERVLISTEND, 235).
-define(RPL_WHOISCHANOP, 316).
-define(RPL_KILLDONE, 361).
-define(RPL_CLOSING, 362).
-define(RPL_CLOSEEND, 363).
-define(RPL_INFOSTART, 373).
-define(RPL_MYPORTIS, 384).
-define(ERR_YOUWILLBEBANNED, 466).
-define(ERR_BADCHANMASK, 476).
-define(ERR_NOSERVICEHOST, 492).
