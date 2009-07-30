-module(eircd_mask).
-author('baryluk@smp.if.uj.edu.pl').

-export([new/1, match/2]).

% split at ! and @,
% and possibly change into prefix/middle/postfix,
% change to lowercase
new(Mask) ->
	Mask.

match(Mask, Text) ->
	false.
