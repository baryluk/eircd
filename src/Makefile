EBIN=../ebin

MODULES:=$(patsubst %.erl,%.beam,$(wildcard *.erl))
RELEASES:=$(patsubst %.rel,%.boot,$(wildcard *.rel))

all: $(MODULES)

%.beam: %.erl
	#erlc +debug_info +native +"{hipe, [o3]}" $<
	#erlc +native +"{hipe, [o3]}" $<
	erlc -v -W9 +debug_info $<

clean:
	rm -rf *.beam

%.boot: %.rel

ERL_TOP=/usr/lib/erlang

dial: all
	dialyzer --check_plt --plt eircd.plt -c .
	# \
	# -Wspecdiffs -Werror_handling -Wunmatched_returns
	#-Werror_handling -Wunmatched_returns -Wunderspecs -Woverspecs -Wspecdiffs

plt:
	dialyzer --add_to_plt --plt $(HOME)/.dialyzer_otp.plt --output_plt eircd.plt -c .

otp_plt:
	dialyzer --build_plt --output_plt $(HOME)/.dialyzer_otp.plt -r $(ERL_TOP)/lib/stdlib*/ebin \
		$(ERL_TOP)/lib/kernel*/ebin $(ERL_TOP)/lib/mnesia*/ebin $(ERL_TOP)/lib/ssl*/ebin \
		$(ERL_TOP)/lib/asn1*/ebin $(ERL_TOP)/lib/compiler*/ebin $(ERL_TOP)/lib/crypto*/ebin \
		$(ERL_TOP)/lib/syntax_tools*/ebin $(ERL_TOP)/lib/inets*/ebin $(ERL_TOP)/lib/sasl*/ebin \
		$(ERL_TOP)/lib/odbc*/ebin $(ERL_TOP)/lib/tools*/ebin $(ERL_TOP)/lib/runtime_tools*/ebin \
		$(ERL_TOP)/lib/hipe*/ebin $(ERL_TOP)/lib/edoc*/ebin $(ERL_TOP)/lib/webtool*/ebin \
		$(ERL_TOP)/lib/gs*/ebin $(ERL_TOP)/lib/xmerl*/ebin $(ERL_TOP)/lib/observer*/ebin \
		$(ERL_TOP)/lib/observer*/ebin $(ERL_TOP)/lib/et*/ebin
