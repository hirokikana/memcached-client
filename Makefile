APP=memcached
LIBS=$(ERL_LIBS):deps
DIALYZER_OPTS=-Werror_handling -Wrace_conditions -Wunmatched_returns

compile:
	rebar compile

start: compile
	@ERL_LIBS=$(LIBS) erl -pa ebin -eval 'application:ensure_started(${APP}).'

eunit: compile
	rebar eunit skip_deps=true

init:
	rebar get-deps

.dialyzer.plt:
	touch .dialyzer.plt
	dialyzer --build_plt --plt .dialyzer.plt --apps erts kernel stdlib

dialyze: compile .dialyzer.plt
	dialyzer --plt .dialyzer.plt -r ebin $(DIALYZER_OPTS)

clean:
	rebar clean
