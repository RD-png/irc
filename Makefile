REBAR=rebar3
ERL=erl

compile:
	${REBAR} compile

dev:
	${REBAR} shell --config config/sys.config

deps:
	${REBAR} tree

dialyzer:
	${REBAR} dialyzer
