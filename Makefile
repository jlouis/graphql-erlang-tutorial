REBAR=rebar3

.PHONY: compile shell-schema release dialyzer

compile:
	$(REBAR) compile

shell-schema:
	$(REBAR) shell --name sw@127.0.0.1

release:
	$(REBAR) release

dialyzer:
	$(REBAR) dialyzer
