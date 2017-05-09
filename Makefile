REBAR=rebar3

.PHONY: compile shell-schema release dialyzer

compile:
	$(REBAR) compile

## Rebar3 advertises that its shell command boots the system with a
## changed path:
## 
## Start a shell with project and deps preloaded similar to
## 'erl -pa ebin -pa deps/*/ebin'.
##
## It doesn't. It also starts the applications. We don't want
## the applications started, hence this little blurb:
shell-schema:
	erl -pa `$(REBAR) path` --name sw@127.0.0.1

release:
	$(REBAR) release

dialyzer:
	$(REBAR) dialyzer

documentation:
	asciidoc -d book doc/book.asciidoc
