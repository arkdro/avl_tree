REBAR=./rebar

all: deps compile_all

compile_all:
	$(REBAR) compile

compile:
	$(REBAR) compile skip_deps=true

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit skip_deps=true

shell:
	erl -pz ebin

xref: compile
	$(REBAR) xref skip_deps=true

ct: compile
	$(REBAR) ct skip_deps=true

dia:
	dialyzer --src -r src

.PHONY: all compile_all compile deps clean test shell xref ct
