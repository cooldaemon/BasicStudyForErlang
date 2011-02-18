REBAR=./rebar

all:
	@$(REBAR) get-deps compile

generate:
	@rm -rf ./rel/kvs
	@$(REBAR) generate

edoc:
	@$(REBAR) doc

ct:
	@$(REBAR) ct

build_plt:
	@$(REBAR) build_plt

dialyze:
	@$(REBAR) dialyze

typer:
	@typer -I ./include -I ./deps/tcp_server/include -r ./src --plt ~/.kvs_dialyzer_plt

clean:
	@$(REBAR) clean

