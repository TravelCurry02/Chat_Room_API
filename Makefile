REBAR = $(shell which rebar3)

.PHONY: all check_rebar compile clean distclean dialyzer xref shell doc

all: check_rebar compile

compile: check_rebar
	$(REBAR) compile

clean: check_rebar
	rm -rf ebin/* logs log
	$(REBAR) clean

distclean: clean
	$(REBAR) clean --all
	rm -rf _build logs log doc *.dump *_plt *.crashdump priv

dialyzer: check_rebar
	$(REBAR) dialyzer

xref: check_rebar
	$(REBAR) xref

shell: check_rebar
	$(REBAR) shell

docs: check_rebar
	$(REBAR) ex_doc

check_rebar:
ifeq ($(REBAR),)
ifeq ($(wildcard rebar3),)
	$(call get_rebar)
else
	$(eval REBAR=./rebar3)
endif
endif

define get_rebar
	curl -O https://s3.amazonaws.com/rebar3/rebar3
	chmod a+x rebar3
	./rebar3 update
	$(eval REBAR=./rebar3)
endef
