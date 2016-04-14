.PHONY: rel deps test
APP      = manager
BASE_DIR = $(shell pwd)
# REBAR    = rebar
REBAR    = $(BASE_DIR)/rebar
DIST     = $(BASE_DIR)/rel/$(APP)


all: deps compile rel

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

rel: compile
	@cd rel && $(REBAR) generate -f
	$(REBAR) generate

xref:
	@$(REBAR) xref skip_deps=true

edoc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean

relclean:
	rm -fr rel/manager

distclean: clean relclean
	@$(REBAR) delete-deps
