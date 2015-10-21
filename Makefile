.PHONY: rel deps test
APP      = manager
BASE_DIR = $(shell pwd)
# REBAR    = rebar
REBAR    = $(BASE_DIR)/rebar
DIST     = $(BASE_DIR)/rel/$(APP)

all: compile

compile: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

update-deps:
	@$(REBAR) update-deps

rel: compile
	# @cd rel && $(REBAR) generate -f
	$(REBAR) generate

edoc:
	@$(REBAR) doc