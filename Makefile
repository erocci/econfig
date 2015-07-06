REBAR_ROOT_DIR ?= .
REBAR_BUILD_DIR ?= _build/default

REBAR = $(shell which rebar3 || $(REBAR_ROOT_DIR)/rebar3)

all: escriptize

escriptize:
	$(REBAR) escriptize
	ln -fs $(REBAR_BUILD_DIR)/bin/econfig econfig

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

.PHONY: all compile clean
