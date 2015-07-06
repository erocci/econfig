REBAR_ROOT_DIR ?= .
REBAR_BUILD_DIR ?= _build/default

REBAR = $(shell which rebar3 || $(REBAR_ROOT_DIR)/rebar3)

all: escriptize

escriptize:
	$(REBAR) escriptize

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

.PHONY: all compile clean
