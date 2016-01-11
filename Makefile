PROJECT = econfig
VERSION = 0.1

define localdep =
$(shell erl -noshell -eval "case application:ensure_all_started($1) of {ok, _} -> halt(0); _ -> halt(1) end." && echo ok || true)
endef

ALL_DEPS = getopt bbmustache
$(foreach dep,$(ALL_DEPS),$(if $(call localdep,$(dep)),$(eval LOCAL_DEPS+=$(dep)),$(eval DEPS+=$(dep))))

dep_getopt = git https://github.com/jcomellas/getopt.git v0.8.2
dep_bbmustache = git https://github.com/soranoba/bbmustache.git v1.0.4

ESCRIPT_EMU_ARGS = -smp auto -pa . -noshell -sasl errlog_type error -escript main econfig

COMPILE_FIRST = econfig_frontend econfig_cmd

VSN = $(shell $(CURDIR)/version.sh $(VERSION))

subst = sed -e 's|@VSN[@]|$(VSN)|g'

include erlang.mk

all:: escript

test-build:: escript

ebin/$(PROJECT).app:: src/$(PROJECT).app.src

src/$(PROJECT).app.src: src/$(PROJECT).app.src.in
	$(gen_verbose) $(subst) $< > $@

clean:: clean-local

clean-local:
	- rm -f $(PROJECT)

dist: $(PROJECT)-$(VSN).tar.xz

debian-dist: $(PROJECT)_$(VSN).orig.tar.xz

$(PROJECT)_$(VSN).orig.tar.xz: $(PROJECT)-$(VSN).tar.xz
	ln -s $< $@

$(PROJECT)-$(VSN).tar.xz:
	-rm -f src/$(PROJECT).app.src
	@$(MAKE) --no-print-directory src/$(PROJECT).app.src
	$(gen_verbose) git archive --prefix=$(PROJECT)-$(VSN)/ HEAD . | \
	  tar xf - && \
	  cp src/$(PROJECT).app.src $(PROJECT)-$(VSN)/src && \
	  tar cf - $(PROJECT)-$(VSN) | xz > $@ && \
	  rm -rf $(PROJECT)-$(VSN)

.PHONY: dist debian-dist
