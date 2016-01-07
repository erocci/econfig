#
# plugin for using econfig in erlang.mk projects
#
# Author: Jean Parpaillon <jean.parpaillon@free.fr>
# Copyright: 2016
#
ECONFIG = $(DEPS_DIR)/econfig/econfig
export ECONFIG

ECONFIG_OPTS ?= 

conf_verbose_0 = @echo " CONFIG "$(PROJECT);
conf_verbose_2 = set -x;
conf_verbose = $(conf_verbose_$(V))

all:: configure

configure: Econfig
	$(conf_verbose) $(ECONFIG) $(ECONFIG_OPTS) configure $(PROJECT):.

.PHONY: configure
