#
# plugin for using econfig in erlang.mk projects
#
# Author: Jean Parpaillon <jean.parpaillon@free.fr>
# Copyright: 2016
#
ECONFIG = $(DEPS_DIR)/econfig/econfig
export ECONFIG

ECONFIG_GLOBAL_OPTS ?= 

conf_verbose_0 = @echo " CONFIG "$(PROJECT);
conf_verbose_2 = set -x;
conf_verbose = $(conf_verbose_$(V))

define econfig_value
$(shell $(ECONFIG) $(ECONFIG_GLOBAL_OPTS) print -o value $(1))
endef

define econfig_compile
$(shell $(ECONFIG) $(ECONFIG_GLOBAL_OPTS) compile $(1))
endef

econfig: Econfig
	$(conf_verbose) $(ECONFIG) $(ECONFIG_GLOBAL_OPTS) configure $(ECONFIG_CONFIGURE_OPTS) $(PROJECT):.

.PHONY: econfig
