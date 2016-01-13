#
# Makefile for using econfig in Makefile based projects
#
# Author: Jean Parpaillon <jean.parpaillon@free.fr>
# Copyright: 2016
#
ECONFIG_MK_FILENAME := $(realpath $(lastword $(MAKEFILE_LIST)))

ECONFIG = $(dir $(ECONFIG_MK_FILENAME))/econfig
export ECONFIG

ECONFIG_GLOBAL_OPTS ?= 

# Verbosity.

V ?= 0

verbose_0 = @
verbose = $(verbose_$(V))

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

conf_verbose_0 = @echo " CONFIG "$(PROJECT);
conf_verbose_2 = set -x;
conf_verbose = $(conf_verbose_$(V))

ECONFIG_VALUE = $(ECONFIG) $(ECONFIG_GLOBAL_OPTS) print -o value
ECONFIG_COMPILE = $(ECONFIG) $(ECONFIG_GLOBAL_OPTS) compile
ECONFIG_RENDER = $(ECONFIG) $(ECONFIG_GLOBAL_OPTS) render
ECONFIG_CONFIGURE = $(ECONFIG) $(ECONFIG_GLOBAL_OPTS) configure $(ECONFIG_CONFIGURE_OPTS)
