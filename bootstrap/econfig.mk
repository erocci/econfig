# Automated update.

ECONFIG_BUILD_DIR ?= .econfig.build

python.mk: bootstrap
	git clone https://github.com/erocci/econfig $(ECONFIG_BUILD_DIR)
	cd $(ECONFIG_BUILD_DIR) && $(MAKE)
	cp $(ECONFIG_BUILD_DIR)/econfig.mk ./econfig.mk
	cp $(ECONFIG_BUILD_DIR)/econfig ./econfig
	rm -rf $(ECONFIG_BUILD_DIR)

.PHONY: bootstrap
bootstrap: ;
