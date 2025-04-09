HOSTNAME := $(shell hostname)

ifeq ($(LOCK), 1)
guix-lock := guix time-machine -C channels.lock --
else
guix-lock := guix
endif

.PHONY: list
list:
	@LC_ALL=C $(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | grep -E -v -e '^[^[:alnum:]]' -e '^$@$$'

install:
	guix pull -C channels.scm

update-channels:
	guix describe --format=channels > channels.lock

system:
	sudo $(guix-lock) system reconfigure system.scm

home:
	$(guix-lock) home reconfigure home.scm

gc:
	guix home delete-generations
	sudo guix system delete-generations
	guix pull --delete-generations
	guix gc
