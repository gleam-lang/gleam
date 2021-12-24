ifndef GLEAM_NO_COMPILE
GLEAM_INSTALLED := $(shell command -v gleam)
endif

ERLANG_FILES = $(wildcard {src,build}/*.erl)
GLEAM_FILES = $(wildcard {src,build}/**/*.gleam)

.PHONY: ebin check

ifdef GLEAM_INSTALLED
ebin: check $(GLEAM_FILES) $(ERLANG_FILES)
	gleam compile-package --target erlang
else
ebin: check $(ERLANG_FILES)
	@mkdir -p ./ebin
	@cp build/*.app ebin/
	@erlc -server -o ebin $(ERLANG_FILES) || (rm -rf ebin && false)
endif

check:
ifndef ERL_LIBS
	$(error "ERL_LIBS environment variable not set")
endif
