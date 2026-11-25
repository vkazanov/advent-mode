.PHONY: test lint compile clean

EMACS ?= emacs
EL_FILES := $(shell find . -maxdepth 1 -name "advent*.el" -print | sort)
BYTE_COMPILE = $(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
	-f batch-byte-compile

test: clean
	eask test ert-runner

lint: clean
	eask lint package
	eask lint checkdoc
	$(BYTE_COMPILE) $(EL_FILES)

compile: clean
	$(BYTE_COMPILE) $(EL_FILES)

clean:
	find . -name "*.elc" -o -name "*.eln" | xargs -r rm -f
