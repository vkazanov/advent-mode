.PHONY: test lint compile clean

EMACS ?= emacs
EL_FILES := $(shell find . -maxdepth 1 -name "advent*.el" -print | sort)

test: clean
	eask test ert-runner

lint: clean
	eask lint package
	eask lint checkdoc
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(EL_FILES)

compile: clean
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(EL_FILES)

clean:
	find . -name "*.elc" -o -name "*.eln" | xargs -r rm -f
