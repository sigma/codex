EMACS ?= emacs

.PHONY: build test clean

build:
	keg build

test:
	keg exec $(EMACS) --batch -l codex-tests.el --eval "(in-codex codex-tests (ert:run-tests-batch-and-exit))"

clean:
	keg clean