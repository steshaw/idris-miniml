PROG := miniml
PKG := $(PROG).ipkg

.PHONY: all
all: build

.PHONY: build
build:
	idris --build $(PKG)

run: build
	rlwrap ./${PROG}

.PHONY: clean
clean:
	idris --clean $(PKG)

.PHONY: repl
repl:
	idris --repl $(PKG)

.PHONY: console
console:
	idris --repl $(PKG)

.PHONY: check
check:
	idris --checkpkg $(PKG)

.PHONY: doc
doc:
	idris --mkdoc $(PKG)
