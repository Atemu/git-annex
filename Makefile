CFLAGS=-Wall
GIT_ANNEX_TMP_BUILD_DIR?=tmp
IGNORE=-ignore-package monads-fd -ignore-package monads-tf
BASEFLAGS=-threaded -Wall $(IGNORE) -outputdir $(GIT_ANNEX_TMP_BUILD_DIR) -IUtility

# If you get build failures due to missing haskell libraries,
# you can turn off some of these features.
FEATURES=-DWITH_ASSISTANT -DWITH_S3 -DWITH_WEBAPP -DWITH_OLD_YESOD -DWITH_MULTICAST

bins=git-annex
mans=git-annex.1 git-annex-shell.1
sources=Build/SysConfig.hs Utility/Touch.hs Utility/Mounts.hs
all=$(bins) $(mans) docs

OS:=$(shell uname | sed 's/[-_].*//')
ifeq ($(OS),Linux)
OPTFLAGS=-DWITH_INOTIFY -DWITH_DBUS
clibs=Utility/libdiskfree.o Utility/libmounts.o
else
# BSD system
OPTFLAGS=-DWITH_KQUEUE
clibs=Utility/libdiskfree.o Utility/libmounts.o Utility/libkqueue.o
ifeq ($(OS),Darwin)
OPTFLAGS=-DWITH_KQUEUE -DOSX
# Ensure OSX compiler builds for 32 bit when using 32 bit ghc
GHCARCH:=$(shell ghc -e 'print System.Info.arch')
ifeq ($(GHCARCH),i386)
CFLAGS=-Wall -m32
endif
endif
endif

PREFIX=/usr
GHCFLAGS=-O2 $(BASEFLAGS) $(FEATURES) $(OPTFLAGS)

ifdef PROFILE
GHCFLAGS=-prof -auto-all -rtsopts -caf-all -fforce-recomp $(BASEFLAGS) $(FEATURES) $(OPTFLAGS)
endif

GHCMAKE=ghc $(GHCFLAGS) --make

# Am I typing :make in vim? Do a fast build.
ifdef VIM
all=fast
endif

build: $(all)
	touch build-stamp

sources: $(sources)

# Disables optimisation. Not for production use.
fast: GHCFLAGS=$(BASEFLAGS) $(FEATURES) $(OPTFLAGS)
fast: $(bins)

Build/SysConfig.hs: configure.hs Build/TestConfig.hs Build/Configure.hs
	$(GHCMAKE) configure
	./configure

%.hs: %.hsc
	hsc2hs $<

git-annex: $(sources) $(clibs)
	install -d $(GIT_ANNEX_TMP_BUILD_DIR)
	$(GHCMAKE) $@ -o $(GIT_ANNEX_TMP_BUILD_DIR)/git-annex $(clibs)
	ln -sf $(GIT_ANNEX_TMP_BUILD_DIR)/git-annex git-annex

git-annex.1: doc/git-annex.mdwn
	./Build/mdwn2man git-annex 1 doc/git-annex.mdwn > git-annex.1
git-annex-shell.1: doc/git-annex-shell.mdwn
	./Build/mdwn2man git-annex-shell 1 doc/git-annex-shell.mdwn > git-annex-shell.1
git-union-merge.1: doc/git-union-merge.mdwn
	./Build/mdwn2man git-union-merge 1 doc/git-union-merge.mdwn > git-union-merge.1

install-mans: $(mans)
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 0644 $(mans) $(DESTDIR)$(PREFIX)/share/man/man1

install-docs: docs install-mans
	install -d $(DESTDIR)$(PREFIX)/share/doc/git-annex
	if [ -d html ]; then \
		rsync -a --delete html/ $(DESTDIR)$(PREFIX)/share/doc/git-annex/html/; \
	fi

install: build-stamp install-docs
	install -d $(DESTDIR)$(PREFIX)/bin
	install $(bins) $(DESTDIR)$(PREFIX)/bin
	ln -sf git-annex $(DESTDIR)$(PREFIX)/bin/git-annex-shell
	runghc Build/InstallDesktopFile.hs $(PREFIX)/bin/git-annex || true

test: $(sources) $(clibs)
	@if ! $(GHCMAKE) -O0 test $(clibs); then \
		echo "** failed to build the test suite" >&2; \
		exit 1; \
	elif ! ./test; then \
		echo "** test suite failed!" >&2; \
		exit 1; \
	fi

testcoverage:
	rm -f test.tix test
	ghc $(GHCFLAGS) -outputdir $(GIT_ANNEX_TMP_BUILD_DIR)/testcoverage --make -fhpc test
	./test
	@echo ""
	@hpc report test --exclude=Main --exclude=QC
	@hpc markup test --exclude=Main --exclude=QC --destdir=.hpc >/dev/null
	@echo "(See .hpc/ for test coverage details.)"

# If ikiwiki is available, build static html docs suitable for being
# shipped in the software package.
ifeq ($(shell which ikiwiki),)
IKIWIKI=@echo "** ikiwiki not found, skipping building docs" >&2; true
else
IKIWIKI=ikiwiki
endif

docs: $(mans)
	$(IKIWIKI) doc html -v --wikiname git-annex --plugin=goodstuff \
		--no-usedirs --disable-plugin=openid --plugin=sidebar \
		--underlaydir=/dev/null --disable-plugin=shortcut \
		--disable-plugin=smiley \
		--plugin=comments --set comments_pagespec="*" \
		--exclude='news/.*'

clean:
	rm -rf $(GIT_ANNEX_TMP_BUILD_DIR) $(bins) $(mans) test configure  *.tix .hpc $(sources) \
		doc/.ikiwiki html dist $(clibs) build-stamp

sdist: clean $(mans)
	./Build/make-sdist.sh

# Upload to hackage.
hackage: sdist
	@cabal upload dist/*.tar.gz

.PHONY: $(bins) test install
