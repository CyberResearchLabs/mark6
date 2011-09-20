include include.mk

# Source root.
VERSION=0.1
PKGNAME=tstmark6
APPNAME=tstmark6
REBAR=./rebar


# Ubuntu pre-requisite packages.
# PREQ="ncurses-dev libssl-dev build-essential git-core"

# Erlang applications.
MODULES=$(shell ls -1 src/*.erl | awk -F[/.] '{ print $$2 }' | sed '$$q;s/$$/,/g')
MAKETIME=$(shell date)

all:	app
	(cd src; $(MAKE))

app:	ebin/$(PKGNAME).app

ebin/$(PKGNAME).app:	src/$(PKGNAME).app.src
	$(MKDIR) -p ebin
	sed -e 's/modules, \[\]/{modules, [$(MODULES)]}/;s/%MAKETIME%/$(MAKETIME)/' < $< > $@

clean:
	(cd src; $(MAKE) clean)
	$(RM) -rf .eunit
	$(RM) -rf ebin
	$(RM) -f src/erl_crash.dump
	$(RM) -f erl_crash.dump
	$(RM) -f $(PKGNAME)-$(VERSION).tar.gz

package:	all
	@mkdir $(PKGNAME)-$(VERSION)/ && $(CP) -rf ebin include include.mk Makefile README src $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true $(TAR) zcf $(PKGNAME)-$(VERSION).tar.gz $(PKGNAME)-$(VERSION)
	@$(RM) -rf $(PKGNAME)-$(VERSION)

install:	compile
	@for i in ebin/*.beam ebin/*.app; do install -m 644 -D $$i /opt/mit/mark6/lib/$(PKGNAME)-$(VERSION)/$$i ; done

compile:
	${REBAR} compile

test:	all
	${REBAR} eunit


EMPTY_AUTOMAKE_TARGETS = dvi pdf ps info html tags ctags
.PHONY:	$(EMPTY_AUTOMAKE_TARGETS)
$(EMPTY_AUTOMAKE_TARGETS):

