# Source root.
MARK6_ROOT=/opt/src/mark6/src/mark6
MARK6_APP=${MARK6_ROOT}/rel/mark6/bin/mark6

# OTP directories (not used).
OTP_SRC=otp_src_R14B02
OTP_DIR=${MARK6_ROOT}/lib/mark6/priv/

# Ubuntu pre-requisite packages.
# PREQ="ncurses-dev libssl-dev build-essential git-core"

# Erlang applications.


# Build tool.
REBAR=./rebar
TAR=/bin/tar
RM=/bin/rm
CP=/bin/cp
MKDIR=/bin/mkdir
CHMOD=/bin/chmod

all:	clean generate

run:
	${MARK6_APP} console

# preq:
	# sudo /usr/bin/apt-get -y install ${PREQ}

node:
	${RM} -fr rel
	${MKDIR} rel
	cd rel; ../rebar create-node nodeid=mark6 force=1

compile:
	${REBAR} compile

generate:	compile node
	${CP} src/mark6.app.src ebin/mark6.app
	${CP} src/reltool.config.src rel/reltool.config
	${RM} -fr rel/mark6
	${REBAR} generate force=1
	${CHMOD} u+x ${MARK6_APP}

tests:
	${REBAR} eunit

clean:
	${REBAR} clean
	${RM} -fr rel
	${RM} -fr files
	${RM} -fr reltool.config
	${RM} -fr lib/mark6/files
	${RM} -fr lib/mark6/reltool.config
	${RM} -fr lib/client/files
	${RM} -fr lib/client/reltool.config
