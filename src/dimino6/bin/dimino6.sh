#!/bin/bash

## This file is part of dimino6.
##
## dimino6 is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## dimino6 is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with dimino6.  If not, see <http://www.gnu.org/licenses/>.

export INSTALL_ROOT=/opt/dimino6
export PYTHONPATH=${INSTALL_ROOT}/lib/python2.6/site-packages/:${PYTHONPATH}
export SITE_PACKAGES=${INSTALL_ROOT}/lib/python2.6/site-packages
export EXEC=${SITE_PACKAGES}/d6/Dimino6.py

PYTHON=`which python`
${PYTHON} ${EXEC} $*

