## Process this file with automake to generate Makefile.in
#
#   Copyright (C) 2021-2025 Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I . -I .. -I ../.. 

bin_SCRIPTS   = gprofng-display-html
CLEANFILES    = $(bin_SCRIPTS)

do_subst = sed -e 's/BINUTILS_VERSION/$(VERSION)/'

gprofng-display-html: $(srcdir)/gp-display-html.in
	$(do_subst) < $(srcdir)/gp-display-html.in > $@
	chmod +x $@
