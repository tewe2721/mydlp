###--------------------------------------------------------------------------
###    This file is part of Foobar.
###
###    Foobar is free software: you can redistribute it and/or modify
###    it under the terms of the GNU General Public License as published by
###    the Free Software Foundation, either version 3 of the License, or
###    (at your option) any later version.
###
###    Foobar is distributed in the hope that it will be useful,
###    but WITHOUT ANY WARRANTY; without even the implied warranty of
###    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
###    GNU General Public License for more details.
###
###    You should have received a copy of the GNU General Public License
###    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
###--------------------------------------------------------------------------

AC_INIT([mydlp], [1.0.0], [kerem@medratech.com])

AC_PREREQ([2.64])
AC_COPYRIGHT([Copyright (C) 2009 H. Kerem Cevahir])
AM_INIT_AUTOMAKE

AC_ERLANG_NEED_ERLC
AC_PREFIX_PROGRAM(erl)
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB([kernel])
AC_ERLANG_CHECK_LIB([stdlib])
AC_ERLANG_CHECK_LIB([edoc])
AC_ERLANG_CHECK_LIB([eunit])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([mydlp], [1.0.0])

AC_CONFIG_FILES([Makefile src/mydlp.app src/mydlp.rel src/overview.edoc
	src/Makefile])
AC_OUTPUT
