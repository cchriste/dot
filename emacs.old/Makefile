#
# File:		Makefile
# Author:	Craig R. Steury
# Created:	April  8, 1997
# 
# Description:	
#               
# (c) Copyright 1997, Parametric Technology Corporation, all rights reserved.
#

# The default action is to build everything.
default: all install

# File containing emacs load-path info.
EMACS_LOAD_PATH_FILE  = load-path.el	

# Order is important here.  Moving this below SOURCES breaks things?
# We also need load-path defined first.
# 
include $(DSG_MAKERULES)

SOURCES	= all-systems.el		\
	  c++-style.el			\
	  convert-comment.el		\
	  group-init.el			\
	  group-init-other.el		\
	  find-grep.el			\
	  fi-patch.el			\
	  franz-init.el			\
	  fsf-mouse.el			\
	  gnats-support.el		\
	  grab-mail.el			\
	  header.el			\
	  ilisp-init.el			\
	  lisp-init.el			\
	  lisp-style.el			\
	  lisp-util.el			\
	  moccur.el			\
	  nt-init.el			\
	  patch.el			\
	  perl-style.el			\
	  rmime.el			\
	  walk-files.el


all: $(EMACS_OBJECTS)

install:
