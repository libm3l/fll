#
#     Copyright (C) 2016  Adam Jirasek
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU Lesser General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU Lesser General Public License for more details.
# 
#     You should have received a copy of the GNU Lesser General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
#     
#     contact: libm3l@gmail.com
# 
#
#
# Description: Makefile 
#
# 
# History:
# Version   Date       Patch number  CLA     Comment
# -------   --------   --------      ---     -------
# 1.1       10/10/16                         Initial implementation
#
#
#

EXE    = fll_test

include src_dir_path.mk
include ../config.mk

DEP_FILE=depend.mk

MPIF = $(shell command -v mpif90)

#SUBDIRS= $(dir $(wildcard $(srcdir)/*))
SUBDIRS= \
	Simple_data_operation \
	Read_Record\

#ifneq ($(and $(MPI_FC), $(command -v mpif90)),)
ifneq ($(MPI_FC),)
ifneq ($(MPIF),)
   SUBDIRS+= Example_MPI-IO
endif
endif



###########################################################################

all: $(SUBDIRS:%=%.all)

include ../rules.mk

clean: $(SUBDIRS:%=%.clean)
	rm -f *.x *.o *.mod

depend: $(SUBDIRS:%=%.depend)

install: $(SUBDIRS:%=%.install)

test: $(SUBDIRS:%=%.test)

chk: $(SUBDIRS:%=%.chk)
