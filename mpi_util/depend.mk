# This file is generated automatically by fort_depend.py. DO NOT EDIT!
#
#  Created by: jka
#  Date: 2019-12-07 15:09:48
#

fll_mpi_proc_struct.o :  \
	../data_util/fll_mods.o

fll_mpi_write_snm.o :  \
	fll_mpi_cp.o \
	../data_util/fll_mods.o

fll_mpi_cp.o :  \
	../data_util/fll_out.o \
	../data_util/fll_mk.o \
	../data_util/fll_type.o \
	../data_util/fll_mv.o

fll_mpi_sum.o :  \
	../data_util/fll_mods.o

fll_mpi_read.o :  \
	fll_mpi_cp_all.o \
	../data_util/fll_mods.o

fll_mpi_mods.o :  \
	fll_mpi_read.o \
	fll_mpi_proc_struct.o \
	fll_mpi_cp_all.o \
	fll_mpi_write_snm.o \
	fll_mpi_write_nm.o \
	fll_mpi_cp.o \
	fll_mpi_sum.o \
	fll_mpi_write.o \
	fll_mpi_mv.o

fll_mpi_write_nm.o :  \
	../data_util/fll_mods.o \
	fll_mpi_write.o

fll_mpi_write.o :  \
	fll_mpi_sum.o \
	../data_util/fll_mods.o

fll_mpi_mv.o :  \
	fll_mpi_cp.o \
	../data_util/fll_rm.o \
	../data_util/fll_type.o

fll_mpi_cp_all.o :  \
	../data_util/fll_out.o \
	../data_util/fll_mk.o \
	../data_util/fll_type.o \
	../data_util/fll_mv.o
