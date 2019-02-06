!
!     Copyright (C) 2018  Adam Jirasek
! 
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU Lesser General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
! 
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU Lesser General Public License for more details.
! 
!     You should have received a copy of the GNU Lesser General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.
!     
!     contact: libm3l@gmail.com
! 
!

!
!     Sample program
!
!     Date: 2018-11-07
! 
! 
!
!
!     Description: export interfaces 
!
!
!     Input parameters:
! 
!
!     Return value:
! 
! 
!
!     Modifications:
!     Date		Version		Patch number		CLA 
!
!
!     Description
!
!
 PROGRAM  FLL_CONVERT

    USE EXPORT_INTERFACES_M
    USE FLL_MODS_M
    IMPLICIT NONE
!
! Description: conversion utility
!
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
!
!
! External Modules used
!
   character(len=file_name_length) :: file,outfile,filebc,fileintf
   type(dnode), pointer  :: pfll,pinterf
   type(func_data_set) :: fpar
   character :: fmt, endian,ofmt
   character(len=20) :: fformat
!
!  read a ugrid file and save it in fll format
!
    read(*,'(a1024)')file     ! mesh file
    read(*,*)fformat    ! mesh file format - fll or ugrid
    read(*,'(a1024)')filebc   ! if ugrid, get boundary condition file
    read(*,'(a1024)')fileintf ! name of interface file
    read(*,*)fmt              ! mesh file format - a (ascii) or b (binary)
    read(*,'(a1024)')outfile  ! root name of output files
    read(*,'(a1)')ofmt        ! format of output_files
    read(*,'(a1)')endian      ! of ugrid bindary file, what endian

    if(trim(fformat) == 'ugrid' .or. trim(fformat) == 'ugrid')then
!
!  read ugrid files
!
      pfll  => fll_read_ugrid(file,fmt,endian,filebc)
    else
!
!  read fll file
!
      pfll => fll_read(trim(file),8,fmt,fpar)
    end if
!
!  read interface file, they are mostly ascii
!
    pinterf => fll_read(fileintf,8,'a',fpar)
    if(.not.associated(pinterf)) then
      pinterf => fll_read(fileintf,8,'b',fpar)
      if(.not.associated(pinterf))then
       write(*,*)' Error reading interface file'
       stop
      end if
   end if
!
!  export interfaces
!
    call export_interfaces(pfll,pinterf,ofmt,outfile)

    call fll_rm(pfll,fpar)
    call fll_rm(pinterf,fpar)

  
END PROGRAM 
