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
!     Date: 2018-11-12
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
 program  fll2ensight

    use fll_mods_m
    use grid2ensight_m
    implicit none
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
   character(len=file_name_length) :: file,outfile,filebc
   type(dnode), pointer  :: pglob
   type(func_data_set) :: fpar
   character :: fmt, endian,bconly
   character(len=20) :: fformat
   logical :: vol
!
!  read a ugrid file and save it in fll format
!
    read(*,'(a1024)')file     ! mesh file
    read(*,*)fformat           ! mesh file format - fll or ugrid
    read(*,'(a1024)')filebc    ! if ugrid, get boundary condition file
    read(*,*)fmt                    ! mesh file format - a (ascii) or b (binary)
    read(*,*)endian              ! if ugrid, specify endian
    read(*,'(a1024)')outfile   ! root name of output files
    read(*,*)bconly              ! if ugrid, specify endian
    
    if(bconly == 'y')then
      vol = .false.
    else
      vol = .true.
    end if

    if(trim(fformat) == 'ugrid' .or. trim(fformat) == 'ugrid')then
!
!  read ugrid files
!
      pglob => fll_read_ugrid(file, filebc,fmt,endian)
    else
!
!  read fll file
!
      pglob => fll_read(trim(file),8,fmt,fpar)
    end if
!
!  export mesh for ensigth format
!
    call grid2ensight(pglob, outfile, vol)
!
!   free memory
!
    call fll_rm(pglob,fpar)
  
end program fll2ensight
