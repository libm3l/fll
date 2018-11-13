!
!     Copyright (C) 2016  Adam Jirasek
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
!     Date: 2016-10-10
! 
! 
!
!
!     Description: prints file on screen
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
 program  fll_ffa2fll

    use fll_mods_m
    use ffa2fll_m
    implicit none
!
! description: conversion utility
!
! 
! history:
! version   date       patch number  cla     comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         initial implementation
!
!
! external modules used
!
   character(len=file_name_length) file,outfile
   type(dnode), pointer  :: pnode
   type(func_data_set) :: fpar
   character :: fmt, fmto
   logical :: ok
!
!  read a file and save it
!
   read(*,'(a1024)')file
   read(*,*)fmt 
   read(*,'(a1024)')outfile
   read(*,*)fmto  
!
!  read ffa mesh
!
   pnode => fll_read_ffa(file,8,fmt,fpar)
!
!  convert some things between fll and ffa
!
   call ffa2fll(pnode)
!
!  save mesh
!
   ok = fll_write(pnode,outfile,9,fmto,fpar) 
    
   call fll_rm(pnode,fpar)

  
end program 
