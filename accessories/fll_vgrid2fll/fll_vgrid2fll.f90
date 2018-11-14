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
    use fll_read_vgrid_m
    use vgrid2fll_m
    use merge_boundaries_m

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
   character(len=file_name_length) outfile,filemapbc,filbc,filecogsg
   type(dnode), pointer  :: pnode
   type(func_data_set) :: fpar
   character :: fmto
   logical :: ok
!
!  read a file and save it
!
   read(*,'(a1024)')filecogsg
   read(*,'(a1024)')filemapbc
   read(*,'(a1024)')filbc
   read(*,'(a1024)')outfile
   read(*,*)fmto  
!
!  read vgrid mesh
!
   pnode => fll_mkdir('pglob', fpar)

   call fll_read_vgrid_mapbc(pnode,filemapbc)
   call fll_read_vgrid_bc(pnode,filbc)
   call fll_read_vgrid_cogsg(pnode, filecogsg)
!
!  merge boundaries
!
   call merge_boundaries(pnode)
!
!  convert some things between fll and ffa
!
   call vgrid2fll(pnode)
!
!  save mesh
!
   ok = fll_write(pnode,outfile,9,fmto,fpar) 
    
   call fll_rm(pnode,fpar)

  
end program 
