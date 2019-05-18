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
    use sol2ensight_m
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
   character(len=file_name_length) :: file,outfile,filebc,solname
   type(dnode), pointer  :: pglob,psol
   type(func_data_set) :: fpar
   character :: endian,bconly,solution
   character(len=20) :: fformat,sformat
   logical :: vol,ok
!
!  read a ugrid file and save it in fll format
!
    read(*,'(a1024)')file            ! mesh file
    read(*,*)fformat                  ! mesh file format - fll or ugrid
    read(*,*)sformat                  ! solution file format - fll or ugrid
    read(*,'(a1024)')outfile       ! root name of output files
    read(*,*)bconly                   ! if ugrid, specify endian
    read(*,*)solution                 ! if y, there is solution too
    read(*,'(a1024)')solname   ! name of solution
    
    if(bconly == 'y')then
      vol = .false.
    else
      vol = .true.
    end if
!
!  read fll file
!
    pglob => fll_read(trim(file),8,fformat,fpar)
!
!  export mesh for ensigth format
!
    call grid2ensight(pglob, outfile, vol)
!
!  if solution - write solution
!
    if(solution == 'y')then
      write(*,*)'Converting solution'
      psol => fll_read(trim(solname),8,sformat,fpar)
      psol%lname='solution'
      ok = fll_mv(psol, pglob, fpar)
      call sol2ensight(pglob, outfile,vol)
    end if
!
!   free memory
!
    call fll_rm(pglob,fpar)
  
end program fll2ensight
