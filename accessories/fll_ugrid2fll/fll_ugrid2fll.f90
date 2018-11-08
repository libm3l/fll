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
 PROGRAM  FLL_CONVERT

    USE READUGRID_M
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
   CHARACTER(LEN=FILE_NAME_LENGTH) :: FILE,OUTFILE,FILEBC
   TYPE(DNODE), POINTER  :: PGLOB,PFLL
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER :: FMT, ENDIAN, EFMT,OFMT
   LOGICAL :: OK
!
!  read a ugrid file and save it in fll format
!
    READ(*,'(A1024)')FILE
    READ(*,'(A1024)')FILEBC
    READ(*,*)FMT 
    READ(*,'(A1024)')OUTFILE
    READ(*,'(A1)')OFMT
    READ(*,'(A1)')ENDIAN

    PGLOB  => FLL_MKDIR('ugrid_data', FPAR,ERRMSG='ALL')
    PFLL  => FLL_MKDIR('ugrid_data', FPAR,ERRMSG='ALL')

    CALL READUGRID(PGLOB, PFLL, FILE, FILEBC,FMT,ENDIAN)

    OK = FLL_WRITE(PFLL,OUTFILE,9,OFMT,FPAR)
    
   CALL FLL_RM(PGLOB,FPAR)
   CALL FLL_RM(PFLL,FPAR)

  
END PROGRAM 
