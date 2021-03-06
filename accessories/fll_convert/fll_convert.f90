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
 PROGRAM  FLL_CONVERT

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
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE,OUTFILE
   TYPE(DNODE), POINTER  :: PNODE,PTMP, PFIND
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER :: FMT, FMTO
   CHARACTER(LEN=3) :: EFMT,OFMT
   LOGICAL :: OK
!
!  read a file and save it
!
    READ(*,'(A1024)')FILE
    READ(*,*)FMT 
    READ(*,'(A3)')EFMT 
    READ(*,'(A1024)')OUTFILE
    READ(*,'(A3)')OFMT
    READ(*,*)FMTO  

    SELECT CASE(EFMT)
     CASE('ffa')
       PNODE => FLL_READ_FFA(FILE,8,FMT,FPAR)
       
          call fll_cat(PNODE, 6, .true., fpar)
       
          PFIND => null()

          DO WHILE(FLL_SWEEP(PNODE, PFIND,'*', '*', -1_LINT,  FPAR))
            WRITE(*,*)'Name of node is ',PFIND%LNAME
        !    write(*,*)'------------------------------------------------'
            
         !        call fll_cat(PFIND, 6, .true., fpar)

          !            write(*,*)'------------------------------------------------'
  
          END DO
!
!    convert all integer arrays to long integers
!
!       CALL I2LCONVERT(PGLOB)
     CASE DEFAULT
       PNODE => FLL_READ(FILE,8,FMT,FPAR)       
    END SELECT

    SELECT CASE(OFMT)
     CASE('ffa')
       OK = FLL_WRITE_FFA(PNODE, OUTFILE,9,FMTO,FPAR)
     CASE DEFAULT
       OK = FLL_WRITE(PNODE,OUTFILE,9,FMTO,FPAR)
    END SELECT    
    
    
   CALL FLL_RM(PNODE,FPAR)

  
END PROGRAM 
