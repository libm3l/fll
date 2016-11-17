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
 PROGRAM  FLL_CATU

    USE FLL_MODS_M
    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER :: FMT, SCAN
!
!  read a file and print on screen
!
    READ(*,*)FILE
    READ(*,*)FMT 
    READ(*,*)SCAN 
  
    PNODE => FLL_READ(FILE,8,FMT,FPAR,SCAN = SCAN)
!
!  print node on the screen
!
   CALL FLL_CAT(PNODE, 6, .TRUE.,FPAR,SCAN = SCAN)
   WRITE(*,*)
   CALL FLL_RM(PNODE,FPAR)

  
END PROGRAM 
