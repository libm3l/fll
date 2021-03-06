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
! Description: cat file
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
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER :: FMT, SCAN, DIR, COLOR
   CHARACTER(LEN=3) :: EFMT
!
!  read a file and print on screen
!
    READ(*,'(A1024)')FILE
    READ(*,*)FMT 
    READ(*,'(A3)')EFMT 
    READ(*,*)SCAN 
    READ(*,*)DIR 
    READ(*,*)COLOR 

    SELECT CASE(EFMT)
     CASE('fll')
      PNODE => FLL_READ(FILE,8,FMT,FPAR,SCAN = SCAN)
     CASE('ffa')
      PNODE => FLL_READ_FFA(FILE,8,FMT,FPAR,SCAN = SCAN)
    END SELECT
!
!  print node on the screen
!
   IF(DIR == 'Y')THEN
     CALL FLL_CAT(PNODE, 6, .TRUE.,FPAR,SCAN = SCAN, SDIR = DIR, COLOR=COLOR)
   ELSE
     CALL FLL_CAT(PNODE, 6, .TRUE.,FPAR,SCAN = SCAN,COLOR=COLOR)
   END IF
   WRITE(*,*)
   CALL FLL_RM(PNODE,FPAR)

  
END PROGRAM 
