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
!     Subroutine FLL_OUT
!
!
MODULE FLL_OUT_M
  IMPLICIT NONE
!
! Description: Contains subroutine FLL_OUT
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

CONTAINS
  SUBROUTINE FLL_OUT(ACTION,FPAR)
!
! Description: Contains subroutine pritning errmessage from FPAR structure
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

   USE FLL_TYPE_M
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! ACTION          In       determines what to do
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
    IMPLICIT NONE
!
!     ARGUMENTS:
!
    CHARACTER(*) :: ACTION
    TYPE(FUNC_DATA_SET) :: FPAR
    CHARACTER(LEN=72) :: STRMES ='LOGFILE'
!
!   Local declarations
!
!
!  Select here to print
!
    SELECT CASE(ACTION)

    CASE('OPEN')
      OPEN(UNIT=IOLOGFILE,STATUS='UNKNOWN',FILE=STRMES,FORM='FORMATTED')

    CASE('OPEN_STAT')
      OPEN(UNIT=IOSTATFILE,STATUS='UNKNOWN',FILE=STRMES,FORM='FORMATTED')
      WRITE(IOSTATFILE,'(A)')'Process running ...'
      CLOSE(IOSTATFILE)

    CASE('CLOSE')
      CLOSE(IOLOGFILE)

    CASE('CLOSE_STAT')
      OPEN(UNIT=IOSTATFILE,STATUS='UNKNOWN',FILE=STRMES,FORM='FORMATTED')
      WRITE(IOSTATFILE,'(A)')'Finished'
      CLOSE(IOSTATFILE)

    CASE('LOG')
      WRITE(IOLOGFILE,'(A)')FPAR%MESG
      FLUSH(IOLOGFILE)

    CASE('OUT')
      WRITE(STDOUT,'(A)')FPAR%MESG
      FLUSH(STDOUT)

    CASE('ALL')
      WRITE(IOLOGFILE,'(A)')FPAR%MESG
      FLUSH(IOLOGFILE)
      WRITE(STDOUT,'(A)')FPAR%MESG
      FLUSH(STDOUT)

    CASE DEFAULT
      WRITE(STDOUT,*)&
           'Error: wrong action in fll_out, action: "'//ACTION//'" not defined.'
      WRITE(STDOUT,*)FPAR%MESG
      FLUSH(STDOUT)
      WRITE(IOLOGFILE,*)&
           'Error: wrong action in fll_out, action: "'//ACTION//'" not defined.'
      WRITE(IOLOGFILE,*)FPAR%MESG
      FLUSH(IOLOGFILE)

    END SELECT

    RETURN
!-------------------------------------END OF FLL_OUT -------------------
  END SUBROUTINE FLL_OUT
END MODULE FLL_OUT_M