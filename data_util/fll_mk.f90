!
!     Copyright (C) 2016  Adam Jirasek
! 
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU Lesser General Public License as published by
!     the Rree Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
! 
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or RITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU Lesser General Public License for more details.
! 
!     You should have received a copy of the GNU Lesser General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.
!     
!     contact: libm3l@gmail.com
! 
!
MODULE FLL_MK_M
!
! Description: creates node
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
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

INTERFACE FLL_MK
  MODULE PROCEDURE FLL_MK_12D,FLL_MK_3D,FLL_MK_4D
END INTERFACE FLL_MK

PUBLIC FLL_MK

CONTAINS
   FUNCTION FLL_MK_12D(NAME,LTYPE,NDIM,NSIZE,FPAR,ERRMSG,DIAGMESSG) RESULT(PNEW)
!
! Description: function creates node specified by name, type and dimensions
!
! External Modules used
! 
       USE FLL_TYPE_M
       USE FLL_OUT_M
       
       IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! NAME         In         name of node
! LTYPE        In         type of node  - can be *
! NDIM, NSIZE  In         node dimensions
! PNEW         Out        return pointer to newly created node
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
       TYPE(FUNC_DATA_SET)   :: FPAR
       TYPE(DNODE), POINTER  :: PNEW
       CHARACTER(*)  :: NAME
       CHARACTER(*)  :: LTYPE
       INTEGER(LINT) :: NDIM, NSIZE
       CHARACTER(*), OPTIONAL :: ERRMSG
       CHARACTER(*), OPTIONAL :: DIAGMESSG
!
! Local declarations
!       
       INTEGER :: ISTAT
       CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  local action
!
       IF(.NOT.PRESENT(ERRMSG))THEN
         LOC_ERRMSG='ALL'
       ELSE
         LOC_ERRMSG = ERRMSG
       END IF
       
       PNEW => NULL()
!
! Body
!
       IF(LEN_TRIM(LTYPE)<1.OR.LEN_TRIM(LTYPE)>TYPE_LENGTH) THEN
         WRITE(FPAR%MESG,'(A,A)')' Wrong type: ',TRIM(LTYPE)
         IF(PRESENT(DIAGMESSG))THEN
           FPAR%MESG = TRIM(FPAR%MESG)//' '//TRIM(DIAGMESSG)
         END IF
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
         FPAR%SUCCESS = .FALSE.
         RETURN
      END IF

      IF(LEN_TRIM(NAME)>NAME_LENGTH) THEN
        WRITE(FPAR%MESG,'(A,A)')' Wrong name: ',TRIM(NAME)
         IF(PRESENT(DIAGMESSG))THEN
           FPAR%MESG = TRIM(FPAR%MESG)//' '//TRIM(DIAGMESSG)
         END IF
         CALL FLL_OUT(LOC_ERRMSG,FPAR) 
        FPAR%SUCCESS = .FALSE.
        RETURN
      END IF

      IF(.NOT.ANY(LTYPE(1:1)==(/'C','S','I','L','R','D','N'/))) THEN
        WRITE(FPAR%MESG,'(A,A)')' Wrong type: ',TRIM(LTYPE)
         IF(PRESENT(DIAGMESSG))THEN
           FPAR%MESG = TRIM(FPAR%MESG)//' '//TRIM(DIAGMESSG)
         END IF
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
        FPAR%SUCCESS = .FALSE.
        RETURN
      END IF

      ALLOCATE(PNEW, STAT = ISTAT)
      IF(ISTAT /= 0)THEN
        WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:129 '
        WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
      
      PNEW%LNAME  = TRIM(NAME)
      PNEW%LTYPE = LTYPE
      PNEW%NDIM = 0
      PNEW%NSIZE = 0 
      
      PNEW%PPAR    =>NULL()
      PNEW%PCHILD  =>NULL()
      PNEW%PNEXT   =>NULL()
      PNEW%PPREV   =>NULL()
      PNEW%PLINK   =>NULL()      
      
      IF(TRIM(LTYPE) == 'DIR' .OR. TRIM(LTYPE) == 'N')THEN
        PNEW%NDIM = 0
        PNEW%NSIZE = 0
        RETURN
      ELSE
        PNEW%NDIM = NDIM
        PNEW%NSIZE = NSIZE
      END IF

      PNEW%PPAR => NULL()
      PNEW%PCHILD => NULL()
      PNEW%PPREV => NULL()
      PNEW%PNEXT => NULL()
      PNEW%PLINK => NULL()

      IF(NDIM < 1 .OR. NSIZE < 1)THEN
        WRITE(FPAR%MESG,'(A,A,I5,I5)')' Wrong dimensions for node ',TRIM(NAME), NDIM, NSIZE
         IF(PRESENT(DIAGMESSG))THEN
           FPAR%MESG = TRIM(FPAR%MESG)//' '//TRIM(DIAGMESSG)
         END IF
         CALL FLL_OUT(LOC_ERRMSG,FPAR) 
        FPAR%SUCCESS = .FALSE.
        RETURN
      END IF
!
!  ALLOCATE ARRAYS
!
     SELECT CASE(LTYPE)
     CASE('R')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%R1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)THEN
             WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:179 '
             WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%R1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)THEN
               WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:188 '
               WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
            STOP
      END IF
         ELSE
            ALLOCATE(PNEW%R2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)THEN
              WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:195 '
              WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
             STOP
      END IF
         END IF
       END IF

     CASE('R0')

     CASE('R1')
       IF(NDIM > 1 .AND. NSIZE >1)THEN
         WRITE(FPAR%MESG,'(A,A)')' fll_mk: R1 array - One dimension must be 1',TRIM(NAME)
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
         FPAR%SUCCESS = .false.
         RETURN
       END IF
       ALLOCATE(PNEW%R1(NDIM*NSIZE), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:205 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF  

     CASE('R2')
       ALLOCATE(PNEW%R2(NDIM,NSIZE), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:213 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF
       
     CASE('D')
            IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%D1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)THEN
              WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:223 '
              WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
      END IF
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%D1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)THEN
              WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:232 '
              WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
            STOP
      END IF
         ELSE
            ALLOCATE(PNEW%D2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)THEN
               WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:239 '
               WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
             STOP
      END IF
         END IF
       END IF

     CASE('D0')
     CASE('D1')
       IF(NDIM > 1 .AND. NSIZE >1)THEN
         WRITE(FPAR%MESG,'(A,A)')' fll_mk: D1 array - One dimension must be 1',TRIM(NAME)
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
         FPAR%SUCCESS = .false.
         RETURN
       END IF
       ALLOCATE(PNEW%D1(NDIM*NSIZE), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:249 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF  

     CASE('D2')
       ALLOCATE(PNEW%D2(NDIM,NSIZE), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:257 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF
       
       
     CASE('I')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%I1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)THEN
             WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:268 '
             WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%I1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)THEN
        WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:277 '
        WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
         ELSE
            ALLOCATE(PNEW%I2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)THEN
        WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:284 '
        WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
         END IF
       END IF

     CASE('I0')
     CASE('I1')
       IF(NDIM > 1 .AND. NSIZE >1)THEN
         WRITE(FPAR%MESG,'(A,A)')' fll_mk: I1 array - One dimension must be 1',TRIM(NAME)
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
         FPAR%SUCCESS = .false.
         RETURN
       END IF
       ALLOCATE(PNEW%I1(NDIM*NSIZE), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:294 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF  

     CASE('I2')
       ALLOCATE(PNEW%I2(NDIM,NSIZE), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:302 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF
       
       
     CASE('L')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%L1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)THEN
        WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:313 '
        WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%L1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)THEN
              WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:322 '
             WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
            STOP
      END IF
         ELSE
            ALLOCATE(PNEW%L2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)THEN
              WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:329 '
              WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
              STOP
            END IF
         END IF
      END IF  
 
     CASE('L0')
     CASE('L1')
       IF(NDIM > 1 .AND. NSIZE >1)THEN
         WRITE(FPAR%MESG,'(A,A)')' fll_mk: L1 array - One dimension must be 1',TRIM(NAME)
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
         FPAR%SUCCESS = .false.
         RETURN
       END IF
       ALLOCATE(PNEW%L1(NDIM*NSIZE), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:339 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF  

     CASE('L2')
       ALLOCATE(PNEW%L2(NDIM,NSIZE), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:347 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF
       
     CASE('S')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%S1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:357 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%S1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)THEN
        WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:366 '
        WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
         ELSE
            ALLOCATE(PNEW%S2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)THEN
        WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:373 '
        WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
         END IF
       END IF

     CASE('S0')
     CASE('S1')
       IF(NDIM > 1 .AND. NSIZE >1)THEN
         WRITE(FPAR%MESG,'(A,A)')' fll_mk: S1 array - One dimension must be 1',TRIM(NAME)
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
         FPAR%SUCCESS = .false.
         RETURN
       END IF
       ALLOCATE(PNEW%S1(NDIM*NSIZE), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:383 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF  

     CASE('S2')
       ALLOCATE(PNEW%S2(NDIM,NSIZE), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:391 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF
     
     CASE('C')

     CASE('N','DIR') 
        FPAR%SUCCESS = .true.
        RETURN
     
     
     END SELECT
     
     FPAR%SUCCESS = .true.

    RETURN
   END FUNCTION FLL_MK_12D

   FUNCTION FLL_MK_3D(NAME,LTYPE,NDIM,NSIZE1,NSIZE2,FPAR,ERRMSG) RESULT(PNEW)
!
! Description: function creates node specified by name, type and dimensions
!
! External Modules used
! 
       USE FLL_TYPE_M
       USE FLL_OUT_M
       
       IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! NAME         In         name of node
! LTYPE        In         type of node  - can be *
! NDIM, NSIZE* In         node dimensions
! PNEW         Out        return pointer to newly created node
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
       TYPE(FUNC_DATA_SET)   :: FPAR
       TYPE(DNODE), POINTER  :: PNEW
       CHARACTER(*)  :: NAME
       CHARACTER(*)  :: LTYPE
       INTEGER(LINT) :: NDIM, NSIZE1,NSIZE2
       CHARACTER(*), OPTIONAL :: ERRMSG
!
! Local declarations
!       
       INTEGER :: ISTAT
       CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  local action
!
       IF(.NOT.PRESENT(ERRMSG))THEN
         LOC_ERRMSG='ALL'
       ELSE
         LOC_ERRMSG = ERRMSG
       END IF
       
       PNEW => NULL()
!
! Body
!
       IF(LEN_TRIM(LTYPE)<1.OR.LEN_TRIM(LTYPE)>TYPE_LENGTH) THEN
         WRITE(FPAR%MESG,'(A,A)')' Wrong type: ',TRIM(LTYPE)
         FPAR%success = .FALSE.
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
         RETURN
      END IF

      IF(LEN_TRIM(NAME)>NAME_LENGTH) THEN
        WRITE(FPAR%MESG,'(A,A)')' Wrong name: ',TRIM(NAME)
        FPAR%success = .FALSE.
        CALL FLL_OUT(LOC_ERRMSG,FPAR)
        RETURN
      END IF

      IF(.NOT.ANY(LTYPE(1:1)==(/'C','S','I','L','R','D','N'/))) THEN
        WRITE(FPAR%MESG,'(A,A)')' Wrong type: ',TRIM(LTYPE)
        FPAR%success = .FALSE.
        CALL FLL_OUT(LOC_ERRMSG,FPAR)
        RETURN
      END IF

      ALLOCATE(PNEW, STAT = ISTAT)
      IF(ISTAT /= 0)THEN
        WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:477 '
        WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
      
      PNEW%LNAME  = TRIM(NAME)
      PNEW%LTYPE = LTYPE
      PNEW%NDIM = 0
      PNEW%NSIZE = 0 
      PNEW%NSIZE1 = 0 
      
      PNEW%PPAR    =>NULL()
      PNEW%PCHILD  =>NULL()
      PNEW%PNEXT   =>NULL()
      PNEW%PPREV   =>NULL()
      PNEW%PLINK   =>NULL()      

      PNEW%NDIM = NDIM
      PNEW%NSIZE = NSIZE1
      PNEW%NSIZE1 = NSIZE2

      PNEW%PPAR => NULL()
      PNEW%PCHILD => NULL()
      PNEW%PPREV => NULL()
      PNEW%PNEXT => NULL()
      PNEW%PLINK => NULL()

      IF(NDIM < 1 .AND. NSIZE2 < 1 .AND. NSIZE2 <1)THEN
        WRITE(FPAR%MESG,'(A,A,I5,I5,I5,I5)')' Wrong dimensions for node ',TRIM(NAME), NDIM, NSIZE1, NSIZE2
        CALL FLL_OUT(LOC_ERRMSG,FPAR)
        FPAR%success = .FALSE.
        RETURN
      END IF
!
!  ALLOCATE ARRAYS
!
     SELECT CASE(LTYPE)

     CASE('R', 'R3')
       ALLOCATE(PNEW%R3(NDIM,NSIZE1,NSIZE2), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:517 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF 

     CASE('D', 'D3')
       ALLOCATE(PNEW%D3(NDIM,NSIZE1,NSIZE2), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:525 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF


     CASE('I', 'I3')
       ALLOCATE(PNEW%I3(NDIM,NSIZE1,NSIZE2), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:534 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF

     CASE('L', 'L3')
       ALLOCATE(PNEW%L3(NDIM,NSIZE1,NSIZE2), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:542 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF

     CASE DEFAULT
        WRITE(FPAR%MESG,'(A,A)')' Wrong type for multidimensional arrays: ',TRIM(LTYPE)
        CALL FLL_OUT(LOC_ERRMSG,FPAR)
        FPAR%success = .FALSE.
        RETURN

     FPAR%success = .TRUE.
     
     
     END SELECT

    RETURN
   END FUNCTION FLL_MK_3D


   FUNCTION FLL_MK_4D(NAME,LTYPE,NDIM,NSIZE1,NSIZE2,NSIZE3,FPAR,ERRMSG) RESULT(PNEW)
!
! Description: function creates node specified by name, type and dimensions
!
! External Modules used
! 
       USE FLL_TYPE_M
       USE FLL_OUT_M
       
       IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! NAME         In         name of node
! LTYPE        In         type of node  - can be *
! NDIM, NSIZE* In         node dimensions
! PNEW         Out        return pointer to newly created node
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
       TYPE(FUNC_DATA_SET)   :: FPAR
       TYPE(DNODE), POINTER  :: PNEW
       CHARACTER(*)  :: NAME
       CHARACTER(*)  :: LTYPE
       INTEGER(LINT) :: NDIM, NSIZE1,NSIZE2,NSIZE3
       CHARACTER(*), OPTIONAL :: ERRMSG
!
! Local declarations
!       
       INTEGER :: ISTAT
       CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  local action
!
       IF(.NOT.PRESENT(ERRMSG))THEN
         LOC_ERRMSG='ALL'
       ELSE
         LOC_ERRMSG = ERRMSG
       END IF
       
       PNEW => NULL()
!
! Body
!
       IF(LEN_TRIM(LTYPE)<1.OR.LEN_TRIM(LTYPE)>TYPE_LENGTH) THEN
         WRITE(FPAR%MESG,'(A,A)')' Wrong type: ',TRIM(LTYPE)
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
         RETURN
      END IF

      IF(LEN_TRIM(NAME)>NAME_LENGTH) THEN
        WRITE(FPAR%MESG,'(A,A)')' Wrong name: ',TRIM(NAME)
        CALL FLL_OUT(LOC_ERRMSG,FPAR)
        RETURN
      END IF

      IF(.NOT.ANY(LTYPE(1:1)==(/'C','S','I','L','R','D','N'/))) THEN
        WRITE(FPAR%MESG,'(A,A)')' Wrong type: ',TRIM(LTYPE)
        CALL FLL_OUT(LOC_ERRMSG,FPAR)
        RETURN
      END IF

      ALLOCATE(PNEW, STAT = ISTAT)
      IF(ISTAT /= 0)THEN
        WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:626 '
        WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
        STOP
      END IF
      
      PNEW%LNAME  = TRIM(NAME)
      PNEW%LTYPE = LTYPE
      PNEW%NDIM = 0
      PNEW%NSIZE = 0 
      PNEW%NSIZE1 = 0 
      PNEW%NSIZE2 = 0 
      
      PNEW%PPAR    =>NULL()
      PNEW%PCHILD  =>NULL()
      PNEW%PNEXT   =>NULL()
      PNEW%PPREV   =>NULL()
      PNEW%PLINK   =>NULL()

      PNEW%NDIM = NDIM
      PNEW%NSIZE = NSIZE1
      PNEW%NSIZE1 = NSIZE2      
      PNEW%NSIZE2 = NSIZE3      

      PNEW%PPAR => NULL()
      PNEW%PCHILD => NULL()
      PNEW%PPREV => NULL()
      PNEW%PNEXT => NULL()
      PNEW%PLINK => NULL()

      IF(NDIM < 1 .AND. NSIZE2 < 1 .AND. NSIZE2 <1 .AND. NSIZE3 < 1)THEN
        WRITE(FPAR%MESG,'(A,A,I5,I5,I5,I5)')' Wrong dimensions for node ',TRIM(NAME), NDIM, NSIZE1, NSIZE2, NSIZE3
        CALL FLL_OUT(LOC_ERRMSG,FPAR)
        RETURN
      END IF
!
!  ALLOCATE ARRAYS
!
     SELECT CASE(LTYPE)

     CASE('R', 'R4')
       ALLOCATE(PNEW%R4(NDIM,NSIZE1,NSIZE2,NSIZE3), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:668 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF 

     CASE('D', 'D4')
       ALLOCATE(PNEW%D4(NDIM,NSIZE1,NSIZE2,NSIZE3), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:676 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF


     CASE('I', 'I4')
       ALLOCATE(PNEW%I4(NDIM,NSIZE1,NSIZE2,NSIZE3), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:685 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF

     CASE('L', 'L4')
       ALLOCATE(PNEW%L4(NDIM,NSIZE1,NSIZE2,NSIZE3), STAT=ISTAT)
         IF(ISTAT /= 0)THEN
           WRITE(*,*)'ERROR ALLOCATING MEMORY ==> fll_mk ERR:693 '
           WRITE(*,*)' NODE NAME IS ',TRIM(NAME)
           STOP
         END IF

     CASE DEFAULT
        WRITE(FPAR%MESG,'(A,A)')' Wrong type for multidimensional arrays: ',TRIM(LTYPE)
        CALL FLL_OUT(LOC_ERRMSG,FPAR)
        RETURN
     
     
     END SELECT

    RETURN
   END FUNCTION FLL_MK_4D
   
END MODULE FLL_MK_M
