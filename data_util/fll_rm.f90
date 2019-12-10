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
MODULE FLL_RM_M
!
! Description: Contains function fll_rm
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
   SUBROUTINE FLL_RM(PNODE,FPAR,ERRMSG,DIAGMESSG)
!
! Description: function removes PNODE
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
    USE FLL_STICH_M
    USE FLL_OUT_M
    
    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be removed
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE,PCHILD
   TYPE(FUNC_DATA_SET) :: FPAR
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
!   
!   BODY OF SUBROUTINE
!   
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A)')' RM - null node '
         IF(PRESENT(DIAGMESSG))THEN
           FPAR%MESG = TRIM(FPAR%MESG)//' '//TRIM(DIAGMESSG)
         END IF
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   
   PCHILD => PNODE%PCHILD
!
!  STICH AND SUBSTRACT FROM PARENT 
!
   CALL FLL_STICH(PNODE,FPAR,LOC_ERRMSG)
!
! IF NODE HAS CHILDREN, REMOVE ALL OF THEM
!
   IF(ASSOCIATED(PCHILD))CALL FLL_RM_RECURSIVE_NODE(PCHILD,FPAR,LOC_ERRMSG)

   CALL FLL_DEALLOC_DATA(PNODE,FPAR,LOC_ERRMSG)
!
!  NULLIFY NODE
!
   DEALLOCATE(PNODE, STAT=ISTAT)
   IF(ISTAT /= 0)THEN
       WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:109 '
       STOP
   END IF
   NULLIFY(PNODE)

   FPAR%SUCCESS = .TRUE.

   RETURN
   END SUBROUTINE FLL_RM
!
!  DELETE CHID WITH ALL ITS CHILDREN
!
  RECURSIVE SUBROUTINE FLL_RM_RECURSIVE_NODE(PNODE,FPAR,LOC_ERRMSG)
!
! Description: recursive function removes PNODE
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
     IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be removed
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
    TYPE(DNODE), POINTER  :: PNODE
    TYPE(FUNC_DATA_SET)   :: FPAR
    CHARACTER(*) :: LOC_ERRMSG
!
!  Local declarations
!
    TYPE(DNODE), POINTER  :: PCURR, PNEXT
    INTEGER :: ISTAT
!
!  IF NODE HAS CHILDREN, DELET THEM FIRST
!
    PCURR => PNODE
!
!  IF CHILDREN, DELETE THEM FIRST
!
    DO WHILE(ASSOCIATED(PCURR))
       PNEXT => PCURR%PNEXT
       IF(ASSOCIATED(PCURR%PCHILD))THEN
         CALL FLL_RM_RECURSIVE_NODE(PCURR%PCHILD,FPAR,LOC_ERRMSG)
       END IF

       IF(TRIM(PCURR%LTYPE) /= 'LINK')THEN

         CALL  FLL_DEALLOC_DATA(PCURR,FPAR,LOC_ERRMSG)
         
         IF(ASSOCIATED(PCURR%PLINK))THEN
          PNODE%PLINK%PCHILD => NULL();
         END IF
         
         DEALLOCATE(PCURR, STAT=ISTAT)
           IF(ISTAT /= 0)THEN
             WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:178 '
             STOP
           END IF
         NULLIFY(PCURR)
         FPAR%SUCCESS = .TRUE.
       ELSE
!
!  SPCIAL TREATMENT FOR LINKS
!
         PCURR%PCHILD%PLINK => NULL()
         PCURR%PCHILD => NULL()
         DEALLOCATE(PCURR, STAT=ISTAT)
         IF(ISTAT /= 0)THEN
            WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:191 '
            STOP
         END IF
         NULLIFY(PCURR)
         FPAR%SUCCESS = .TRUE.
       END IF
       
       PCURR => PNEXT

    END DO
    
    FPAR%SUCCESS = .TRUE.
    RETURN

  END SUBROUTINE FLL_RM_RECURSIVE_NODE
!
!  FREE MEMORY FOR NODE
!
  SUBROUTINE FLL_DEALLOC_DATA(PNODE,FPAR,LOC_ERRMSG)
!
! Description: function deallocates data from associated with PNODE
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
    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be removed
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER(*) :: LOC_ERRMSG
!
! local declarations
!   
   INTEGER :: ISTAT
!
!   1D ARRAYS
!
           IF(ASSOCIATED(PNODE%R1))THEN
             DEALLOCATE(PNODE%R1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:247 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE.
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%D1))THEN
             DEALLOCATE(PNODE%D1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:258 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%I1))THEN
             DEALLOCATE(PNODE%I1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:269 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%L1))THEN
             DEALLOCATE(PNODE%L1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:280 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF
           
          IF(ASSOCIATED(PNODE%S1))THEN
             DEALLOCATE(PNODE%S1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:291 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF
!
!  2D ARRAYS
!
           IF(ASSOCIATED(PNODE%R2))THEN
             DEALLOCATE(PNODE%R2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:304 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%D2))THEN
             DEALLOCATE(PNODE%D2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:315 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%I2))THEN
             DEALLOCATE(PNODE%I2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:326 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%L2))THEN
             DEALLOCATE(PNODE%L2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:337 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF
           
            IF(ASSOCIATED(PNODE%S2))THEN
             DEALLOCATE(PNODE%S2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:348 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

!
!  3D ARRAYS
!
           IF(ASSOCIATED(PNODE%R3))THEN
             DEALLOCATE(PNODE%R3, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:362 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%D3))THEN
             DEALLOCATE(PNODE%D3, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:373 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%I3))THEN
             DEALLOCATE(PNODE%I3, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:384 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%L3))THEN
             DEALLOCATE(PNODE%L3, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:395 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF
!
!  4D ARRAYS
!
           IF(ASSOCIATED(PNODE%R4))THEN
             DEALLOCATE(PNODE%R4, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:408 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%D4))THEN
             DEALLOCATE(PNODE%D4, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:419 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%I4))THEN
             DEALLOCATE(PNODE%I4, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:430 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%L4))THEN
             DEALLOCATE(PNODE%L4, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                WRITE(*,*)'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:441 '
                STOP
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF
  
  END SUBROUTINE FLL_DEALLOC_DATA

END MODULE FLL_RM_M
