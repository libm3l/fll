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
!
MODULE FLL_MPI_CP_M
!
! Description: Sends FLL list from one node to another
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
   FUNCTION FLL_MPI_CP(PNODE,COMMUNICATOR,SENDPART,RECPART,FPAR,ERRMSG,DIAGMESSG) RESULT(PNEW)
!
! Description: Sends FLL subset to a specified process in comunicator
!              if process ID == sending proceess ID, do not create
!              data set, just return the pointer on existing data set
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
    USE MPI
    USE FLL_TYPE_M
    USE FLL_MK_M
    USE FLL_MV_M
    USE FLL_OUT_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         node to duplicate
! PNEW         Out        duplicate node
! COMMUNICATOR In         MPI communicatior
! SENDPART     In         Sending process
! RECPART      In         Receiving process
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE,PNEW
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: COMMUNICATOR,SENDPART,RECPART
   CHARACTER(*), OPTIONAL :: ERRMSG,DIAGMESSG
!
!  Local declarations
!
   TYPE(DNODE), POINTER :: PCHILD
   INTEGER :: RANK, IERR
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
!  if not in group, return
!
   IF(COMMUNICATOR == MPI_COMM_NULL)THEN
     PNEW => NULL()
     RETURN
   END IF
!
!  check the node is not null
!
   FPAR%SUCCESS = .FALSE.
!
!  If not sending process, nullify pointer
!  owherwise check that sending process does not send NULL pointer and 
!  associate returning pointer with sending
!
   CALL MPI_Comm_rank ( COMMUNICATOR, RANK, IERR )

   DIFFPART: IF(SENDPART /= RANK)THEN
     PNEW => NULL()

     PNEW => NODE_RECEIVE(COMMUNICATOR, SENDPART, FPAR)
!
!  If dir has childrenm loop over them
!  the number of children in this routine is stored in 
!  NLINK, not ndim, ndim is set to 0 and then incremented automatically
!  when adding children
!
     IF(PNEW%NLINK > 0)THEN
!
!  Node has children
!
       CALL FLL_RECEIVE_RECURSIVE(PNEW,COMMUNICATOR,SENDPART,FPAR)

     END IF

     RETURN 

   ELSE
!
!  Sending process
!
     IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A)')' DUPLICATE - null node '
         IF(PRESENT(DIAGMESSG))THEN
           FPAR%MESG = TRIM(FPAR%MESG)//' '//TRIM(DIAGMESSG)
         END IF
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
       FPAR%SUCCESS = .FALSE.
       RETURN
     END IF

     PNEW => PNODE

     CALL NODE_SEND(PNODE, COMMUNICATOR, SENDPART, RECPART, FPAR)
  
     PCHILD => PNODE%PCHILD
!
! If node has children, duplicate them too
!
     IF(ASSOCIATED(PCHILD))THEN

       CALL FLL_SEND_RECURSIVE(PCHILD,COMMUNICATOR,SENDPART,RECPART,FPAR)

     END IF

   END IF DIFFPART

   FPAR%SUCCESS = .TRUE.

   RETURN

   END FUNCTION FLL_MPI_CP
!
!  DELETE CHID WITH ALL ITS CHILDREN
!
  RECURSIVE SUBROUTINE FLL_SEND_RECURSIVE(PNODE,COMMUNICATOR,SENDPART,RECPART,FPAR)
!
! Description: makes recursive duplicate of PNODE
!
! External Modules used
!
     USE FLL_TYPE_M
     USE FLL_MK_M
     USE FLL_MV_M
     USE FLL_OUT_M

     IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be duplicated
! SENDPART     In         sending process rank
! RECPART      In         receiving process rank
! COMMUNICATOR In         Commuticator
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
! 
    TYPE(DNODE), POINTER :: PNODE,PDUPL
    TYPE(FUNC_DATA_SET)  :: FPAR
    INTEGER              :: SENDPART,RECPART,COMMUNICATOR
!
! Local declarations
!
    TYPE(DNODE), POINTER  :: PCURR, PNEXT,PNEW,PCHILD
    LOGICAL :: OK
!
    PCURR => PNODE
    PCHILD => PNODE%PCHILD
!  NODE IS DIR
!  LOOP OVER CHILDREN
!
    DO WHILE(ASSOCIATED(PCURR))

       PNEXT => PCURR%PNEXT
       PCHILD=> PCURR%PCHILD

       CALL NODE_SEND(PCURR, COMMUNICATOR, SENDPART, RECPART, FPAR)
!
!  NODE HAS CHILDREN
!
       DO WHILE(ASSOCIATED(PCHILD))
          
          CALL FLL_SEND_RECURSIVE(PCHILD,COMMUNICATOR,SENDPART,RECPART,FPAR)
          PCHILD => PCHILD%PNEXT
         
       END DO
!
!  ADD TO PDUPL LIST
!
       PCURR => PNEXT

    END DO
    
    FPAR%SUCCESS = .TRUE.
    RETURN

  END SUBROUTINE FLL_SEND_RECURSIVE


  RECURSIVE SUBROUTINE FLL_RECEIVE_RECURSIVE(PNODE,COMMUNICATOR,SENDPART,FPAR)
!
! Description: makes recursive duplicate of PNODE
!
! External Modules used
!
     USE FLL_TYPE_M
     USE FLL_MK_M
     USE FLL_MV_M
     USE FLL_OUT_M

     IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be duplicated
! PNEW         In         recevied pointer
! SENDPART     In         sending process rank
! COMMUNICATOR In         Commuticator
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
! 
    TYPE(DNODE), POINTER :: PNODE,PNEW
    TYPE(FUNC_DATA_SET)  :: FPAR
    INTEGER              :: SENDPART,COMMUNICATOR
    INTEGER(LINT) :: I
!
! Local declarations
!
    TYPE(DNODE), POINTER  :: PCURR,PNEXT,PCHILD
    LOGICAL :: OK
    
    PCURR => PNODE
    PCHILD => PNODE%PCHILD
    
    DO I = 1, PCURR%NLINK

     PNEW => NODE_RECEIVE(COMMUNICATOR,SENDPART,FPAR)
     OK = FLL_MV(PNEW,PCURR, FPAR)
     
     IF(PNEW%NLINK >0) CALL FLL_RECEIVE_RECURSIVE(PNEW,COMMUNICATOR,SENDPART,FPAR)
     
   END DO
!

  END SUBROUTINE FLL_RECEIVE_RECURSIVE
!
!
  SUBROUTINE NODE_SEND(PNODE,COMMUNICATOR,SENDPART,RECPART,FPAR)
!
! Description: Boradcast - send the node
!
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
!
!
! External Modules used
! 
    USE MPI
    USE FLL_TYPE_M
    USE FLL_OUT_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer data which is to be duplicated
! COMMUNICATOR In         communicator
! SENDPART     In         sending process
! RECPART      In         receiving process
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER :: PNODE,PNEW
   TYPE(FUNC_DATA_SET)  :: FPAR
   INTEGER :: SENDPART, RECPART, COMMUNICATOR
!
! Local declarations
!
   INTEGER(LINT) :: NDIM, NSIZE,NSIZE1,NSIZE2, CODE, IARR(5)
   INTEGER :: IERR
!
! Prepare header of the node
!
   CODE  = GET_NCODE(PNODE)
   NDIM  = PNODE%NDIM
   NSIZE = PNODE%NSIZE
   NSIZE1 = PNODE%NSIZE1
   NSIZE2 = PNODE%NSIZE2

   IARR(1) = CODE
   IARR(2) = NDIM
   IARR(3) = NSIZE
   IARR(4) = NSIZE1
   IARR(5) = NSIZE2
!
!  Send node header
!
   CALL MPI_SSEND(IARR, 5, MPI_INTEGER8, RECPART, SENDPART, COMMUNICATOR, IERR ) 
   CALL MPI_SSEND(PNODE%LNAME, NAME_LENGTH, MPI_CHARACTER, RECPART, SENDPART, COMMUNICATOR, IERR ) 

   IF(CODE == 0) RETURN
!   IF(NDIM*NSIZE > 1) THEN
   IF(CODE > 10) THEN

!
!   1D ARRAYS
!
     IF(ASSOCIATED(PNODE%R1))THEN
       CALL MPI_SSEND(PNODE%R1, NDIM*NSIZE, MPI_REAL, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D1))THEN
       CALL MPI_SSEND(PNODE%D1, NDIM*NSIZE, MPI_DOUBLE, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I1))THEN
       CALL MPI_SSEND(PNODE%I1, NDIM*NSIZE, MPI_INTEGER, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L1))THEN
       CALL MPI_SSEND(PNODE%L1, NDIM*NSIZE, MPI_INTEGER8, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%S1))THEN
       CALL MPI_SSEND(PNODE%S1, NDIM*NSIZE*NAME_LENGTH, MPI_CHARACTER, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
!
! 2D ARRAYS
!
     IF(ASSOCIATED(PNODE%R2))THEN
       CALL MPI_SSEND(PNODE%R2, NDIM*NSIZE, MPI_REAL, RECPART, SENDPART, COMMUNICATOR, IERR ) 
      RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D2))THEN
       CALL MPI_SSEND(PNODE%D2, NDIM*NSIZE, MPI_DOUBLE, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I2))THEN
       CALL MPI_SSEND(PNODE%I2, NDIM*NSIZE, MPI_INTEGER, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L2))THEN
       CALL MPI_SSEND(PNODE%L2, NDIM*NSIZE, MPI_INTEGER8, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%S2))THEN
       CALL MPI_SSEND(PNODE%S2, NDIM*NSIZE*NAME_LENGTH, MPI_CHARACTER, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
!
! 3D ARRAYS
!
     IF(ASSOCIATED(PNODE%R3))THEN
       CALL MPI_SSEND(PNODE%R3, NDIM*NSIZE*NSIZE1, MPI_REAL, RECPART, SENDPART, COMMUNICATOR, IERR ) 
      RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D3))THEN
       CALL MPI_SSEND(PNODE%D3, NDIM*NSIZE*NSIZE1, MPI_DOUBLE, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I3))THEN
       CALL MPI_SSEND(PNODE%I3, NDIM*NSIZE*NSIZE1, MPI_INTEGER, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L3))THEN
       CALL MPI_SSEND(PNODE%L3, NDIM*NSIZE*NSIZE1, MPI_INTEGER8, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
!
! 4D ARRAYS
!
     IF(ASSOCIATED(PNODE%R4))THEN
       CALL MPI_SSEND(PNODE%R4, NDIM*NSIZE*NSIZE1*NSIZE2, MPI_REAL, RECPART, SENDPART, COMMUNICATOR, IERR ) 
      RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D4))THEN
       CALL MPI_SSEND(PNODE%D4, NDIM*NSIZE*NSIZE1*NSIZE2, MPI_DOUBLE, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I4))THEN
       CALL MPI_SSEND(PNODE%I4, NDIM*NSIZE*NSIZE1*NSIZE2, MPI_INTEGER, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L4))THEN
       CALL MPI_SSEND(PNODE%L4, NDIM*NSIZE*NSIZE1*NSIZE2, MPI_INTEGER8, RECPART, SENDPART, COMMUNICATOR, IERR ) 
       RETURN     
     END IF
!
!  SCALARS AND STATICALLY DEFINED ARRAYS
!
!   ELSE IF(NDIM*NSIZE == 1 )THEN
    ELSE
     SELECT CASE(CODE)
      CASE(1)
       CALL MPI_SSEND(PNODE%R0, 1, MPI_REAL, RECPART, SENDPART, COMMUNICATOR, IERR ) 
      CASE(2)
       CALL MPI_SSEND(PNODE%D0, 1, MPI_DOUBLE, RECPART, SENDPART, COMMUNICATOR, IERR ) 
      CASE(3)
       CALL MPI_SSEND(PNODE%I0, 1, MPI_INTEGER, RECPART, SENDPART, COMMUNICATOR, IERR ) 
      CASE(4)
       CALL MPI_SSEND(PNODE%L0, 1, MPI_INTEGER8, RECPART, SENDPART, COMMUNICATOR, IERR ) 
      CASE(5)
       CALL MPI_SSEND(PNODE%S0, NAME_LENGTH, MPI_CHARACTER, RECPART, SENDPART, COMMUNICATOR, IERR ) 
     END SELECT
   END IF  

  END SUBROUTINE NODE_SEND


  FUNCTION NODE_RECEIVE(COMMUNICATOR,SENDPART,FPAR) RESULT(PNODE)
!
! Description: Boradcast - receive the node
!
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
!
!
! External Modules used
! 
    USE MPI
    USE FLL_TYPE_M
    USE FLL_OUT_M
    USE FLL_MK_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer data which is to be duplicated
! COMMUNICATOR In         communicator
! SENDPART     In         sending process
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER :: PNODE
   TYPE(FUNC_DATA_SET)  :: FPAR
   INTEGER :: COMMUNICATOR, SENDPART
!
! Local declarations
!
   INTEGER(LINT) :: IARR(5), NDIM, NSIZE, NSIZE1, NSIZE2
   INTEGER :: IERR
   CHARACTER(LEN=TYPE_LENGTH) :: TYPE
   CHARACTER(LEN=NAME_LENGTH) :: NAME
   INTEGER STATUS(MPI_STATUS_SIZE)

   INTEGER :: TAG
!
! Prepare header of the node
!
   CALL MPI_RECV(IARR, 5, MPI_INTEGER8, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR ) 
   CALL MPI_RECV(NAME, NAME_LENGTH, MPI_CHARACTER, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
   TAG = STATUS(MPI_TAG)
   IF(SENDPART /= TAG)THEN
     WRITE(*,*)' COMMUNICATIONERROR'
     STOP
   END IF 

   TYPE = GET_NTYPE(INT(IARR(1), KIND=SINT))

   IF(IARR(1) == 0)THEN
!
!  IF DIR, STORE THE NDIM DIMENSION IN NLINK AND 
!  KEEP NIDM = 0
!  UPON ADDING MODES TO DIR, NDIM IS INCREMENTED AUTOMATICALLY
!
     PNODE => FLL_MK(NAME,TYPE,0_LINT,0_LINT ,FPAR)
     PNODE%LNAME = NAME
     PNODE%NLINK = IARR(2)
     IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A)')' NODE_RECEIVE - error allocating PNEW '
       FPAR%SUCCESS = .FALSE.
       PNODE => NULL()
       RETURN
     END IF
     RETURN

   ELSE
   
     PNODE => FLL_MK(NAME,TYPE,IARR(2),IARR(3) ,FPAR)
     IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A)')' NODE_RECEIVE - error allocating PNEW '
       FPAR%SUCCESS = .FALSE.
       PNODE => NULL()
       RETURN
     END IF
   END IF

   NDIM = IARR(2)
   NSIZE = IARR(3)
   NSIZE1 = IARR(4)
   NSIZE2 = IARR(5)
!    IF(NDIM*NSIZE > 1 )THEN
   IF(IARR(1) > 10)THEN
!
!   1D ARRAYS
!
     IF(ASSOCIATED(PNODE%R1))THEN
       CALL MPI_RECV(PNODE%R1, NDIM*NSIZE, MPI_REAL, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D1))THEN
       CALL MPI_RECV(PNODE%D1, NDIM*NSIZE, MPI_DOUBLE, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I1))THEN
       CALL MPI_RECV(PNODE%I1, NDIM*NSIZE, MPI_INTEGER, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L1))THEN
       CALL MPI_RECV(PNODE%L1, NDIM*NSIZE, MPI_INTEGER8, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%S1))THEN
       CALL MPI_RECV(PNODE%S1, NDIM*NSIZE*NAME_LENGTH, MPI_CHARACTER, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
!
! 2D ARRAYS
!
     IF(ASSOCIATED(PNODE%R2))THEN
       CALL MPI_RECV(PNODE%R2, NDIM*NSIZE, MPI_REAL, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D2))THEN
       CALL MPI_RECV(PNODE%D2, NDIM*NSIZE, MPI_DOUBLE, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I2))THEN
       CALL MPI_RECV(PNODE%I2, NDIM*NSIZE, MPI_INTEGER, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L2))THEN
       CALL MPI_RECV(PNODE%L2, NDIM*NSIZE, MPI_INTEGER8, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%S2))THEN
       CALL MPI_RECV(PNODE%S2, NDIM*NSIZE*NAME_LENGTH, MPI_CHARACTER, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
!
! 3D ARRAYS
!
     IF(ASSOCIATED(PNODE%R3))THEN
       CALL MPI_RECV(PNODE%R3, NDIM*NSIZE*NSIZE1, MPI_REAL, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D3))THEN
       CALL MPI_RECV(PNODE%D3, NDIM*NSIZE*NSIZE1, MPI_DOUBLE, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I3))THEN
       CALL MPI_RECV(PNODE%I3, NDIM*NSIZE*NSIZE1, MPI_INTEGER, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L3))THEN
       CALL MPI_RECV(PNODE%L3, NDIM*NSIZE*NSIZE1, MPI_INTEGER8, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
!
! 4D ARRAYS
!
     IF(ASSOCIATED(PNODE%R4))THEN
       CALL MPI_RECV(PNODE%R4, NDIM*NSIZE*NSIZE1*NSIZE2, MPI_REAL, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D4))THEN
       CALL MPI_RECV(PNODE%D4, NDIM*NSIZE*NSIZE1*NSIZE2, MPI_DOUBLE, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I4))THEN
       CALL MPI_RECV(PNODE%I4, NDIM*NSIZE*NSIZE1*NSIZE2, MPI_INTEGER, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L4))THEN
       CALL MPI_RECV(PNODE%L4, NDIM*NSIZE*NSIZE1*NSIZE2, MPI_INTEGER8, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
       RETURN     
     END IF
!
!  SCALARS AND STATICALLY DEFINED ARRAYS
!
!   ELSE IF(NDIM*NSIZE == 1 )THEN
    ELSE
     SELECT CASE(INT(IARR(1), KIND=SINT))
      CASE(1)
       CALL MPI_RECV(PNODE%R0, 1, MPI_REAL, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
      CASE(2)
       CALL MPI_RECV(PNODE%D0, 1, MPI_DOUBLE, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
      CASE(3)
       CALL MPI_RECV(PNODE%I0, 1, MPI_INTEGER, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
      CASE(4)
       CALL MPI_RECV(PNODE%L0, 1, MPI_INTEGER8, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
      CASE(5)
       PNODE%S0 = ''
       CALL MPI_RECV(PNODE%S0, NAME_LENGTH, MPI_CHARACTER, SENDPART, MPI_ANY_TAG, COMMUNICATOR, STATUS, IERR )
     END SELECT
   END IF

   RETURN
  
  END FUNCTION NODE_RECEIVE





  FUNCTION GET_NCODE(PNODE) RESULT(CODE)
!
! Description: gives back code for node
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
   USE FLL_OUT_M

   IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer data which is to be duplicated
! CODE         Out        return code
!
! Arguments declaration
!
   TYPE(DNODE), POINTER :: PNODE
   INTEGER :: CODE
!
! Local declaration
! 
   IF(.NOT.ASSOCIATED(PNODE))THEN
    CODE = -1
    RETURN
   END IF

   SELECT CASE(TRIM(PNODE%LTYPE))

   CASE('DIR','N')
     CODE = 0
   CASE('R')
     CODE = 1
   CASE('R1')
     CODE = 11
   CASE('R2')
     CODE = 12
   CASE('R3')
     CODE = 13
   CASE('R4')
     CODE = 14
   CASE('D')
     CODE = 2
   CASE('D1')
     CODE = 21
   CASE('D2')
     CODE = 22
   CASE('D3')
     CODE = 23
   CASE('D4')
     CODE = 24
   CASE('I')
     CODE = 3
   CASE('I1')
     CODE = 31
   CASE('I2')
     CODE = 32
   CASE('I3')
     CODE = 33
   CASE('I4')
     CODE = 34
   CASE('L')
     CODE = 4
   CASE('L1')
     CODE = 41
   CASE('L2')
     CODE = 42
   CASE('L3')
     CODE = 43
   CASE('L4')
     CODE = 44
   CASE('S')
     CODE = 5
   CASE('S1')
     CODE = 51
   CASE('S2')
     CODE = 52
!   CASE('S3')
!     CODE = 53
!   CASE('S4')
!     CODE = 54
   CASE('C')
     CODE = 6
   CASE('C1')
     CODE = 61
   CASE('C2')
     CODE = 62
!   CASE('C3')
!     CODE = 63
!   CASE('C4')
!     CODE = 64
   CASE DEFAULT
     CODE = -1
   END SELECT


  END FUNCTION GET_NCODE



  FUNCTION GET_NTYPE(CODE) RESULT(TYPE)
!
! Description: gives back code for node
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
    USE FLL_OUT_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer data which is to be duplicated
! TYPE         Out        type of node
!
! Arguments declaration
!
   INTEGER :: CODE
   CHARACTER(LEN=TYPE_LENGTH):: TYPE
!
! Local declaration
!

    SELECT CASE(CODE)
    CASE(0)
      TYPE = 'DIR'
    CASE(1)
      TYPE = 'R'
    CASE(11)
      TYPE = 'R1'
    CASE(12)
      TYPE = 'R2'
    CASE(13)
      TYPE = 'R3'
    CASE(14)
      TYPE = 'R4'
    CASE(2)
      TYPE = 'D'
    CASE(21)
      TYPE = 'D1'
    CASE(22)
      TYPE = 'D2'
    CASE(23)
      TYPE = 'D3'
    CASE(24)
      TYPE = 'D4'
    CASE(3)
      TYPE = 'I'
    CASE(31)
      TYPE = 'I1'
    CASE(32)
      TYPE = 'I2'
    CASE(33)
      TYPE = 'I3'
    CASE(34)
      TYPE = 'I4'
    CASE(4)
      TYPE = 'L'
    CASE(41)
      TYPE = 'L1'
    CASE(42)
      TYPE = 'L2'
    CASE(43)
      TYPE = 'L3'
    CASE(44)
      TYPE = 'L4'
    CASE(5)
      TYPE = 'S'
    CASE(51)
      TYPE = 'S1'
    CASE(52)
      TYPE = 'S2'
!    CASE(53)
!      TYPE = 'S3'
!    CASE(54)
!      TYPE = 'S4'
    CASE(6)
      TYPE = 'C'
    CASE(61)
      TYPE = 'C1'
    CASE(62)
      TYPE = 'C2'
!    CASE(63)
!      TYPE = 'C3'
!    CASE(64)
!      TYPE = 'C4'
    CASE DEFAULT
      TYPE = '0'
  
    END SELECT


  END FUNCTION GET_NTYPE

 

END MODULE FLL_MPI_CP_M
