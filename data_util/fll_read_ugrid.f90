
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
MODULE FLL_READ_UGRID_M

CONTAINS

  FUNCTION  FLL_READ_UGRID(NAME, FMT,ENDIAN, FILEBC) RESULT(PFLL)
!
!  READS UGRID FILE
!
    USE FLL_TYPE_M
    USE FLL_OUT_M
    USE FLL_MV_M
    USE FLL_MK_M
    USE FLL_MKDIR_M
    USE FLL_RM_M
    IMPLICIT NONE
! 
! INPUT/OUTPUT DATA
!
    TYPE(DNODE), POINTER :: PFLL
    CHARACTER(LEN=*) :: NAME
    CHARACTER :: FMT, ENDIAN
    CHARACTER(LEN=*),OPTIONAL :: FILEBC
!
! LOCAL PARAMATERS
!
    TYPE(DNODE), POINTER :: PTMP,PREG,PBC,PELEM
    TYPE(FUNC_DATA_SET) :: FPAR
    REAL(RDOUBLE), POINTER :: COO(:,:)
    INTEGER(SINT) :: Number_of_Nodes, Number_of_Surf_Trias, Number_of_Surf_Quads,&
                         Number_of_Vol_Tets, Number_of_Vol_Pents_5, Number_of_Vol_Pents_6,&
                         Number_of_Vol_Hexs
    INTEGER ISTAT
    INTEGER(LINT) :: I,NBC,IBC,NTRIA,NQUAD,ITMP
    INTEGER(LINT), POINTER :: SIND3(:,:),SIND4(:,:),SURFID(:),VOLTET(:,:),&
        VOLPENT(:,:),VOLPENT6(:,:),VOLHEX(:,:)
    INTEGER(SINT) :: INDTMP3(3), INDTMP4(4),INDTMP1, INDTMP5(5),INDTMP6(6),&
        INDTMP8(8)
    LOGICAL :: OK
    CHARACTER(LEN=LSTRING_LENGTH), ALLOCATABLE :: CONDNAME(:)
    CHARACTER(LEN = LSTRING_LENGTH) :: BCNAME, STR
    INTEGER, ALLOCATABLE :: BCTYPE(:)
    LOGICAL :: BIN
    CHARACTER(LEN=ERR_MSG_LENGTH) :: LOC_ERRMSG
    
    PFLL  => FLL_MKDIR('ugrid_data', FPAR,ERRMSG='ALL')
!
!  READ BC NAMES
!
   IF(PRESENT(FILEBC))THEN
     OPEN(15, FILE=FILEBC)
     READ(15,*)NBC
     ALLOCATE(BCTYPE(NBC),CONDNAME(NBC), STAT = ISTAT)
      IF(ISTAT /= 0)THEN
        WRITE(*,*)'ERROR ALLOCATING'
        STOP
      END IF

     DO I=1,NBC
       READ(15,*)IBC,BCTYPE(I),CONDNAME(I)
     END DO
     CLOSE(15)
   END IF

   BIN = .FALSE.

   WRITE(*,*)' Name of ugrid file is ', trim(NAME)
   IF(FMT == 'A' .OR. FMT == 'a')then
     OPEN(15, FILE=NAME)
   ELSE
     BIN = .TRUE.
     IF(ENDIAN == 'B' .OR. ENDIAN == 'b')then
       OPEN(15,STATUS='UNKNOWN',FILE=TRIM(NAME),&
           ACCESS='STREAM',CONVERT='big_endian',IOSTAT=ISTAT)
     ELSE
       OPEN(15,STATUS='UNKNOWN',FILE=TRIM(NAME),&
           ACCESS='STREAM',CONVERT='little_endian',IOSTAT=ISTAT)
     END IF
   END IF
   
   IF(BIN)THEN
     READ(15) Number_of_Nodes, Number_of_Surf_Trias, Number_of_Surf_Quads,&
     Number_of_Vol_Tets, Number_of_Vol_Pents_5, Number_of_Vol_Pents_6,&
     Number_of_Vol_Hexs
   ELSE
     READ(15,*) Number_of_Nodes, Number_of_Surf_Trias, Number_of_Surf_Quads,&
     Number_of_Vol_Tets, Number_of_Vol_Pents_5, Number_of_Vol_Pents_6,&
     Number_of_Vol_Hexs
   END IF
   
   write(*,*)Number_of_Nodes, Number_of_Surf_Trias, Number_of_Surf_Quads,&
   Number_of_Vol_Tets, Number_of_Vol_Pents_5, Number_of_Vol_Pents_6,&
   Number_of_Vol_Hexs

   if(Number_of_Nodes < 1)then
    WRITE(FPAR%MESG,'(A)')' Read_ugrid  - number of nodes == 0, terminating ... '
    CALL FLL_OUT(LOC_ERRMSG,FPAR)
    FPAR%SUCCESS = .FALSE.
    call fll_rm(pfll, fpar)
    nullify(pfll)
    close(15)
    RETURN
   end if

   write(*,*)'Number of nodes: ', Number_of_Nodes
   if(Number_of_Surf_Trias > 0) write(*,*)'Number of surface triangles: ', Number_of_Surf_Trias
   if(Number_of_Surf_Quads > 0) write(*,*)'Number of surface quads: ', Number_of_Surf_Quads
   if(Number_of_Vol_Tets > 0)   write(*,*)'Number of surface : ', Number_of_Vol_Tets
   if(Number_of_Vol_Pents_5 > 0)write(*,*)'Number of surface : ', Number_of_Vol_Pents_5
   if(Number_of_Vol_Pents_6 > 0)write(*,*)'Number of surface : ', Number_of_Vol_Pents_6
   if(Number_of_Vol_Hexs > 0)   write(*,*)'Number of surface : ', Number_of_Vol_Hexs
!
!  ADD SOME BASIC INFO
!
  PTMP => FLL_MK('mesh_name','S',1_LINT,1_LINT,FPAR)
  PTMP%S0 = NAME
  OK = FLL_MV(PTMP, PFLL,FPAR)
  PTMP => FLL_MK('boundaries_name','S',1_LINT,1_LINT,FPAR)
  PTMP%S0 = FILEBC
  OK = FLL_MV(PTMP, PFLL,FPAR) 
  
  PREG  => FLL_MKDIR('grid', FPAR,ERRMSG='ALL')
  OK = FLL_MV(PREG, PFLL,FPAR) 
!
!  CREATE ARRAY FOR NODES
!
   WRITE(*,*)' Reading coordinates'
   PTMP => FLL_MK('coordinates','D',INT(Number_of_Nodes, KIND=LINT),3_LINT,FPAR)
   OK = FLL_MV(PTMP, PREG,FPAR)
   COO => PTMP%D2
   IF(BIN)THEN
     DO I=1,Number_of_Nodes
      READ(15)COO(I,:)
     END DO
   ELSE
     DO I=1,Number_of_Nodes
      READ(15,*)COO(I,:)
     END DO
   END IF
!
!  READ TRIANGLES
!
  IF( Number_of_Surf_Trias > 0)THEN
    WRITE(*,*)' Surface triangles'
    ALLOCATE(SIND3(Number_of_Surf_Trias, 3), STAT = ISTAT)
    IF(ISTAT /= 0)THEN
      WRITE(*,*)' ERROR ALLOCATING SIND3'
      STOP
    END IF
    IF(BIN)THEN
      DO I=1,Number_of_Surf_Trias
        READ(15)INDTMP3
        SIND3(I,:) = INDTMP3
      END DO
    ELSE
      DO I=1,Number_of_Surf_Trias
        READ(15,*)SIND3(I,:)
      END DO
    END IF
  END IF
!
!  READ QUADS
!
  IF( Number_of_Surf_Quads > 0)THEN
    WRITE(*,*)' Surface quads'
    ALLOCATE(SIND4(Number_of_Surf_Quads, 4), STAT = ISTAT)
    IF(ISTAT /= 0)THEN
      WRITE(*,*)' ERROR ALLOCATING SIND4'
      STOP
    END IF
    IF(BIN)THEN
      DO I=1,Number_of_Surf_Quads
        READ(15)INDTMP4
        SIND4(I,:) = INDTMP4
      END DO
    ELSE
      DO I=1,Number_of_Surf_Quads
        READ(15,*)SIND4(I,:)
      END DO
    END IF
 END IF
!
!  read surface IDs
!
  IF( Number_of_Surf_Trias+Number_of_Surf_Quads > 0)THEN
    WRITE(*,*)' Surface Bc IDs ', Number_of_Surf_Trias, Number_of_Surf_Quads
    ALLOCATE(SURFID(Number_of_Surf_Trias+Number_of_Surf_Quads), STAT = ISTAT)
    SURFID  = 0
    IF(ISTAT /= 0)THEN
      WRITE(*,*)' ERROR ALLOCATING SURFID'
      STOP
    END IF
    IF(BIN)THEN
      DO I=1,Number_of_Surf_Trias+Number_of_Surf_Quads
       READ(15)INDTMP1
       SURFID(I) = INDTMP1
      END DO
    ELSE
      DO I=1,Number_of_Surf_Trias+Number_of_Surf_Quads
        READ(15,*)SURFID(I)
      END DO    
    END IF
  END IF
!
!  find maximum value
!
  NBC = MAXVAL(SURFID) - MINVAL(SURFID)+1
  IF(NBC > 10000)THEN
   WRITE(*,*)' Number of BCs too high, check your file', nbc
   STOP
  END IF

  WRITE(*,*)' Estimated number of boundary conditions is ', NBC
 
  DO IBC=1,NBC

    NTRIA = 0
    DO I=1,Number_of_Surf_Trias
      IF(SURFID(I) == IBC) NTRIA = NTRIA + 1
    END DO
    NQUAD = 0
    DO I=Number_of_Surf_Trias+1, Number_of_Surf_Trias+Number_of_Surf_Quads
      IF(SURFID(I) == IBC) NQUAD = NQUAD + 1
    END DO
    
    write(*,*)'                                trias               quads'
    write(*,*)'BC statistics :', NTRIA,NQUAD
    if(NTRIA + NQUAD < 1)THEN
      WRITE(*,*)' Boundary condition ID ', IBC, ' does not contain any element, skipping whatever follows'
      cycle
    end if


    PBC => FLL_MKDIR('boundary',FPAR)
    OK = FLL_MV(PBC,PREG,FPAR)
    PTMP => FLL_MK('boundary_name','S',1_LINT,1_LINT,FPAR)
    IF(PRESENT(FILEBC))THEN
      PTMP%S0 = CONDNAME(IBC)
      write(*,*)' Boundary condition name is: ', trim(CONDNAME(IBC))
    ELSE
      WRITE(str,*)IBC
      BCNAME = 'boundary_'//trim(adjustl(str))
      PTMP%S0 = BCNAME
      write(*,*)' Setting boundary condition name to: ', trim(BCNAME)
    END IF


    OK = FLL_MV(PTMP,PBC,FPAR)
    
    IF(NTRIA > 0)THEN
      PELEM => FLL_MKDIR('bound_elem_group',FPAR)
      OK = FLL_MV(PELEM,PBC,FPAR)
      PTMP => FLL_MK('bound_elem_type','S',1_LINT,1_LINT,FPAR)
      PTMP%S0 = 'tria3'
      OK = FLL_MV(PTMP,PELEM,FPAR)
      PTMP => FLL_MK('bound_elem_nodes','L',NTRIA,3_LINT,FPAR)
      OK = FLL_MV(PTMP,PELEM,FPAR)

      ITMP = 0
      DO I=1,Number_of_Surf_Trias
         IF(SURFID(I) == IBC) THEN
           ITMP = ITMP +1
           PTMP%L2(ITMP,:) = SIND3(I,:)
         END IF
      END DO
      
    END IF
        
    IF(NQUAD > 0)THEN
      PELEM => FLL_MKDIR('bound_elem_group',FPAR)
      OK = FLL_MV(PELEM,PBC,FPAR)
      PTMP => FLL_MK('bound_elem_type','S',1_LINT,1_LINT,FPAR)
      PTMP%S0 = 'quad4'
      OK = FLL_MV(PTMP,PELEM,FPAR)
      PTMP => FLL_MK('bound_elem_nodes','L',NQUAD,4_LINT,FPAR)
      OK = FLL_MV(PTMP,PELEM,FPAR)

      ITMP = 0
      DO I=Number_of_Surf_Trias+1, Number_of_Surf_Trias+Number_of_Surf_Quads
         IF(SURFID(I) == IBC) THEN
           ITMP = ITMP +1
           PTMP%L2(ITMP,:) = SIND4(I-Number_of_Surf_Trias,:)
         END IF
      END DO
      
    END IF

  END DO
!
!  READ TET ELEMENTS
!
  IF( Number_of_Vol_Tets > 0)THEN
    WRITE(*,*)' Volume tetras: ',Number_of_Vol_Tets
    PTMP => FLL_MK('element_nodes','L',INT(Number_of_Vol_Tets, KIND=LINT),4_LINT,FPAR)
    PELEM=> FLL_MKDIR('element_group', FPAR)
    OK = FLL_MV(PELEM, PREG,FPAR)
    OK = FLL_MV(PTMP, PELEM,FPAR)
    VOLTET => PTMP%L2
    PTMP => FLL_MK('element_type', 'S', 1_LINT, 1_LINT, FPAR)
    PTMP%S0 ='tetra4'
    OK = FLL_MV(PTMP, PELEM,FPAR)
    IF(BIN)THEN
      DO I=1,Number_of_Vol_Tets
        READ(15)INDTMP4
        VOLTET(I,:) = INDTMP4
      END DO
    ELSE
      DO I=1,Number_of_Vol_Tets
        READ(15,*)VOLTET(I,:)
      END DO
    END IF
  END IF!
!
!  READ PENT ELEMENTS
!
  IF( Number_of_Vol_Pents_5 > 0)THEN
    WRITE(*,*)' Volume pentas: ',Number_of_Vol_Pents_5
    PTMP => FLL_MK('element_nodes','L',INT(Number_of_Vol_Pents_5, KIND=LINT),5_LINT,FPAR)
    PELEM=> FLL_MKDIR('element_group', FPAR)
    OK = FLL_MV(PELEM, PREG,FPAR)
    OK = FLL_MV(PTMP, PELEM,FPAR)
    VOLPENT => PTMP%L2
    PTMP => FLL_MK('element_type', 'S', 1_LINT, 1_LINT, FPAR)
    PTMP%S0 ='penta5'
    OK = FLL_MV(PTMP, PELEM,FPAR)
    IF(BIN)THEN
      DO I=1,Number_of_Vol_Pents_5
        READ(15)INDTMP5
          VOLPENT(I,:)=INDTMP5 
      END DO
    ELSE
      DO I=1,Number_of_Vol_Pents_5
        READ(15,*)VOLPENT(I,:)
      END DO
    END IF
  END IF
!
!  READ PENT6 ELEMENTS
!
  IF( Number_of_Vol_Pents_6 > 0)THEN
    WRITE(*,*)' Volume penta6: ',Number_of_Vol_Pents_6
    PTMP => FLL_MK('element_nodes','L',INT(Number_of_Vol_Pents_6, KIND=LINT),6_LINT,FPAR)
    PELEM=> FLL_MKDIR('element_group', FPAR)
    OK = FLL_MV(PELEM, PREG,FPAR)
    OK = FLL_MV(PTMP, PELEM,FPAR)   
    VOLPENT6 => PTMP%L2
    PTMP => FLL_MK('element_type', 'S', 1_LINT, 1_LINT, FPAR)
    PTMP%S0 ='penta6'
    OK = FLL_MV(PTMP, PELEM,FPAR)
    IF(BIN)THEN
      DO I=1,Number_of_Vol_Pents_6
        READ(15)INDTMP6
           VOLPENT6(I,:) = INDTMP6
      END DO
    ELSE
      DO I=1,Number_of_Vol_Pents_6
        READ(15,*)VOLPENT6(I,:)
      END DO
    END IF
  END IF
!
!  READ HEX ELEMENTS
!
  IF( Number_of_Vol_Hexs > 0)THEN
    WRITE(*,*)' Volume hexas: ',Number_of_Vol_Hexs
    PTMP => FLL_MK('element_nodes','L',INT(Number_of_Vol_Hexs, KIND=LINT),8_LINT,FPAR)
    PELEM=> FLL_MKDIR('element_group', FPAR)
    OK = FLL_MV(PELEM, PREG,FPAR)
    OK = FLL_MV(PTMP, PELEM,FPAR)
    VOLHEX => PTMP%L2
    PTMP => FLL_MK('element_type', 'S', 1_LINT, 1_LINT, FPAR)
    PTMP%S0 ='hexa8'
    OK = FLL_MV(PTMP, PELEM,FPAR)
    IF(BIN)THEN
      DO I=1,Number_of_Vol_Hexs
        READ(15)INDTMP8
         VOLHEX(I,:) = INDTMP8
      END DO
    ELSE
      DO I=1,Number_of_Vol_Hexs
        READ(15,*)VOLHEX(I,:)
      END DO
    ENDIF
  END IF
  CLOSE(15)    
!
!  deallocate arrays
!
  IF(ASSOCIATED(SIND3))DEALLOCATE(SIND3)
  IF(ASSOCIATED(SIND4))DEALLOCATE(SIND4)
  IF(ASSOCIATED(SURFID))DEALLOCATE(SURFID)
   
END FUNCTION FLL_READ_UGRID

END MODULE FLL_READ_UGRID_M
