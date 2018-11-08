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
MODULE READUGRID_M

CONTAINS

  SUBROUTINE READUGRID(PGLOB, PFLL, NAME, FILEBC, FMT,ENDIAN)
!
!  READS UGRID FILE
!
    USE FLL_MODS_M
    
    IMPLICIT NONE
! 
! INPUT/OUTPUT DATA
!
    TYPE(DNODE), POINTER :: PGLOB,PFLL
    CHARACTER(LEN=*) :: NAME, FILEBC
    CHARACTER :: FMT,ENDIAN
!
! LOCAL PARAMATERS
!
    TYPE(DNODE), POINTER :: PTMP,PREG,PBC,PCOPY,PELEM
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
    INTEGER, ALLOCATABLE :: BCTYPE(:)
    LOGICAL :: BIN
!
!  READ BC NAMES
!
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
!
!  ADD DATA TO STRUCTURE
!
  PTMP => FLL_MK('Num_Nodes','L',1_LINT,1_LINT,FPAR)
  PTMP%L0 = Number_of_Nodes
  OK = FLL_MV(PTMP, PGLOB,FPAR)
  PTMP => FLL_MK('Num_Surf_Trias','L',1_LINT,1_LINT,FPAR)
  PTMP%L0 = Number_of_Surf_Trias
  OK = FLL_MV(PTMP, PGLOB,FPAR)
  PTMP => FLL_MK('Num_Surf_Quads','L',1_LINT,1_LINT,FPAR)
  PTMP%L0 = Number_of_Surf_Quads   
  OK = FLL_MV(PTMP, PGLOB,FPAR)
  PTMP => FLL_MK('Num_Vol_Tets','L',1_LINT,1_LINT,FPAR)
  PTMP%L0 = Number_of_Vol_Tets 
  OK = FLL_MV(PTMP, PGLOB,FPAR)
  PTMP => FLL_MK('Num_Vol_Pents_5','L',1_LINT,1_LINT,FPAR)
  PTMP%L0 = Number_of_Vol_Pents_5   
  OK = FLL_MV(PTMP, PGLOB,FPAR)
  PTMP => FLL_MK('Num_Vol_Pents_6','L',1_LINT,1_LINT,FPAR)
  PTMP%L0 = Number_of_Vol_Pents_6
  OK = FLL_MV(PTMP, PGLOB,FPAR)
  PTMP => FLL_MK('Num_Vol_Hexs','L',1_LINT,1_LINT,FPAR)
  PTMP%L0 = Number_of_Vol_Hexs  
  OK = FLL_MV(PTMP, PGLOB,FPAR)
!
!  ADD SOME BASIC INFOR
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
   WRITE(*,*)' Reading cordinates'
   PTMP => FLL_MK('coordinates','D',INT(Number_of_Nodes, KIND=LINT),3_LINT,FPAR)
   PCOPY => FLL_CP(PTMP, PGLOB,FPAR)
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
    PTMP => FLL_MK('Surface_Trias','L',INT(Number_of_Surf_Trias, KIND=LINT),3_LINT,FPAR)
    OK = FLL_MV(PTMP, PGLOB,FPAR)
    SIND3 => PTMP%L2
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
    PTMP => FLL_MK('Surface_Quads','L',INT(Number_of_Surf_Quads, KIND=LINT),4_LINT,FPAR)
    OK = FLL_MV(PTMP, PGLOB,FPAR)
    SIND4 => PTMP%L2
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
    WRITE(*,*)' Surface Bc IDs'
    PTMP => FLL_MK('Surface_Bcs_ID','L',INT(Number_of_Surf_Trias+Number_of_Surf_Quads, KIND=LINT),1_LINT,FPAR)
    OK = FLL_MV(PTMP, PGLOB,FPAR)
    SURFID => PTMP%L1
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
  NBC =MAXVAL(SURFID)
  WRITE(*,*)' Number of boundary conditions is ', NBC
  
  DO IBC=1,NBC

    PBC => FLL_MKDIR('boundary',FPAR)
    OK = FLL_MV(PBC,PREG,FPAR)
    PTMP => FLL_MK('boundary_name','S',1_LINT,1_LINT,FPAR)
    PTMP%S0 = CONDNAME(IBC)
    OK = FLL_MV(PTMP,PBC,FPAR)
    NTRIA = 0
    DO I=1,Number_of_Surf_Trias
      IF(SURFID(I) == IBC) NTRIA = NTRIA + 1
    END DO
    NQUAD = 0
    DO I=Number_of_Surf_Trias+1, Number_of_Surf_Quads
      IF(SURFID(I) == IBC) NQUAD = NQUAD + 1
    END DO
    
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
      DO I=Number_of_Surf_Trias+1, Number_of_Surf_Quads
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
    WRITE(*,*)' Volume tetras'
    PTMP => FLL_MK('element_nodes','L',INT(Number_of_Vol_Tets, KIND=LINT),4_LINT,FPAR)
    PCOPY => FLL_CP(PTMP, PGLOB,FPAR)
    PELEM=> FLL_MKDIR('element_group', FPAR)
    OK = FLL_MV(PELEM, PREG,FPAR)
    OK = FLL_MV(PTMP, PELEM,FPAR)
    VOLTET => PTMP%L2
    PTMP => FLL_MK('element_type', 'S', 1_LINT, 1_LINT, FPAR)
    PTMP%S0 ='tetra4'
    OK = FLL_MV(PTMP, PELEM,FPAR)
    DO I=1,Number_of_Vol_Tets
      READ(15,*)VOLTET(I,:)
    END DO
  END IF!
!  READ PENT ELEMENTS
!
  IF( Number_of_Vol_Pents_5 > 0)THEN
    WRITE(*,*)' Volume pentas'
    PTMP => FLL_MK('element_nodes','L',INT(Number_of_Vol_Pents_5, KIND=LINT),5_LINT,FPAR)
    PCOPY = FLL_CP(PTMP, PGLOB,FPAR)
    PELEM=> FLL_MKDIR('element_group', FPAR)
    OK = FLL_MV(PELEM, PREG,FPAR)
    OK = FLL_MV(PTMP, PELEM,FPAR)
    VOLPENT => PTMP%L2
    PTMP => FLL_MK('element_type', 'S', 1_LINT, 1_LINT, FPAR)
    PTMP%S0 ='penta5'
    OK = FLL_MV(PTMP, PELEM,FPAR)
    IF(BIN)THEN
      DO I=1,Number_of_Vol_Pents_5
        READ(15,*)INDTMP5
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
    WRITE(*,*)' Volume penta6'
    PTMP => FLL_MK('element_nodes','L',INT(Number_of_Vol_Pents_6, KIND=LINT),6_LINT,FPAR)
    OK = FLL_MV(PTMP, PGLOB,FPAR)
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
  IF( Number_of_Vol_Pents_6 > 0)THEN
    WRITE(*,*)' Volume hexas'
    PTMP => FLL_MK('element_nodes','L',INT(Number_of_Vol_Pents_6, KIND=LINT),8_LINT,FPAR)
    OK = FLL_MV(PTMP, PGLOB,FPAR)
    PELEM=> FLL_MKDIR('element_group', FPAR)
    OK = FLL_MV(PELEM, PREG,FPAR)
    OK = FLL_MV(PTMP, PELEM,FPAR)
    VOLHEX => PTMP%L2
    PTMP => FLL_MK('element_type', 'S', 1_LINT, 1_LINT, FPAR)
    PTMP%S0 ='hexa'
    OK = FLL_MV(PTMP, PELEM,FPAR)
    IF(BIN)THEN
      DO I=1,Number_of_Vol_Pents_6
        READ(15)INDTMP8
 	VOLHEX(I,:) = INDTMP8
      END DO
    ELSE
      DO I=1,Number_of_Vol_Pents_6
        READ(15,*)VOLHEX(I,:)
      END DO
    ENDIF
  END IF
  CLOSE(15)    


! CALL FLL_CAT(PFLL, 6, .true., FPAR)

   
END SUBROUTINE READUGRID
   

END MODULE READUGRID_M
