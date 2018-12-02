!***********************************************************************
!
! $HeadURL: https://costello.foi.se/svn/edge/branches/aeroelastics/src_util/basic/sortint_m.f90 $
! $LastChangedDate: 2010-10-15 15:17:01 +0200 (Fri, 15 Oct 2010) $
! $LastChangedBy: enp $
! $LastChangedRevision: 2360 $
!
!***********************************************************************
!
MODULE SORTINT_M
!
! This module contains efficient sorting- and table-lookup functionality.
!
  IMPLICIT NONE
  PRIVATE
  PUBLIC SORTINT              ! Efficient sorting algorithm
  PUBLIC REMOVEDUPINT         ! Remove non-unique elements from a sorted array
  PUBLIC LOOKUP_EQ            ! Lookup value in sorted table
  PUBLIC LOOKUP_LE            ! Lookup lower or equal value in sorted table
  PUBLIC LOOKUP_LO            ! Lookup strictly lower value in sorted table
!
! The following constant determines the size of data at which
! the algorithm switches between "quicksort" and "insertion sort"
!
  INTEGER, PARAMETER :: SMALLVECTORLIMIT = 10

  INTERFACE LOOKUP_EQ
    MODULE PROCEDURE LOOKUP_EQ_INT
  END INTERFACE LOOKUP_EQ

  INTERFACE LOOKUP_LE
    MODULE PROCEDURE LOOKUP_LE_INT
  END INTERFACE LOOKUP_LE

  INTERFACE LOOKUP_LO
    MODULE PROCEDURE LOOKUP_LO_INT
  END INTERFACE LOOKUP_LO

  INTERFACE SORTINT
    MODULE PROCEDURE SORTINT_INDIRECT,SORTINT_DIRECT
  END INTERFACE SORTINT

  INTERFACE REMOVEDUPINT
    MODULE PROCEDURE REMOVEDUPINT_DIRECT,REMOVEDUPINT_INDIRECT
  END INTERFACE REMOVEDUPINT

CONTAINS

!***********************************************************************
  SUBROUTINE SORTINT_INDIRECT(INTDATA, IND)
!***********************************************************************
!
! FUNCTION:
!   THE SUBROUTINE SORTINT SORTS THE ONE DIMENSIONAL
!   INTEGER ARRAY INTDATA IN ASCENDING ORDER. INTDATA IS
!   UNCHANGED BY SORTINT, THE OUTPUT ONE DIMENSIONAL INTEGER
!   ARRAY IND CONTAINS THE SORTED ELEMENT POSITIONS AS
!   REFERENCES TO THE ELEMENT POSITIONS IN INTDATA. SORTINT
!   IS THE PUBLIC INTERFACE TO THE SUBROUTINE SORTINTBASE.
!
! INPUTS:    NAME     TYPE           DESCRIPTION
!
! 1          INTDATA INTEGER(:)      DATA TO BE SORTED, NOT CHANGED
!
! OUTPUTS :  NAME     TYPE           DESCRIPTION
!
! 2          IND      INTEGER(:)     SORTED INDEX LIST WITH REFERENCES
!                                    TO THE ORIGINAL ELEMENT POSITIONS
!
! CALLED FROM:
!
! CREATION BY: INGMAR KARLSSON
!
! CREATION DATE: 2002-01-10
!
! MODIFICATIONS:
! DATE        VERS   PROGRAMMER    DESCRIPTION
!
! REFERENCES:
!
!----------------------------------------------------------------------
    IMPLICIT NONE
!
! ARGUMENTS:
!
    INTEGER,            INTENT(IN)   :: INTDATA(:)
    INTEGER,            INTENT(OUT)  :: IND(:)
!
! SUBPROGRAMS: SORTINTBASE
!
!
! LOCAL VARIABLES:
    INTEGER                :: I
!
! BODY
!
!
! CHECK THE SIZES OF THE ARGUMENTS
!
    IF( SIZE(INTDATA) /= SIZE(IND) ) THEN
      WRITE(*,*) '*** ERROR: MISMATCHING SIZE OF ARGUMENTS TO ***'
      WRITE(*,*) '*** SORTINT. PROGRAM STOPPED               ***'
      STOP
    END IF
!
! INITIALIZE THE ARRAY OF ELEMENT POSITIONS
!
    DO I=1, SIZE(IND)
      IND(I) = I
    END DO
!
! CALL THE SORTING ROUTINE
!
    CALL SORTINTBASE(INTDATA, 1, SIZE(INTDATA), IND)

    RETURN
!------------------------------------ END OF SORTINT -----------------
  END SUBROUTINE SORTINT_INDIRECT


!***********************************************************************
  RECURSIVE SUBROUTINE SORTINTBASE(INTDATA, FIRST, LAST, IND)
!***********************************************************************
!
!     FUNCTION: THE RECURSIVE SUBROUTINE SORTINTBASE USES QUICKSORT
!               TO SORT THE ONE DIMENSIONAL INTEGER ARRAY INTDATA.
!               USES MEDIAN-OF-THREE PARTITIONING AND HAS A CUTOFF
!               FOR SMALL VECTORS WHERE INSERTION SORT IS APPLIED.
!               THE PERFORMANCE IS ON THE AVERAGE N*LOG(N) OPERATIONS.
!               THE ARRAY INTDATA IS UNCHANGED.
!               THE OUTPUT ONE DIMENSIONAL SORTED INTEGER ARRAY IND
!               CONTAINS REFERENCES TO THE ELEMENT POSITIONS OF THE
!               ORIGINAL ARRAY.
!
!     INPUTS:    NAME     TYPE           DESCRIPTION
!
!     1          INTDATA  INTEGER(:)     DATA TO BE SORTED, UNCHANGED
!     2          FIRST    INTEGER        INDEX OF THE FIRST ELEMENT
!     3          LAST     INTEGER        INDEX OF THE LAST ELEMENT
!
!     OUTPUTS :  NAME     TYPE           DESCRIPTION
!
!     4          IND      INTEGER(:)     SORTED ELEMENT POSITION LIST
!
!     CALLED FROM:
!
!     CREATION BY: INGMAR KARLSSON
!
!     CREATION DATE: 2002-01-10
!
!     MODIFICATIONS:
!     DATE        VERS   PROGRAMMER    DESCRIPTION
!     020528      2.3    I.KARLSSON    FIXED BUG IN INSERTION SORT
!
!     REFERENCES:
!
!----------------------------------------------------------------------
    IMPLICIT NONE
!
! ARGUMENTS:
!
    INTEGER,            INTENT(IN)   :: INTDATA(:)
    INTEGER,            INTENT(IN)   :: FIRST
    INTEGER,            INTENT(IN)   :: LAST
    INTEGER,            INTENT(INOUT):: IND(:)
!
! SUBPROGRAMS:
!
!
! LOCAL VARIABLES:
!
    INTEGER                :: LEFT
    INTEGER                :: RIGHT
    INTEGER                :: CENTER
    INTEGER                :: ISWAP
    INTEGER                :: PIVOT
!
!-----------------------------------------------------------------------
! BODY OF SORTINTBASE
!-----------------------------------------------------------------------
!
! QUICKSORT FOR VECTORS LONGER THAN SMALLVECTORLIMIT ELSE
! INSERTIONSORT
!
    IF( FIRST + SMALLVECTORLIMIT < LAST ) THEN
!
!   SELECT THE PIVOT AS THE MEDIAN OF THREE
!
      CENTER = (FIRST+LAST)/2
!
      IF( INTDATA(IND(CENTER)) < INTDATA(IND(FIRST)) ) THEN
        ISWAP            = IND(CENTER)
        IND(CENTER)      = IND(FIRST)
        IND(FIRST)       = ISWAP
      END IF
!
      IF( INTDATA(IND(LAST)) < INTDATA(IND(FIRST)) ) THEN
        ISWAP            = IND(LAST)
        IND(LAST)        = IND(FIRST)
        IND(FIRST)       = ISWAP
      END IF
!
      IF( INTDATA(IND(LAST)) < INTDATA(IND(CENTER)) ) THEN
        ISWAP            = IND(LAST)
        IND(LAST)        = IND(CENTER)
        IND(CENTER)      = ISWAP
      END IF
!
!   PLACE THE PIVOT AT POSITION LAST-1
!
      ISWAP            = IND(LAST-1)
      IND(LAST-1)      = IND(CENTER)
      IND(CENTER)      = ISWAP
      PIVOT            = INTDATA(IND(LAST-1))
!
!   BEGIN PARTITIONING
!
      LEFT   = FIRST
      RIGHT  = LAST-1
!
!   REPEAT WHILE LEFT AND RIGHT HAVE NOT MET
!
      DO
        DO
          LEFT = LEFT+1
          IF( INTDATA(IND(LEFT)) >= PIVOT ) EXIT
        END DO
!
        DO
          RIGHT = RIGHT-1
          IF( PIVOT >= INTDATA(IND(RIGHT)) ) EXIT
        END DO
!
        IF( LEFT < RIGHT ) THEN
          ISWAP           = IND(LEFT)
          IND(LEFT)       = IND(RIGHT)
          IND(RIGHT)      = ISWAP
        ELSE
          EXIT
        END IF
      END DO
!
!   RESTORE THE PIVOT
!
      ISWAP             = IND(LAST-1)
      IND(LAST-1)       = IND(LEFT)
      IND(LEFT)         = ISWAP
!
!   RECURSIVE CALLS
!
      CALL SORTINTBASE(INTDATA, FIRST , LEFT-1, IND)
      CALL SORTINTBASE(INTDATA, LEFT+1, LAST  , IND)
    ELSE
!
!   INSERTION SORT FOR SMALL VECTORS
!
      DO LEFT = FIRST+1, LAST
        RIGHT = LEFT-1
        DO WHILE( RIGHT >= FIRST )
          IF( INTDATA(IND(RIGHT+1)) >= INTDATA(IND(RIGHT))) THEN
            EXIT
          END IF
          ISWAP             = IND(RIGHT+1)
          IND(RIGHT+1)      = IND(RIGHT)
          IND(RIGHT)        = ISWAP
          RIGHT             = RIGHT-1
        END DO
      END DO
    END IF
!
    RETURN
!-------------------------------------END OF SORTINTBASE ---------------
  END SUBROUTINE SORTINTBASE


!***********************************************************************
  SUBROUTINE SORTINT_DIRECT(INTDATA)
!***********************************************************************
!
! FUNCTION:
!   This subroutine uses the same algorithm as SORTINT
!   but reorders the elements of INTDATA inplace (not
!   indirectly using an index vector).
!
! CREATION BY: Oskar Enoksson
!
! MODIFICATIONS:
! DATE        VERS   PROGRAMMER    DESCRIPTION
!
! REFERENCES:
!
!----------------------------------------------------------------------
    IMPLICIT NONE
!
! ARGUMENTS:
!
    INTEGER,            INTENT(INOUT):: INTDATA(:) ! Integers to be sorted
!
! LOCAL VARIABLES:
!
!
! BODY
!
    CALL SORTINTBASE_DIRECT(INTDATA, 1, SIZE(INTDATA))

  END SUBROUTINE SORTINT_DIRECT


!***********************************************************************
  RECURSIVE SUBROUTINE SORTINTBASE_DIRECT(INTDATA, FIRST, LAST)
!***********************************************************************
!
! FUNCTION:
!   This subroutine uses the same algorithm as SORTINTBASE
!   but reorders the elements of INTDATA inplace (not
!   indirectly using an index vector).
!
! INPUTS:    NAME     TYPE           DESCRIPTION
!
! 1          INTDATA  INTEGER(:)     DATA TO BE SORTED
! 2          FIRST    INTEGER        INDEX OF THE FIRST ELEMENT
! 3          LAST     INTEGER        INDEX OF THE LAST ELEMENT
!
! OUTPUTS :  NAME     TYPE           DESCRIPTION
!
! 4          INTDATA  INTEGER(:)     SORTED ELEMENT LIST
!
! CALLED FROM:
!
! CREATION BY: Oskar Enoksson
!
! MODIFICATIONS:
! DATE        VERS   PROGRAMMER    DESCRIPTION
!
! REFERENCES:
!
!----------------------------------------------------------------------
    IMPLICIT NONE
!
! ARGUMENTS:
!
    INTEGER,            INTENT(INOUT):: INTDATA(:)
    INTEGER,            INTENT(IN)   :: FIRST
    INTEGER,            INTENT(IN)   :: LAST
!
! LOCAL VARIABLES:
!
    INTEGER                :: LEFT
    INTEGER                :: RIGHT
    INTEGER                :: CENTER
    INTEGER                :: ISWAP
    INTEGER                :: PIVOT
!
!-----------------------------------------------------------------------
! BODY OF SORTINTBASE_DIRECT
!-----------------------------------------------------------------------
!
! QUICKSORT FOR VECTORS LONGER THAN SMALLVECTORLIMIT ELSE
! INSERTIONSORT
!
    IF( FIRST + SMALLVECTORLIMIT < LAST ) THEN
!
!   SELECT THE PIVOT AS THE MEDIAN OF THREE
!
      CENTER = (FIRST+LAST)/2
!
      IF( INTDATA(CENTER) < INTDATA(FIRST) ) THEN
        ISWAP            = INTDATA(CENTER)
        INTDATA(CENTER)  = INTDATA(FIRST)
        INTDATA(FIRST)   = ISWAP
      END IF
!
      IF( INTDATA(LAST) < INTDATA(FIRST) ) THEN
        ISWAP            = INTDATA(LAST)
        INTDATA(LAST)    = INTDATA(FIRST)
        INTDATA(FIRST)   = ISWAP
      END IF
!
      IF( INTDATA(LAST) < INTDATA(CENTER) ) THEN
        ISWAP            = INTDATA(LAST)
        INTDATA(LAST)    = INTDATA(CENTER)
        INTDATA(CENTER)  = ISWAP
      END IF
!
!   PLACE THE PIVOT AT POSITION LAST-1
!
      ISWAP            = INTDATA(LAST-1)
      INTDATA(LAST-1)  = INTDATA(CENTER)
      INTDATA(CENTER)  = ISWAP
      PIVOT            = INTDATA(LAST-1)
!
!   BEGIN PARTITIONING
!
      LEFT   = FIRST
      RIGHT  = LAST-1
!
!   REPEAT WHILE LEFT AND RIGHT HAVE NOT MET
!
      DO
        DO
          LEFT = LEFT+1
          IF( INTDATA(LEFT) >= PIVOT ) EXIT
        END DO
!
        DO
          RIGHT = RIGHT-1
          IF( PIVOT >= INTDATA(RIGHT) ) EXIT
        END DO
!
        IF( LEFT < RIGHT ) THEN
          ISWAP           = INTDATA(LEFT)
          INTDATA(LEFT)   = INTDATA(RIGHT)
          INTDATA(RIGHT)  = ISWAP
        ELSE
          EXIT
        END IF
      END DO
!
!   RESTORE THE PIVOT
!
      ISWAP             = INTDATA(LAST-1)
      INTDATA(LAST-1)   = INTDATA(LEFT)
      INTDATA(LEFT)     = ISWAP
!
!   RECURSIVE CALLS
!
      CALL SORTINTBASE_DIRECT(INTDATA, FIRST , LEFT-1)
      CALL SORTINTBASE_DIRECT(INTDATA, LEFT+1, LAST  )
    ELSE
!
!   INSERTION SORT FOR SMALL VECTORS
!
      DO LEFT = FIRST+1, LAST
        RIGHT = LEFT-1
        DO WHILE( RIGHT >= FIRST )
          IF( INTDATA(RIGHT+1) >= INTDATA(RIGHT)) THEN
            EXIT
          END IF
          ISWAP             = INTDATA(RIGHT+1)
          INTDATA(RIGHT+1)  = INTDATA(RIGHT)
          INTDATA(RIGHT)    = ISWAP
          RIGHT             = RIGHT-1
        END DO
      END DO
    END IF
!
    RETURN
!-------------------------------------END OF SORTINTBASE ---------------
  END SUBROUTINE SORTINTBASE_DIRECT


!***********************************************************************
  FUNCTION REMOVEDUPINT_DIRECT(INTDATA) RESULT(REMOVEDUP)
!***********************************************************************
!
! FUNCTION:
!   Remove duplicate values from the sorted array INTDATA
!   and pack unique elements towards the beginning of INTDATA.
!   Returns number of non-duplicate elements.
!   INTDATA must be sorted in ascending order before calling this
!   function.
!
! CREATION BY: Oskar Enoksson
!
! MODIFICATIONS:
! DATE        VERS   PROGRAMMER    DESCRIPTION
!
! REFERENCES:
!
!----------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER                          :: REMOVEDUP
!
! ARGUMENTS:
!
    INTEGER,            INTENT(INOUT):: INTDATA(:) ! Sorted data
!
! LOCAL VARIABLES:
!
    INTEGER N
    INTEGER VAL,LASTVAL
!
! BODY
!
    REMOVEDUP           = 0

    IF(SIZE(INTDATA) == 0) RETURN

    REMOVEDUP           = 1
    LASTVAL             = INTDATA(1)

    DO N=2,SIZE(INTDATA)
      VAL               = INTDATA(N)
      IF(VAL == LASTVAL) CYCLE

      REMOVEDUP         = REMOVEDUP+1
      INTDATA(REMOVEDUP)= VAL
      LASTVAL           = VAL
    END DO

    RETURN

  END FUNCTION REMOVEDUPINT_DIRECT


!***********************************************************************
  FUNCTION REMOVEDUPINT_INDIRECT(INTDATA,IND) RESULT(REMOVEDUP)
!***********************************************************************
!
! FUNCTION:
!   Remove duplicate values from the sorted array INTDATA
!   and pack unique elements towards the beginning of INTDATA.
!   Returns number of non-duplicate elements.
!
!   INTDATA is not modified, only the index permutation IND is changed.
!   INTDATA(IND(:)) must be sorted in ascending order before
!   calling this function.
!
! CREATION BY: Oskar Enoksson
!
! MODIFICATIONS:
! DATE        VERS   PROGRAMMER    DESCRIPTION
!
! REFERENCES:
!
!----------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER                          :: REMOVEDUP
!
! ARGUMENTS:
!
    INTEGER,            INTENT(IN)   :: INTDATA(:) ! Data values (unchanged)
    INTEGER,            INTENT(INOUT):: IND(:) ! Permutation (changed)
!
! LOCAL VARIABLES:
!
    INTEGER N
    INTEGER VAL,LASTVAL
!
! BODY
!
    REMOVEDUP           = 0

    IF(SIZE(IND) == 0) RETURN

    REMOVEDUP           = 1
    LASTVAL             = INTDATA(IND(1))

    DO N=2,SIZE(IND)
      VAL               = INTDATA(IND(N))
      IF(VAL == LASTVAL) CYCLE

      REMOVEDUP         = REMOVEDUP+1
      IND(REMOVEDUP)    = IND(N)
      LASTVAL           = VAL
    END DO

    RETURN

  END FUNCTION REMOVEDUPINT_INDIRECT


!***********************************************************************
  FUNCTION LOOKUP_LE_INT(TABLE,SOUGHT) RESULT(IND)
!***********************************************************************
!
! FUNCTION:
! Returns the highest index IND with TABLE[IND]<=SOUGHT
! or 0 if all values in TABLE are >SOUGHT
! TABLE must be sorted in ascending order before calling this function.
!
! CREATION BY: Oskar Enoksson
!
! MODIFICATIONS:
! DATE        VERS   PROGRAMMER    DESCRIPTION
!
! REFERENCES:
!
!----------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER                          :: IND
!
! ARGUMENTS:
!
    INTEGER,            INTENT(IN)   :: TABLE(:)
    INTEGER,            INTENT(IN)   :: SOUGHT
!
! LOCAL VARIABLES:
!
    INTEGER FIRST,LAST
!
! BODY
!
    IND    = 0
    IF(SIZE(TABLE)==0)  RETURN

    FIRST  = 0
    LAST   = UBOUND(TABLE,1)
    IND    = LAST
    IF(TABLE(LAST)<=SOUGHT) RETURN

    DO WHILE(FIRST/=LAST)
      IND    = (FIRST+LAST+1)/2
      IF(TABLE(IND)<=SOUGHT) THEN
        FIRST=IND
      ELSE
        IND  =IND-1
        LAST =IND
      END IF
    END DO

    RETURN

  END FUNCTION LOOKUP_LE_INT


!***********************************************************************
  FUNCTION LOOKUP_LO_INT(TABLE,SOUGHT) RESULT(IND)
!***********************************************************************
!
! FUNCTION:
! Returns the highest index IND with TABLE[IND]<SOUGHT
! or UBOUND(TABLE)+1 if all values in TABLE are >SOUGHT
! TABLE must be sorted in ascending order before calling this function.
!
! CREATION BY: Oskar Enoksson
!
! MODIFICATIONS:
! DATE        VERS   PROGRAMMER    DESCRIPTION
!
! REFERENCES:
!
!----------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER                          :: IND
!
! ARGUMENTS:
!
    INTEGER,            INTENT(IN)   :: TABLE(:)
    INTEGER,            INTENT(IN)   :: SOUGHT
!
! LOCAL VARIABLES:
!
    INTEGER FIRST,LAST
!
! BODY
!
    IND    = 0
    IF(SIZE(TABLE)==0)  RETURN

    FIRST  = 0
    LAST   = UBOUND(TABLE,1)
    IND    = LAST
    IF(TABLE(LAST)<SOUGHT) RETURN

    DO WHILE(FIRST/=LAST)
      IND    = (FIRST+LAST+1)/2
      IF(TABLE(IND)<SOUGHT) THEN
        FIRST=IND
      ELSE
        IND  =IND-1
        LAST =IND
      END IF
    END DO

    RETURN

  END FUNCTION LOOKUP_LO_INT


!***********************************************************************
  FUNCTION LOOKUP_EQ_INT(TABLE,SOUGHT,FOUND) RESULT(IND)
!***********************************************************************
!
! FUNCTION:
! Returns the index of value SOUGHT in the array TABLE
! TABLE must be sorted in ascending order before calling this function.
!
! If the SOUGHT appears in more than one position any matching index could
! be returned.
!
! If SOUGHT is not found in TABLE the execution is stopped (STOP), unless
! the argument FOUND is present, in that case FOUND is set to .FALSE.
! and execution continues.
!
! CREATION BY: Oskar Enoksson
!
! MODIFICATIONS:
! DATE        VERS   PROGRAMMER    DESCRIPTION
!
! REFERENCES:
!
!----------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER                          :: IND
!
! ARGUMENTS:
!
    INTEGER,            INTENT(IN)   :: TABLE(:)
    INTEGER,            INTENT(IN)   :: SOUGHT
    LOGICAL,   OPTIONAL,INTENT(OUT)  :: FOUND
!
! LOCAL VARIABLES:
!
    INTEGER FIRST,LAST
    INTEGER VAL
!
! BODY
!
    FIRST = 1
    LAST  = UBOUND(TABLE,1)
    IND   = -1
    IF(PRESENT(FOUND)) FOUND=.TRUE.

    DO WHILE(FIRST<=LAST)
      IND    = (FIRST+LAST)/2
      VAL    = TABLE(IND)
      IF(VAL<SOUGHT) THEN
        FIRST=IND+1
      ELSE IF(VAL>SOUGHT) THEN
        LAST =IND-1
      ELSE
        RETURN
      END IF
    END DO

    IF(.NOT.PRESENT(FOUND)) THEN
      STOP 'LOOKUP_EQ_INT failed'
    ELSE
      FOUND = .FALSE.
    END IF

    IND=0            ! Not found !!!
    RETURN

  END FUNCTION LOOKUP_EQ_INT

END MODULE SORTINT_M
