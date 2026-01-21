C     Fortran subroutines with COMMON block for shared array
C
      SUBROUTINE INITARRAY()
C     Initialize the array with values 1 to 10
      IMPLICIT NONE
      INTEGER N
      PARAMETER (N=10)
      DOUBLE PRECISION MYARRAY
      COMMON /ARRAYBLOCK/ MYARRAY(N)

      INTEGER I
      DO I = 1, N
          MYARRAY(I) = DBLE(I)
      ENDDO
      RETURN
      END

      SUBROUTINE SCALEARRAY(FACTOR)
C     Multiply all elements by a scalar factor
      IMPLICIT NONE
      INTEGER N
      PARAMETER (N=10)
      DOUBLE PRECISION MYARRAY
      COMMON /ARRAYBLOCK/ MYARRAY(N)

      DOUBLE PRECISION FACTOR
      INTEGER I
      DO I = 1, N
          MYARRAY(I) = MYARRAY(I) * FACTOR
      ENDDO
      RETURN
      END

      DOUBLE PRECISION FUNCTION SUMARRAY()
C     Return the sum of all array elements
      IMPLICIT NONE
      INTEGER N
      PARAMETER (N=10)
      DOUBLE PRECISION MYARRAY
      COMMON /ARRAYBLOCK/ MYARRAY(N)

      INTEGER I
      DOUBLE PRECISION TOTAL
      TOTAL = 0.0D0
      DO I = 1, N
          TOTAL = TOTAL + MYARRAY(I)
      ENDDO
      SUMARRAY = TOTAL
      RETURN
      END

      SUBROUTINE GETARRAY(OUTARRAY, ARRSIZE)
C     Copy the internal array to an output array
      IMPLICIT NONE
      INTEGER N
      PARAMETER (N=10)
      DOUBLE PRECISION MYARRAY
      COMMON /ARRAYBLOCK/ MYARRAY(N)

      INTEGER ARRSIZE
      DOUBLE PRECISION OUTARRAY(*)
      INTEGER I
      DO I = 1, MIN(N, ARRSIZE)
          OUTARRAY(I) = MYARRAY(I)
      ENDDO
      RETURN
      END

      SUBROUTINE SETARRAY(INARRAY, ARRSIZE)
C     Set the internal array from an input array
      IMPLICIT NONE
      INTEGER N
      PARAMETER (N=10)
      DOUBLE PRECISION MYARRAY
      COMMON /ARRAYBLOCK/ MYARRAY(N)

      INTEGER ARRSIZE
      DOUBLE PRECISION INARRAY(*)
      INTEGER I
      DO I = 1, MIN(N, ARRSIZE)
          MYARRAY(I) = INARRAY(I)
      ENDDO
      RETURN
      END
