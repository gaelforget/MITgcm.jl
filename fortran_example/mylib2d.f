C     Fortran subroutines with COMMON block for 2D array
C
      SUBROUTINE INITARRAY2D()
C     Initialize the 2D array
      IMPLICIT NONE
      INTEGER NX, NY
      PARAMETER (NX=4, NY=3)
      DOUBLE PRECISION MYARRAY
      COMMON /ARRAYBLOCK2D/ MYARRAY(NX, NY)

      INTEGER I, J
      DO J = 1, NY
          DO I = 1, NX
              MYARRAY(I, J) = DBLE(I + (J-1)*NX)
          ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE SCALEARRAY2D(FACTOR)
C     Multiply all elements by a scalar factor
      IMPLICIT NONE
      INTEGER NX, NY
      PARAMETER (NX=4, NY=3)
      DOUBLE PRECISION MYARRAY
      COMMON /ARRAYBLOCK2D/ MYARRAY(NX, NY)

      DOUBLE PRECISION FACTOR
      INTEGER I, J
      DO J = 1, NY
          DO I = 1, NX
              MYARRAY(I, J) = MYARRAY(I, J) * FACTOR
          ENDDO
      ENDDO
      RETURN
      END

      DOUBLE PRECISION FUNCTION SUMARRAY2D()
C     Return the sum of all array elements
      IMPLICIT NONE
      INTEGER NX, NY
      PARAMETER (NX=4, NY=3)
      DOUBLE PRECISION MYARRAY
      COMMON /ARRAYBLOCK2D/ MYARRAY(NX, NY)

      INTEGER I, J
      DOUBLE PRECISION TOTAL
      TOTAL = 0.0D0
      DO J = 1, NY
          DO I = 1, NX
              TOTAL = TOTAL + MYARRAY(I, J)
          ENDDO
      ENDDO
      SUMARRAY2D = TOTAL
      RETURN
      END

      SUBROUTINE GETARRAY2D(OUTARRAY, M, N)
C     Copy the internal 2D array to an output array
      IMPLICIT NONE
      INTEGER NX, NY
      PARAMETER (NX=4, NY=3)
      DOUBLE PRECISION MYARRAY
      COMMON /ARRAYBLOCK2D/ MYARRAY(NX, NY)

      INTEGER M, N
      DOUBLE PRECISION OUTARRAY(M, N)
      INTEGER I, J
      DO J = 1, MIN(NY, N)
          DO I = 1, MIN(NX, M)
              OUTARRAY(I, J) = MYARRAY(I, J)
          ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE SETARRAY2D(INARRAY, M, N)
C     Set the internal 2D array from an input array
      IMPLICIT NONE
      INTEGER NX, NY
      PARAMETER (NX=4, NY=3)
      DOUBLE PRECISION MYARRAY
      COMMON /ARRAYBLOCK2D/ MYARRAY(NX, NY)

      INTEGER M, N
      DOUBLE PRECISION INARRAY(M, N)
      INTEGER I, J
      DO J = 1, MIN(NY, N)
          DO I = 1, MIN(NX, M)
              MYARRAY(I, J) = INARRAY(I, J)
          ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE GETDIMS(M, N)
C     Return the dimensions of the array
      IMPLICIT NONE
      INTEGER NX, NY
      PARAMETER (NX=4, NY=3)
      INTEGER M, N
      M = NX
      N = NY
      RETURN
      END
