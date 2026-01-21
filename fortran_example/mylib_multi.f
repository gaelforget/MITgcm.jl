C     Fortran subroutines with multiple arrays in COMMON block
C     Arrays can be selected by name (character) or index (integer)
C
      BLOCK DATA INITCOMMON
C     Initialize array names
      IMPLICIT NONE
      INTEGER NUMARRAYS
      PARAMETER (NUMARRAYS=3)
      CHARACTER*16 ARRAYNAMES(NUMARRAYS)
      COMMON /NAMELIST/ ARRAYNAMES
      DATA ARRAYNAMES /'temperature', 'velocity', 'pressure'/
      END

      SUBROUTINE GETARRAYINDEX(NAME, IDX)
C     Convert array name to index (1-based), returns 0 if not found
      IMPLICIT NONE
      INTEGER NUMARRAYS
      PARAMETER (NUMARRAYS=3)
      CHARACTER*16 ARRAYNAMES(NUMARRAYS)
      COMMON /NAMELIST/ ARRAYNAMES

      CHARACTER*(*) NAME
      INTEGER IDX, I

      IDX = 0
      DO I = 1, NUMARRAYS
          IF (TRIM(NAME) .EQ. TRIM(ARRAYNAMES(I))) THEN
              IDX = I
              RETURN
          ENDIF
      ENDDO
      RETURN
      END

      SUBROUTINE INITARRAYS()
C     Initialize all arrays with default values
      IMPLICIT NONE
      INTEGER NX, NY, NUMARRAYS
      PARAMETER (NX=4, NY=3, NUMARRAYS=3)
      DOUBLE PRECISION TEMPERATURE(NX, NY)
      DOUBLE PRECISION VELOCITY(NX, NY)
      DOUBLE PRECISION PRESSURE(NX, NY)
      COMMON /DATABLOCK/ TEMPERATURE, VELOCITY, PRESSURE

      INTEGER I, J
      DO J = 1, NY
          DO I = 1, NX
              TEMPERATURE(I,J) = 273.15D0 + DBLE(I+J)
              VELOCITY(I,J) = DBLE(I) * 0.1D0
              PRESSURE(I,J) = 101325.0D0 + DBLE(J) * 100.0D0
          ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE GETARRAYBYIDX(IDX, OUTARRAY, M, N)
C     Get array by index (1=temperature, 2=velocity, 3=pressure)
      IMPLICIT NONE
      INTEGER NX, NY
      PARAMETER (NX=4, NY=3)
      DOUBLE PRECISION TEMPERATURE(NX, NY)
      DOUBLE PRECISION VELOCITY(NX, NY)
      DOUBLE PRECISION PRESSURE(NX, NY)
      COMMON /DATABLOCK/ TEMPERATURE, VELOCITY, PRESSURE

      INTEGER IDX, M, N, I, J
      DOUBLE PRECISION OUTARRAY(M, N)

      DO J = 1, MIN(NY, N)
          DO I = 1, MIN(NX, M)
              IF (IDX .EQ. 1) THEN
                  OUTARRAY(I,J) = TEMPERATURE(I,J)
              ELSE IF (IDX .EQ. 2) THEN
                  OUTARRAY(I,J) = VELOCITY(I,J)
              ELSE IF (IDX .EQ. 3) THEN
                  OUTARRAY(I,J) = PRESSURE(I,J)
              ENDIF
          ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE SETARRAYBYIDX(IDX, INARRAY, M, N)
C     Set array by index
      IMPLICIT NONE
      INTEGER NX, NY
      PARAMETER (NX=4, NY=3)
      DOUBLE PRECISION TEMPERATURE(NX, NY)
      DOUBLE PRECISION VELOCITY(NX, NY)
      DOUBLE PRECISION PRESSURE(NX, NY)
      COMMON /DATABLOCK/ TEMPERATURE, VELOCITY, PRESSURE

      INTEGER IDX, M, N, I, J
      DOUBLE PRECISION INARRAY(M, N)

      DO J = 1, MIN(NY, N)
          DO I = 1, MIN(NX, M)
              IF (IDX .EQ. 1) THEN
                  TEMPERATURE(I,J) = INARRAY(I,J)
              ELSE IF (IDX .EQ. 2) THEN
                  VELOCITY(I,J) = INARRAY(I,J)
              ELSE IF (IDX .EQ. 3) THEN
                  PRESSURE(I,J) = INARRAY(I,J)
              ENDIF
          ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE SCALEARRAYBYIDX(IDX, FACTOR)
C     Scale array by index
      IMPLICIT NONE
      INTEGER NX, NY
      PARAMETER (NX=4, NY=3)
      DOUBLE PRECISION TEMPERATURE(NX, NY)
      DOUBLE PRECISION VELOCITY(NX, NY)
      DOUBLE PRECISION PRESSURE(NX, NY)
      COMMON /DATABLOCK/ TEMPERATURE, VELOCITY, PRESSURE

      INTEGER IDX, I, J
      DOUBLE PRECISION FACTOR

      DO J = 1, NY
          DO I = 1, NX
              IF (IDX .EQ. 1) THEN
                  TEMPERATURE(I,J) = TEMPERATURE(I,J) * FACTOR
              ELSE IF (IDX .EQ. 2) THEN
                  VELOCITY(I,J) = VELOCITY(I,J) * FACTOR
              ELSE IF (IDX .EQ. 3) THEN
                  PRESSURE(I,J) = PRESSURE(I,J) * FACTOR
              ENDIF
          ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE GETNUMARRAYS(N)
C     Return number of arrays
      IMPLICIT NONE
      INTEGER N
      N = 3
      RETURN
      END

      SUBROUTINE GETARRAYNAME(IDX, NAME)
C     Return name of array at index
      IMPLICIT NONE
      INTEGER NUMARRAYS
      PARAMETER (NUMARRAYS=3)
      CHARACTER*16 ARRAYNAMES(NUMARRAYS)
      COMMON /NAMELIST/ ARRAYNAMES

      INTEGER IDX
      CHARACTER*16 NAME

      IF (IDX .GE. 1 .AND. IDX .LE. NUMARRAYS) THEN
          NAME = ARRAYNAMES(IDX)
      ELSE
          NAME = ''
      ENDIF
      RETURN
      END

      SUBROUTINE GETDIMS(M, N)
C     Return array dimensions
      IMPLICIT NONE
      INTEGER M, N
      M = 4
      N = 3
      RETURN
      END
