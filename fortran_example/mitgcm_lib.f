C     mitgcm_lib.f - Minimal MITgcm-style library
C
C     Demonstrates the COMMON block pattern used by MITgcm

      SUBROUTINE MITGCM_INIT()
C     Initialize grid and state variables
      IMPLICIT NONE

C     Grid dimensions (like SIZE.h)
      INTEGER sNx, sNy, OLx, OLy, Nr
      PARAMETER (sNx=4, sNy=3, OLx=1, OLy=1, Nr=2)

C     Grid variables (like GRID.h)
      DOUBLE PRECISION dxC(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      DOUBLE PRECISION dyC(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      DOUBLE PRECISION drF(Nr)
      DOUBLE PRECISION xC (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      DOUBLE PRECISION yC (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      DOUBLE PRECISION rC (Nr)
      COMMON /GRID_RS/ dxC, dyC, drF, xC, yC, rC

C     Dynamic variables (like DYNVARS.h)
      DOUBLE PRECISION etaN (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      DOUBLE PRECISION uVel (1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr)
      DOUBLE PRECISION vVel (1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr)
      DOUBLE PRECISION theta(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr)
      DOUBLE PRECISION salt (1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr)
      COMMON /DYNVARS_R/ etaN, uVel, vVel, theta, salt

      INTEGER i, j, k
      DOUBLE PRECISION dx, dy, dz

C     Grid spacing
      dx = 1.0D4
      dy = 1.0D4
      dz = 50.0D0

C     Initialize grid arrays
      DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
          dxC(i,j) = dx
          dyC(i,j) = dy
          xC(i,j) = (DBLE(i) - 0.5D0) * dx
          yC(i,j) = (DBLE(j) - 0.5D0) * dy
        ENDDO
      ENDDO

      DO k = 1, Nr
        drF(k) = dz
        rC(k) = -dz * (DBLE(k) - 0.5D0)
      ENDDO

C     Initialize state variables
      DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
          etaN(i,j) = 0.0D0
        ENDDO
      ENDDO

      DO k = 1, Nr
        DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
            uVel(i,j,k) = 0.0D0
            vVel(i,j,k) = 0.0D0
            theta(i,j,k) = 20.0D0 - 5.0D0*DBLE(k-1)
            salt(i,j,k) = 35.0D0
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE MITGCM_TIMESTEP(deltaT)
C     Simple timestep: advect theta with constant velocity
      IMPLICIT NONE

C     Grid dimensions
      INTEGER sNx, sNy, OLx, OLy, Nr
      PARAMETER (sNx=4, sNy=3, OLx=1, OLy=1, Nr=2)

C     Dynamic variables
      DOUBLE PRECISION etaN (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      DOUBLE PRECISION uVel (1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr)
      DOUBLE PRECISION vVel (1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr)
      DOUBLE PRECISION theta(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr)
      DOUBLE PRECISION salt (1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr)
      COMMON /DYNVARS_R/ etaN, uVel, vVel, theta, salt

      DOUBLE PRECISION deltaT
      DOUBLE PRECISION u0
      INTEGER i, j, k

C     Simple advection: dT/dt = -u * dT/dx (upwind)
      u0 = 0.1D0
      DO k = 1, Nr
        DO j = 1, sNy
          DO i = 1, sNx
            theta(i,j,k) = theta(i,j,k)
     &        - u0 * deltaT * (theta(i,j,k) - theta(i-1,j,k)) / 1.0D4
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE GET_DIMS(nxOut, nyOut, nzOut, olxOut, olyOut)
C     Return grid dimensions to caller
      IMPLICIT NONE
      INTEGER sNx, sNy, OLx, OLy, Nr
      PARAMETER (sNx=4, sNy=3, OLx=1, OLy=1, Nr=2)

      INTEGER nxOut, nyOut, nzOut, olxOut, olyOut
      nxOut = sNx
      nyOut = sNy
      nzOut = Nr
      olxOut = OLx
      olyOut = OLy
      RETURN
      END
