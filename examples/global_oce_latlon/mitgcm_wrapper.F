C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C
C     mitgcm_wrapper.F - Library wrapper for MITgcm
C
C     Provides callable subroutines to drive MITgcm from Julia:
C       MITGCM_LIB_INIT      - Initialize model (boot + fixed + varia)
C       MITGCM_LIB_STEP       - One forward time step
C       MITGCM_LIB_FINALIZE   - Shut down
C       MITGCM_LIB_GET_DIMS   - Return grid dimensions
C       MITGCM_LIB_GET_NITER  - Return current iteration number
C       MITGCM_LIB_GET_TIME   - Return current model time
C       MITGCM_LIB_GET_*      - Copy prognostic/grid fields to global arrays
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MITGCM_LIB_INIT
C     !INTERFACE:
      SUBROUTINE MITGCM_LIB_INIT()

C     !DESCRIPTION:
C     Initialize MITgcm in library mode.
C     Calls EEBOOT (execution environment), INITIALISE_FIXED (grid,
C     parameters), and INITIALISE_VARIA (initial conditions).

C     !USES:
      IMPLICIT NONE

C     == Global variables (must include all COMMON blocks from main.F) ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SURFACE.h"

C     Library state tracking
      INTEGER myThid_lib
      INTEGER myIter_lib
      _RL     myTime_lib
      INTEGER iloop_lib
      COMMON /MITGCM_LIB_STATE/ myThid_lib, myIter_lib,
     &                          myTime_lib, iloop_lib

C     !LOCAL VARIABLES:
      INTEGER dummyComm
CEOP

C--   Boot execution environment
      dummyComm = -1
      CALL EEBOOT( dummyComm )

C--   Trap boot errors
      IF ( eeBootError ) THEN
        WRITE(*,*) 'MITGCM_LIB_INIT: EEBOOT failed'
        RETURN
      ENDIF

C--   Set thread number for library mode (single-thread)
      myThid_lib = 1

C--   Validate thread setup (sets threadIsRunning flag)
      CALL CHECK_THREADS( myThid_lib )

C--   Mark thread as complete (needed by EEDIE)
      threadIsComplete(myThid_lib) = .TRUE.

C--   Initialize fixed model configuration (grid, params, solver)
      CALL INITIALISE_FIXED( myThid_lib )

C--   Initialize variable model state (ICs, forcing, etc.)
      CALL INITIALISE_VARIA( myThid_lib )

C--   Set initial time and iteration from model parameters
      myTime_lib = startTime
      myIter_lib = nIter0
      iloop_lib  = 0

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MITGCM_LIB_STEP
C     !INTERFACE:
      SUBROUTINE MITGCM_LIB_STEP()

C     !DESCRIPTION:
C     Advance MITgcm by one forward time step.
C     Calls FORWARD_STEP (via MAIN_DO_LOOP).

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     Library state tracking
      INTEGER myThid_lib
      INTEGER myIter_lib
      _RL     myTime_lib
      INTEGER iloop_lib
      COMMON /MITGCM_LIB_STATE/ myThid_lib, myIter_lib,
     &                          myTime_lib, iloop_lib
CEOP

C--   Increment loop counter
      iloop_lib = iloop_lib + 1

C--   Call forward step
      CALL FORWARD_STEP( iloop_lib, myTime_lib,
     &                   myIter_lib, myThid_lib )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MITGCM_LIB_FINALIZE
C     !INTERFACE:
      SUBROUTINE MITGCM_LIB_FINALIZE()

C     !DESCRIPTION:
C     Shut down MITgcm library mode.
C     Closes all Fortran file units so the library can be
C     re-initialized in the same process (e.g. Julia session).

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"

C     Library state tracking
      INTEGER myThid_lib
      INTEGER myIter_lib
      _RL     myTime_lib
      INTEGER iloop_lib
      COMMON /MITGCM_LIB_STATE/ myThid_lib, myIter_lib,
     &                          myTime_lib, iloop_lib

C     !LOCAL VARIABLES:
      INTEGER iUnit
      LOGICAL isOpen
#ifdef HAVE_NETCDF
#include "netcdf.inc"
      INTEGER ncId, ncStat
#endif
CEOP

C--   Print timing statistics
      CALL TIMER_PRINTALL( myThid_lib )

#ifdef HAVE_NETCDF
C--   Close all open NetCDF file handles (packages like PROFILES
C     use NF_OPEN which EEDIE does not clean up)
      DO ncId = 1, 1024
        ncStat = NF_CLOSE( ncId )
      ENDDO
#endif

C--   Close all open Fortran file units (skip stdin=5, stdout=6, stderr=0)
      DO iUnit = 7, 999
        INQUIRE( UNIT=iUnit, OPENED=isOpen )
        IF ( isOpen ) CLOSE( iUnit )
      ENDDO

C--   Shut down execution environment
      CALL EEDIE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MITGCM_LIB_GET_DIMS
C     !INTERFACE:
      SUBROUTINE MITGCM_LIB_GET_DIMS(
     O     sNx_out, sNy_out, Nr_out,
     O     OLx_out, OLy_out,
     O     nSx_out, nSy_out,
     O     nPx_out, nPy_out,
     O     Nx_out, Ny_out )

C     !DESCRIPTION:
C     Return grid dimensions to caller.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"

C     !OUTPUT PARAMETERS:
      INTEGER sNx_out, sNy_out, Nr_out
      INTEGER OLx_out, OLy_out
      INTEGER nSx_out, nSy_out
      INTEGER nPx_out, nPy_out
      INTEGER Nx_out, Ny_out
CEOP

      sNx_out = sNx
      sNy_out = sNy
      Nr_out  = Nr
      OLx_out = OLx
      OLy_out = OLy
      nSx_out = nSx
      nSy_out = nSy
      nPx_out = nPx
      nPy_out = nPy
      Nx_out  = Nx
      Ny_out  = Ny

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MITGCM_LIB_GET_NITER
C     !INTERFACE:
      SUBROUTINE MITGCM_LIB_GET_NITER( nIter_out )

C     !DESCRIPTION:
C     Return current iteration number.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     Library state tracking
      INTEGER myThid_lib
      INTEGER myIter_lib
      _RL     myTime_lib
      INTEGER iloop_lib
      COMMON /MITGCM_LIB_STATE/ myThid_lib, myIter_lib,
     &                          myTime_lib, iloop_lib

C     !OUTPUT PARAMETERS:
      INTEGER nIter_out
CEOP

      nIter_out = myIter_lib

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MITGCM_LIB_GET_TIME
C     !INTERFACE:
      SUBROUTINE MITGCM_LIB_GET_TIME( time_out )

C     !DESCRIPTION:
C     Return current model time (seconds).

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     Library state tracking
      INTEGER myThid_lib
      INTEGER myIter_lib
      _RL     myTime_lib
      INTEGER iloop_lib
      COMMON /MITGCM_LIB_STATE/ myThid_lib, myIter_lib,
     &                          myTime_lib, iloop_lib

C     !OUTPUT PARAMETERS:
      _RL time_out
CEOP

      time_out = myTime_lib

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Field getter subroutines
C     These assemble tile data into global (Nx, Ny, ...) arrays,
C     extracting only interior points (no halos).
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_THETA( arr )
C     Copy potential temperature to a global (Nx,Ny,Nr) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
      _RL arr(Nx, Ny, Nr)
      INTEGER bi, bj, i, j, k, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO k = 1, Nr
            DO j = 1, sNy
              DO i = 1, sNx
                ig = sNx*(bi-1) + i
                jg = sNy*(bj-1) + j
                arr(ig, jg, k) = theta(i, j, k, bi, bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_SALT( arr )
C     Copy salinity to a global (Nx,Ny,Nr) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
      _RL arr(Nx, Ny, Nr)
      INTEGER bi, bj, i, j, k, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO k = 1, Nr
            DO j = 1, sNy
              DO i = 1, sNx
                ig = sNx*(bi-1) + i
                jg = sNy*(bj-1) + j
                arr(ig, jg, k) = salt(i, j, k, bi, bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_UVEL( arr )
C     Copy zonal velocity to a global (Nx,Ny,Nr) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
      _RL arr(Nx, Ny, Nr)
      INTEGER bi, bj, i, j, k, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO k = 1, Nr
            DO j = 1, sNy
              DO i = 1, sNx
                ig = sNx*(bi-1) + i
                jg = sNy*(bj-1) + j
                arr(ig, jg, k) = uVel(i, j, k, bi, bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_VVEL( arr )
C     Copy meridional velocity to a global (Nx,Ny,Nr) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
      _RL arr(Nx, Ny, Nr)
      INTEGER bi, bj, i, j, k, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO k = 1, Nr
            DO j = 1, sNy
              DO i = 1, sNx
                ig = sNx*(bi-1) + i
                jg = sNy*(bj-1) + j
                arr(ig, jg, k) = vVel(i, j, k, bi, bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_WVEL( arr )
C     Copy vertical velocity to a global (Nx,Ny,Nr) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
      _RL arr(Nx, Ny, Nr)
      INTEGER bi, bj, i, j, k, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO k = 1, Nr
            DO j = 1, sNy
              DO i = 1, sNx
                ig = sNx*(bi-1) + i
                jg = sNy*(bj-1) + j
                arr(ig, jg, k) = wVel(i, j, k, bi, bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_ETAN( arr )
C     Copy free-surface anomaly to a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              arr(ig, jg) = etaN(i, j, bi, bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Grid field getters (note: grid vars are _RS precision)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_XC( arr )
C     Copy cell-center X-coordinate to a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              arr(ig, jg) = xC(i, j, bi, bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_YC( arr )
C     Copy cell-center Y-coordinate to a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              arr(ig, jg) = yC(i, j, bi, bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_RC( arr )
C     Copy vertical level centers to a (Nr) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
      _RL arr(Nr)
      INTEGER k

      DO k = 1, Nr
        arr(k) = rC(k)
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_DRF( arr )
C     Copy vertical level thicknesses to a (Nr) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
      _RL arr(Nr)
      INTEGER k

      DO k = 1, Nr
        arr(k) = drF(k)
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_HFACC( arr )
C     Copy hFacC (cell fraction) to a global (Nx,Ny,Nr) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
      _RL arr(Nx, Ny, Nr)
      INTEGER bi, bj, i, j, k, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO k = 1, Nr
            DO j = 1, sNy
              DO i = 1, sNx
                ig = sNx*(bi-1) + i
                jg = sNy*(bj-1) + j
                arr(ig, jg, k) = hFacC(i, j, k, bi, bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_RLOW( arr )
C     Copy bathymetry (R_low) to a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              arr(ig, jg) = R_low(i, j, bi, bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Setter subroutines - for coupling, allow Julia to modify state
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_SET_THETA( arr )
C     Set potential temperature from a global (Nx,Ny,Nr) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
      _RL arr(Nx, Ny, Nr)
      INTEGER bi, bj, i, j, k, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO k = 1, Nr
            DO j = 1, sNy
              DO i = 1, sNx
                ig = sNx*(bi-1) + i
                jg = sNy*(bj-1) + j
                theta(i, j, k, bi, bj) = arr(ig, jg, k)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_SET_SALT( arr )
C     Set salinity from a global (Nx,Ny,Nr) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
      _RL arr(Nx, Ny, Nr)
      INTEGER bi, bj, i, j, k, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO k = 1, Nr
            DO j = 1, sNy
              DO i = 1, sNx
                ig = sNx*(bi-1) + i
                jg = sNy*(bj-1) + j
                salt(i, j, k, bi, bj) = arr(ig, jg, k)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Surface forcing field getters/setters
C     These allow Julia to read and modify surface boundary conditions:
C       fu       - zonal wind stress (N/m^2)
C       fv       - meridional wind stress (N/m^2)
C       Qnet     - net surface heat flux (W/m^2, positive=cooling)
C       EmPmR    - freshwater flux (kg/m^2/s)
C       Qsw      - shortwave radiation (W/m^2)
C       saltFlux - direct salt flux (g/m^2/s)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_FU( arr )
C     Copy zonal wind stress to a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              arr(ig, jg) = fu(i, j, bi, bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_SET_FU( arr )
C     Set zonal wind stress from a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              fu(i, j, bi, bj) = arr(ig, jg)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_FV( arr )
C     Copy meridional wind stress to a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              arr(ig, jg) = fv(i, j, bi, bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_SET_FV( arr )
C     Set meridional wind stress from a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              fv(i, j, bi, bj) = arr(ig, jg)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_QNET( arr )
C     Copy net surface heat flux to a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              arr(ig, jg) = Qnet(i, j, bi, bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_SET_QNET( arr )
C     Set net surface heat flux from a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              Qnet(i, j, bi, bj) = arr(ig, jg)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_EMPMR( arr )
C     Copy freshwater flux (E-P-R) to a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              arr(ig, jg) = EmPmR(i, j, bi, bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_SET_EMPMR( arr )
C     Set freshwater flux (E-P-R) from a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              EmPmR(i, j, bi, bj) = arr(ig, jg)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_QSW( arr )
C     Copy shortwave radiation to a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              arr(ig, jg) = Qsw(i, j, bi, bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_SET_QSW( arr )
C     Set shortwave radiation from a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              Qsw(i, j, bi, bj) = arr(ig, jg)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_GET_SALTFLUX( arr )
C     Copy salt flux to a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              arr(ig, jg) = saltFlux(i, j, bi, bj)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MITGCM_LIB_SET_SALTFLUX( arr )
C     Set salt flux from a global (Nx,Ny) array
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
      _RL arr(Nx, Ny)
      INTEGER bi, bj, i, j, ig, jg

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO j = 1, sNy
            DO i = 1, sNx
              ig = sNx*(bi-1) + i
              jg = sNy*(bj-1) + j
              saltFlux(i, j, bi, bj) = arr(ig, jg)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END
