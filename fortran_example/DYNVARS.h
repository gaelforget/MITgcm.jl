C     DYNVARS.h - Dynamic variables (simplified MITgcm style)
C
C     State Variables:
C     etaN  :: surface height anomaly (2D)
C     uVel  :: zonal velocity (3D)
C     vVel  :: meridional velocity (3D)
C     theta :: potential temperature (3D)
C     salt  :: salinity (3D)

#include "SIZE.h"

      COMMON /DYNVARS_R/
     &                   etaN,
     &                   uVel, vVel,
     &                   theta, salt

      DOUBLE PRECISION etaN (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      DOUBLE PRECISION uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      DOUBLE PRECISION vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      DOUBLE PRECISION theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      DOUBLE PRECISION salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
