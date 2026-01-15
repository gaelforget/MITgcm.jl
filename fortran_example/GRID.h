C     GRID.h - Grid variables (simplified MITgcm style)
C
C     dxC  :: Cell center separation in X (m)
C     dyC  :: Cell center separation in Y (m)
C     drF  :: Cell face separation in Z (m)
C     xC   :: X-coordinate of cell center
C     yC   :: Y-coordinate of cell center
C     rC   :: R-coordinate of cell center (depth)

#include "SIZE.h"

      COMMON /GRID_RS/
     &  dxC, dyC, drF,
     &  xC, yC, rC

      DOUBLE PRECISION dxC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      DOUBLE PRECISION dyC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      DOUBLE PRECISION drF (Nr)
      DOUBLE PRECISION xC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      DOUBLE PRECISION yC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      DOUBLE PRECISION rC  (Nr)
