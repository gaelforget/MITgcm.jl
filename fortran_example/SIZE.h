C     SIZE.h - Grid dimensions (simplified MITgcm style)
C
C     sNx :: Number of X points in tile
C     sNy :: Number of Y points in tile
C     OLx :: Overlap extent in X (halo)
C     OLy :: Overlap extent in Y (halo)
C     Nr  :: Number of vertical levels

      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER Nr
      PARAMETER (
     &           sNx =  4,
     &           sNy =  3,
     &           OLx =  1,
     &           OLy =  1,
     &           Nr  =  2)
