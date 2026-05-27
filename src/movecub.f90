   FUNCTION MoveCub(iCub,iX,iY,iZ,nR) 

   IMPLICIT NONE

   INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(14)
   INTEGER, INTENT(IN) :: iCub, iX, iY, iZ
   INTEGER, DIMENSION(3), INTENT(IN) :: nR 
   INTEGER :: MoveCub, iM2 
   INTEGER :: iC, XCub, YCub, ZCub, XCubN, YCubN, ZCubN

   iM2 = nR(1)*nR(2)

   ! Find the "coordinates" of the block on the mesh
   iC = iCub - 1
   ZCub = INT(iC/iM2) + 1 
   YCub = INT(MOD(iC,iM2)/nR(1)) + 1 
   XCub = MOD(MOD(iC,iM2),nR(1)) + 1 

   XCubN = XCub + iX
   if(XCubN.lt.1) XCubN = Mod(XCubN,nR(1)) + nR(1) 
   if(XCubN.gt.nR(1)) XCubN = Mod(XCubN,nR(1))  

   YCubN = YCub + iY
   if(YCubN.lt.1) YCubN = Mod(YCubN,nR(2)) + nR(2) 
   if(YCubN.gt.nR(2)) YCubN = Mod(YCubN,nR(2)) 

   ZCubN = ZCub + iZ
   if(ZCubN.lt.1) ZCubN = Mod(ZCubN,nR(3)) + nR(3)
   if(ZCubN.gt.nR(3)) ZCubN = Mod(ZCubN,nR(3)) 

   MoveCub = iM2*(ZCubN-1) + nR(1)*(YCubN-1) + XCubN
  end 
