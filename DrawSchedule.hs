module DrawSchedule where

{-
import Schedule
import Dependency
import MicroOp as O

import PPrint

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ( Map )
import System.IO
import Graphics.Rendering.Cairo
import Text.PrettyPrint.HughesPJ as PP
import Control.Monad ( forM )
import Data.Maybe ( catMaybes )

import Debug.Trace
------------------------------------------------------------------------------

drawSchedulePNG :: Map Int [MicroOp] -> FilePath -> IO ()
drawSchedulePNG sched file = 
    withImageSurface FormatRGB24 width height $ \surface -> do
      renderWith surface $ do
        setSourceRGB 1 1 1
        paint
        setSourceRGB 0 0 0
        {-moveTo 0 0
        curveTo 100 100 100 300 0 400
        stroke-}
        --lineTo 
        forM coordsLst $ \(_,(_,txt, x, y)) -> do
          moveTo (10.0 + x * 80.0) (10.0 + y * 30.0)
          showText txt
        
        --let x' = 10; y' = 10
        forM coordsLst $ \(oid,(o,_,x0,y0)) -> do
          let srcs = catMaybes (map (\p -> M.lookup (pvToOp p) coords) (S.toList $ uses o))
          forM srcs $ \(_,_,x1,y1) -> do
            let x' = 10.0 + x1 * 80.0
            let y' = 10.0 + y1 * 30.0
            let x = 10.0 + x0 * 80.0
            let y = 10.0 + y0 * 30.0
            let dy = abs (y - y')
            let dx = - (dy / 6)
            let mx = (3.0*x + x') / 4.0
            let my = (3.0*y + y') / 4.0
            let mx' = (3.0*x' + x) / 4.0
            let my' = (3.0*y' + y) / 4.0
            let l = abs (y0 - y1)
            let blackness = (1 - 1 / (exp (l-1)**2))
            case oid `mod` 3 of
              0 -> setSourceRGBA blackness 0 0 0.5    
              1 -> setSourceRGBA 0 blackness 0 0.5    
              2 -> setSourceRGBA 0 0 blackness 0.5    
            moveTo x y
            --lineTo mx my
            --trace (show (x,y,x',y',mx,my)) return ()
            curveTo (mx - dx) my (mx' - dx) my' x' y'
            stroke
      surfaceWriteToPNG surface file
 where
  pvToOp (PV i _) = i
  width = 400
  height = 800
  --renderSched = 
  coordsLst = [ (mopInfo o, (o,ppMop o, dbl x, dbl y)) 
                    | (y, os) <- M.toList sched
                    , (o, x) <- zip os [0..] ]
  coords = M.fromList coordsLst
  --pp = render . pshow
  dbl = fromIntegral
  ppMop (Mop _ _ x) = case x of
      Imm c -> show c
      ALU o _ _ -> render $ pshow o
      Load w _ _ -> "L" ++ render (pshow w)
      ReadReg r -> '$' : show r
      ReadBuf b -> 'B' : show b
      ReadPC    -> "PC"
      WriteReg r _ ->  '$' : show r
      WriteBuf b _ -> 'B' : show b
      Store w _ _  -> "S"++ render (pshow w)
      MultMIPS _ _ -> "*"
      Branch _ _ _ -> "br"
      Jump _ _     -> "j"
--  ppMop (ParPair x y) = ppMop x ++ " | " ++ ppMop y 
-}