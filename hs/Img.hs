module Img where

import Control.Monad
import Data.List
import Data.Maybe
import System.Directory {- directory -}
import System.FilePath {- filepath -}
import System.Process {- process -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified Text.HTML.Light.Composite.Menu as H
import qualified Text.XML.Light as X {- xml -}

type Area = String
type Id = String
type Description = String
type Class = String
type Img = (Area,[(Id,Description)])
type Neighbours = (Maybe Id,(Area,Id),Maybe Id)

-- | Initial image.
img_initial :: [Img] -> (Area,Id)
img_initial c =
    case c of
      ((x,((y,_):_)):_) -> (x,y)
      _ -> undefined

img_find :: Id -> [Img] -> [(Area,Id)]
img_find x =
    let f i (j,_) = if j == x then Just (i,j) else Nothing
    in catMaybes . concatMap (\(a,e) -> map (f a) e)

img_resize_dir :: Int -> FilePath
img_resize_dir n = "r-" ++ show n

type Renamer = FilePath -> FilePath

img_set :: (Class,Int) -> (FilePath,Renamer) -> [Img] -> Area -> X.Content
img_set (cl,rd) (rt,bs) is ar =
    let (Just a) = lookup ar is
        fn n = rt </> "rgen/photos" </> img_resize_dir rd </> n <.> "jpg"
        ln n = bs ("photos" </> n)
        cl' = H.class' cl
        f (n,_) = H.a [H.href (ln n)] [H.img [cl',H.src (fn n),H.alt n]]
    in H.div [cl'] (map f a)

img_preview :: (FilePath,Renamer) -> [Img] -> Area -> X.Content
img_preview = img_set ("img-preview",60)

img_preload :: Int -> (FilePath,Renamer) -> [Img] -> Area -> X.Content
img_preload sz = img_set ("img-preload",sz)

img_no_preload :: X.Content
img_no_preload = H.div [H.class' "img-preload"] []

img_submenu :: Renamer -> [Img] -> X.Content
img_submenu p d =
    let adr i = p ("photos" </> i)
        f (n,((i,_):_)) = (n,i,Just (adr i))
        f (_,[]) = undefined
    in H.nav_menu_span id "submenu" (map f d) ""

sets :: [(a,b)] -> [(Maybe a,(a,b),Maybe a)]
sets n =
    let n' = map fst n
        n'' = map Just n'
    in zip3 (Nothing : n'') n (drop 1 n'' ++ [Nothing])

img_neighbours :: [Img] -> (Area,Id) -> Neighbours
img_neighbours is (a,x) =
    let Just s = find (\(a',_) -> a' == a) is
        s' = sets (snd s)
        Just r = find (\(_,(x',_),_) -> x' == x) s'
    in r

img_mk_reduction :: FilePath -> String -> IO ()
img_mk_reduction rt nm = do
  let sz = [60,80,400,450,500]::[Int]
      sz' = map show sz
      d = rt </> "data/image/photos"
      i_fn = d </> nm <.> "jpg"
      o_fd n = rt </> "rgen/photos/r-"++n
      o_fn n = o_fd n </> nm <.> "jpg"
      mk n = do
        e <- doesFileExist (o_fn n)
        when (not e)
             (rawSystem "mkdir" ["-p",o_fd n] >>
              rawSystem "convert" ["-resize","x"++n,i_fn,o_fn n] >>
              return ())
  mapM_ mk sz'

