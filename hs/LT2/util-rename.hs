import Data.List {- base -}
import System.FilePath {- filepath -}

import qualified LT2.LTR as LT2

-- * Renaming

-- > map is_non_rel ["./shows","shows","?p=shows"] == [False,False,False]
-- > is_non_rel "http://luciethorne.com/?p=shows" == True
is_non_rel :: String -> Bool
is_non_rel = isPrefixOf "http://"

-- > to_non_rel "?p=shows" == "http://luciethorne.com/?p=shows"
-- > let u = "http://luciethorne.com/?p=shows" in u == to_non_rel u
to_non_rel :: String -> FilePath
to_non_rel x = if is_non_rel x then x else LT2.lt_site </> x

add_prefix :: LT2.Config -> (String,String) -> (String,String)
add_prefix c (d,t) =
    if "http://" `isPrefixOf` d
    then (d,t)
    else if head (splitDirectories d) `elem` LT2.lt_data_dirs
         then (LT2.lt_root c </> d,t)
         else (LT2.lt_base c d,t)

-- non-relative (for rss etc.)
non_rel :: (String,String) -> (String,String)
non_rel (i,j) = (to_non_rel i,to_non_rel j)
