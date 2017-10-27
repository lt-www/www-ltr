import Data.List {- base -}
import Data.Function {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import qualified System.Directory.Tree as T {- directory-tree -}

import LTR

-- * Redirects

-- lt_md helper
mk_md :: [T.FileName] -> T.DirTree t -> [([T.FileName],T.FileName)]
mk_md st t =
    case t of
      T.Dir nm xs -> concatMap (mk_md (st ++ [nm])) xs
      T.File nm _ -> [(st,nm)]
      T.Failed _ _ -> []

-- | The list of markdown files (read from file system).
--
-- > md <- lt_md "/home/rohan/ut/www-ltr/data/md"
lt_md :: FilePath -> IO [([String],[String])]
lt_md dir = do
  (_ T.:/ t) <- T.readDirectoryWith return dir
  let simplify xs = let a = tail (fst (head xs))
                        xs' = map (dropExtension . snd) xs
                    in (a,xs')
      d = mk_md [] t
      d' = groupBy ((==) `on` fst) (sort d)
      d'' = map simplify d'
  return d''

-- | Flatten results of 'lt_md'.
--
-- > lt_md "/home/rohan/ut/www-ltr/data/md" >>= return.md_flatten
md_flatten :: [([String],[String])] -> [[String]]
md_flatten =
    let f (a,e) = map (\x -> a ++ [x]) e
    in concatMap f

-- | Generate full set of apache redirects.
--
-- > lt_redirects "/home/rohan/ut/www-ltr/data/md" >>= mapM_ putStrLn
lt_redirects :: FilePath -> IO [String]
lt_redirects d = do
  md <- lt_md d
  let mdf = map joinPath (md_flatten md)
      f p = printf "Redirect permanent /%s /?p=%s" p p
  return (map f mdf)

-- * Renaming

-- > map is_non_rel ["./shows","shows","?p=shows"] == [False,False,False]
-- > is_non_rel "http://luciethorne.com/?p=shows" == True
is_non_rel :: String -> Bool
is_non_rel = isPrefixOf "http://"

-- > to_non_rel "?p=shows" == "http://luciethorne.com/?p=shows"
-- > let u = "http://luciethorne.com/?p=shows" in u == to_non_rel u
to_non_rel :: String -> FilePath
to_non_rel x = if is_non_rel x then x else lt_site </> x

add_prefix :: Config -> (String,String) -> (String,String)
add_prefix c (d,t) =
    if "http://" `isPrefixOf` d
    then (d,t)
    else if head (splitDirectories d) `elem` lt_data_dirs
         then (lt_root c </> d,t)
         else (lt_base c d,t)

-- non-relative (for rss etc.)
non_rel :: (String,String) -> (String,String)
non_rel (i,j) = (to_non_rel i,to_non_rel j)
