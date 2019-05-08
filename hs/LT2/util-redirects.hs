import Data.List {- base -}
import Data.Function {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import qualified System.Directory.Tree as T {- directory-tree -}

-- * Redirects

-- | lt_md helper
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
