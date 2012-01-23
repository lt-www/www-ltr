module News where

import Data.Char
import Data.List
import Data.List.Split {- split -}
import Data.Maybe
import Data.Time {- time -}
import System.Locale {- old-locale -}
import qualified WWW.Minus.RSS as R {- www-minus -}
import qualified Text.XML.Light as X {- xml -}

-- | Variant of 'splitWhen', keeping left.
--
-- > sep isDigit "1a2b3c" == ["","1a","2b","3c"]
sep :: (a -> Bool) -> [a] -> [[a]]
sep = split . keepDelimsL . whenElt

type N_Date = UTCTime
type N_Title = String
type N_Description = [String]
type N_Entry = ((N_Date,N_Title),N_Description)
type MD = [String]

-- | Heuristic to check if a line is a markdown reference definition.
--
-- > md_ln_p "[1]: http://w3.org" == True
md_ln_p :: String -> Bool
md_ln_p s =
    case s of
      i:j:_ -> i == '[' && isDigit j
      _ -> False

-- | Inverse of filter.
--
-- > remove even [1..10] == filter (not . even) [1..10]
remove :: (a -> Bool) -> [a] -> [a]
remove f = filter (not . f)

-- | 'dropWhile' from both ends of a list.
--
-- > trim isSpace " string " == "string"
trim :: (a -> Bool) -> [a] -> [a]
trim f = reverse . dropWhile f . reverse . dropWhile f

-- | Parse 'N_Date'.
--
-- > parse_date "7 january 2012"
parse_date :: String -> Maybe N_Date
parse_date = parseTime defaultTimeLocale "%e %B %Y"

sep_h :: String -> Maybe (N_Date,N_Title)
sep_h h =
    case splitOn " | " h of
      [d,t] -> case parse_date d of
                 Just d' -> Just (d',trim isSpace t)
                 Nothing -> Nothing
      _ -> Nothing

parse_hd :: String -> Maybe (N_Date,N_Title)
parse_hd h = stripPrefix "### " h >>= sep_h

parse :: String -> ([N_Entry],MD)
parse s =
    let (nw,md) = partition (not . md_ln_p) (lines s)
        e = remove null (sep (isPrefixOf "###") nw)
        f x = case trim null x of
                t:_:r -> fmap (\t' -> (t',r)) (parse_hd t)
                _ -> undefined
    in (mapMaybe f e,sort md)

n_dsc_md :: MD -> N_Description -> String
n_dsc_md md dsc = unlines (dsc ++ ["\n"] ++ md)

type N_Markup = (String -> String)

n_item :: N_Markup -> MD -> N_Entry -> X.Content
n_item f md ((dt,tt),dsc) =
    R.item [R.title tt
           ,R.pubDate_t dt
           ,R.description (f (n_dsc_md md dsc))]

rss :: N_Markup -> ([N_Entry],MD) -> X.Element
rss f (nw,md) =
    let cf = [R.title "lucie thorne: news"
             ,R.link "http://luciethorne.com/news"
             ,R.description "lucie thorne: news & updates"]
    in R.rss "2.0" [R.channel (cf ++ map (n_item f md) nw)]

rss_s :: N_Markup -> ([N_Entry], MD) -> String
rss_s f = R.render . rss f

{-
s <- readFile "/home/rohan/ut/www-ltr/data/md/news.md"
let (e,md) = parse s
writeFile "/tmp/test.xml" (R.render (rss (head . lines) (e,md)))
-}
