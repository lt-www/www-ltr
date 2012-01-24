module News where

import Data.Char
import Data.List
import Data.List.Split {- split -}
import Data.Maybe
import Data.Time {- time -}
import System.Locale {- old-locale -}
import qualified WWW.Minus.RSS as R {- www-minus -}
import qualified Text.XML.Light as X {- xml -}

import qualified LTR as L

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
type N_News = ([N_Entry],MD)

-- | Heuristic to check if a line is a markdown reference definition.
--
-- > md_ln_p "[1]: http://w3.org" == True
md_ln_p :: String -> Bool
md_ln_p s =
    case s of
      i:j:_ -> i == '[' && isDigit j
      _ -> False

-- | Apply 'not' to function at 'filter'.
--
-- > remove isDigit "0one2" == "one"
-- > remove even [1..10] == filter (not . even) [1..10]
remove :: (a -> Bool) -> [a] -> [a]
remove f = filter (not . f)

-- | 'dropWhile' from both ends of a list.
--
-- > trim isSpace " string " == "string"
trim :: (a -> Bool) -> [a] -> [a]
trim f = reverse . dropWhile f . reverse . dropWhile f

lt_date_format_str :: [Char]
lt_date_format_str = "%e %B %Y"

-- | Parse 'N_Date'.
--
-- > parse_lt_news_date "7 january 2012"
parse_lt_news_date :: String -> Maybe N_Date
parse_lt_news_date = parseTime defaultTimeLocale lt_date_format_str

-- > fmap format_lt_news_date (parse_iso_8601_date "2012-01-07")
format_lt_news_date :: N_Date -> String
format_lt_news_date = formatTime defaultTimeLocale lt_date_format_str

iso_8601_date_format_str :: String
iso_8601_date_format_str = "%Y-%m-%d"

-- > parse_iso_8601_date "2012-01-07"
parse_iso_8601_date :: String -> Maybe N_Date
parse_iso_8601_date = parseTime defaultTimeLocale iso_8601_date_format_str

format_iso_8601_date :: N_Date -> String
format_iso_8601_date = formatTime defaultTimeLocale iso_8601_date_format_str

-- > sep_h "1 january 2012 | New News"
sep_h :: String -> Maybe (N_Date,N_Title)
sep_h h =
    case splitOn " | " h of
      [d,t] -> case parse_lt_news_date d of
                 Just d' -> Just (d',trim isSpace t)
                 Nothing -> Nothing
      _ -> Nothing

-- > parse_hd "### 1 january 2012 | New News"
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

n_entry_md :: MD -> N_Entry -> String
n_entry_md md ((d,t),dsc) =
    let h = "### " ++ format_lt_news_date d ++ " | " ++ t
        b = n_dsc_md md dsc
    in unlines [h,"",b]

-- | Type to transform entry markup.
type N_Markup = (String -> String)

n_item :: N_Markup -> MD -> N_Entry -> X.Content
n_item f md ((dt,tt),dsc) =
    R.item [R.title tt
           ,R.pubDate_t dt
           ,R.guid_ln (L.lt_site ++ "?n=" ++ format_iso_8601_date dt)
           ,R.description_cdata (f (n_dsc_md md dsc))]

rss :: N_Markup -> N_News -> X.Element
rss f (nw,md) =
    let cf = [R.title "lucie thorne: news"
             ,R.link "http://luciethorne.com/news"
             ,R.atom_link_url "http://luciethorne.com/news/rss.xml"
             ,R.description "lucie thorne: news & updates"]
    in R.rss "2.0" [R.channel (cf ++ map (n_item f md) nw)]

rss_s :: N_Markup -> N_News -> String
rss_s f = R.render . rss f

entry_by_date :: N_Date -> [N_Entry] -> Maybe N_Entry
entry_by_date d = find (\((d',_),_) -> d' == d)

entry_by_date_s :: String -> [N_Entry] -> Maybe N_Entry
entry_by_date_s d e = parse_iso_8601_date d >>= flip entry_by_date e

{-
s <- readFile "/home/rohan/ut/www-ltr/data/md/news.md"
let (e,md) = parse s
writeFile "/tmp/test.xml" (R.render (rss (head . lines) (e,md)))
-}
