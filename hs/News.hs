module News where

import qualified Text.XML.Light as X {- xml -}
import qualified WWW.Minus.RSS as R {- www-minus -}
import qualified WWW.Minus.RSS.News as N {- www-minus -}

import qualified LTR as L

rss :: N.N_Markup -> N.N_News -> X.Element
rss f =
    let opt = ("lucie thorne: news"
              ,"http://luciethorne.com/news"
              ,"http://luciethorne.com/news/rss.xml"
              ,"lucie thorne: news & updates"
              ,L.lt_site)
    in N.n_rss opt f

rss_s :: N.N_Markup -> N.N_News -> String
rss_s f = R.render . rss f

{-
s <- readFile "/home/rohan/ut/www-ltr/data/md/news.md"
let (e,md) = N.parse "###" s
writeFile "/tmp/test.xml" (R.render (rss (head . lines) (e,md)))
-}
