module News where

import qualified Text.HTML.Minus as H {- html-minus -}
import qualified WWW.Minus.RSS as RSS {- www-minus -}
import qualified WWW.Minus.RSS.News as News {- www-minus -}

import qualified LTR as L

rss :: News.N_Markup -> News.N_News -> H.Element
rss f =
    let opt = ("lucie thorne: news"
              ,"http://luciethorne.com/news"
              ,"http://luciethorne.com/news/rss.xml"
              ,"lucie thorne: news & updates"
              ,L.lt_site)
    in News.n_rss opt f

rss_s :: News.N_Markup -> News.N_News -> String
rss_s f = RSS.render . rss f

{-
s <- readFile "/home/rohan/ut/www-ltr/data/md/news.md"
let (e,md) = News.parse "###" s
writeFile "/tmp/test.xml" (RSS.render (rss (head . lines) (e,md)))
-}
