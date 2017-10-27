-- import Data.List {- base -}
import System.FilePath {- filepath -}

{-
import qualified Network.CGI as C {- cgi -}
import qualified Network.CGI.Protocol as C {- cgi -}
import qualified WWW.Minus.CGI.Editor as E {- www-minus -}
import qualified WWW.Minus.RSS.News as RSS {- www-minus -}
-}

import qualified Text.HTML.Minus as H {- html-minus -}

import qualified WWW.Minus.CGI as CGI {- www-minus -}
import qualified WWW.Minus.IO as IO {- www-minus -}

import qualified Img as I
import qualified LTR as L
--import qualified News as News
--import qualified Pwd as Pwd

md_page :: L.Config -> [FilePath] -> IO ()
md_page cf p = do
  let fn = L.lt_markdown_file_name p
  md <- IO.read_file_utf8 fn
  h <- L.lt_markdown_to_html md
  let h' = L.lt_std_html cf p (H.cdata_raw h)
  CGI.utf8_html_output (H.renderHTML5 h')

ph_page_def :: L.Config -> IO ()
ph_page_def cf = do
  cs <- L.lt_img_data cf
  let i = I.img_initial cs
  h <- L.lt_photo_page_io cf i
  CGI.utf8_html_output h

ph_page_i :: L.Config -> I.Id -> IO ()
ph_page_i cf i = do
  h <- L.lt_photo_page_io_id cf i
  CGI.utf8_html_output h

ph_page :: L.Config -> [I.Id] -> IO ()
ph_page cf x =
    case x of
      i:_ -> ph_page_i cf i
      _ -> ph_page_def cf

d_page :: L.Config -> [String] -> IO ()
d_page cf d =
    case d of
      --["news","rss.xml"] -> rss_news cf
      "photos":i -> ph_page cf i
      _ -> md_page cf d

mk_viewer :: String -> H.Content
mk_viewer v =
    let v' = "https://www.youtube.com/embed/" ++ v
        o_a = [H.width "425", H.height "344"]
        o_c = [H.param [H.name "movie"
                       ,H.value v']
              ,H.param [H.name "allowFullScreen"
                       ,H.value "true"]
              ,H.param [H.name "allowscriptaccess"
                       ,H.value "always"]
              ,H.iframe [H.src v'
                        ,H.frameborder "0"
                        ,H.mk_attr "allowfullscreen" "true"
                        ,H.width "560"
                        ,H.height "315"] []]
        o = H.object o_a o_c
    in H.div [H.class_attr "viewer"] [o]

v_page :: L.Config -> String -> IO ()
v_page cf d = do
  let h' = L.lt_std_html cf ["?v="++d] (mk_viewer d)
  CGI.utf8_html_output (H.renderHTML5 h')

resize_get :: L.Config -> IO ()
resize_get cf = do
  L.lt_img_reductions cf
  return ()

{-
e_config :: E.Config
e_config =
    let v = Just (E.Git,("lucie thorne <lucie@luciethorne.com>",Just "sp"))
    in E.Config {E.cfg_vcs = v
                ,E.cfg_url = "http://luciethorne.com"
                ,E.cfg_pwd = Just Pwd.lt_pwd}

photos_post :: E.Config -> L.Config -> IO ()
photos_post e cf = do
  r <- E.edit_post e "/"
  C.liftIO (L.lt_img_reductions cf)
  return r

require_verified :: E.Config -> W.Query -> IO () -> IO ()
require_verified e q y = do
  v <- E.validated e
  if v then y else E.login_get (("o","login"):q)

rss_news :: L.Config -> IO ()
rss_news _ = do
  s <- L.read_file_or "" "data/md/news.md"
  let n = RSS.parse "###" s
  f <- L.lt_markdown_to_html 
  let x = News.rss_s f n
  CGI.utf8_rss_output x

news_d :: L.Config -> String -> IO ()
news_d cf d = do
  s <- L.read_file_or "" "data/md/news.md"
  let (e,md) = RSS.parse "###" s
      m = case RSS.entry_by_date_s d e of
            Just e' -> RSS.n_entry_md "###" md e'
            Nothing -> "No news today"
      f = L.lt_markdown_to_html
      h = L.lt_std_html cf ["?n="++d] (H.cdata_raw (f m))
  CGI.utf8_html_output (H.renderHTML5 h)
-}

-- > splitDirectories "photos/fence" == ["photos","fence"]
dispatch :: L.Config -> CGI.Parameters -> IO ()
dispatch cf (m,q) =
{-
  let e = e_config
  v = require_verified e q
  in
 -}
      case (m,q) of
         ("GET",[("p",d)]) -> d_page cf (splitDirectories d)
         ("GET",[("o","resize")]) -> resize_get cf >> v_page cf []
         ("GET",[("v",d)]) -> v_page cf d
         ("GET",[]) -> d_page cf []
         _ -> CGI.utf8_text_output "ltr: unknown_request"
{-
         ("GET",[("n",d)]) -> news_d cf d
         ("GET",[("n","rss")]) -> rss_news cf
         ("GET",_,[("e","photos")]) -> v (E.edit_get "data/config/photos.hs")
         ("POST",_,[("e","photos")]) -> v (photos_post e cf)
         ("GET",_,[("e",d)]) -> v (E.edit_get (L.lt_markdown_file_name_f d))
         ("POST",_,[("e",d)]) -> v (E.edit_post e ("?p=" ++ d))
         ("GET",_,[("o","login")]) -> E.login_get q
         ("POST",_,("o","login"):q') -> E.login_post e q'
         ("GET",_,[("o","logout")]) -> E.logout_get e
         ("GET",_,[("o","upload")]) -> v (E.upload_get "data/image/photos" "*/*")
         ("POST",_,[("o","upload")]) -> v (E.upload_post e)
-}

main :: IO ()
main = CGI.cgi_main (dispatch (L.Config ("?p=" ++) "." True))
