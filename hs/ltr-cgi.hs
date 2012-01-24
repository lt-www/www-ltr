import Data.List
import qualified Network.CGI as C {- cgi -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import System.FilePath {- filepath -}
import qualified WWW.Minus.CGI as W {- hwww-minus -}
import qualified WWW.Minus.CGI.Editor as E

import qualified Img as I
import qualified LTR as L
import qualified News as N
import qualified Pwd as P

add_prefix :: L.Config -> (String,String) -> (String,String)
add_prefix c (d,t) =
    if "http://" `isPrefixOf` d
    then (d,t)
    else if head (splitDirectories d) `elem` L.lt_data_dirs
         then (L.lt_root c </> d,t)
         else (L.lt_base c d,t)

-- non-relative (for rss etc.)
non_rel :: (String,String) -> (String,String)
non_rel (i,j) = (L.lt_site </> i,L.lt_site </> j)

md_page :: L.Config -> [FilePath] -> W.Result
md_page cf p = do
  let p' = L.lt_markdown_file_name p
  h <- C.liftIO (L.lt_markdown_to_html_io (add_prefix cf) p')
  let h' = L.lt_std_html cf p (H.cdata_raw h)
  W.utf8_html_output (H.renderHTML5 h')

ph_page_def :: L.Config -> W.Result
ph_page_def cf = do
  cs <- C.liftIO (L.lt_img_data cf)
  let i = I.img_initial cs
  h <- C.liftIO (L.lt_photo_page_io cf i)
  W.utf8_html_output h

ph_page_i :: L.Config -> I.Id -> W.Result
ph_page_i cf i = do
  h <- C.liftIO (L.lt_photo_page_io_id cf i)
  W.utf8_html_output h

ph_page :: L.Config -> [I.Id] -> W.Result
ph_page cf x =
    case x of
      i:_ -> ph_page_i cf i
      _ -> ph_page_def cf

d_page :: L.Config -> [String] -> W.Result
d_page cf d =
    case d of
      ["news","rss.xml"] -> rss_news cf
      "photos":i -> ph_page cf i
      _ -> md_page cf d

e_config :: E.Config
e_config =
    let v = Just (E.Git,("lucie thorne <lucie@luciethorne.com>",Just "sp"))
    in E.Config {E.cfg_vcs = v
                ,E.cfg_url = "http://luciethorne.com"
                ,E.cfg_pwd = Just P.lt_pwd}

photos_post :: E.Config -> L.Config -> W.Result
photos_post e cf = do
  r <- E.edit_post e "/"
  C.liftIO (L.lt_img_reductions cf)
  return r

require_verified :: E.Config -> W.Result -> W.Result
require_verified e y = do
  v <- E.validated e
  let l = "?o=login"
      n = E.message_link "require_verified" "un-verified" l
  if v then y else E.output_html n

rss_news :: L.Config -> W.Result
rss_news cf = do
  s <- C.liftIO (L.read_file_or "" "data/md/news.md")
  let n = N.parse s
      f = L.lt_markdown_to_html (non_rel . add_prefix cf)
      x = N.rss_s f n
  W.utf8_ct_output "application/rss+xml" x

news_d :: L.Config -> String -> W.Result
news_d cf d = do
  s <- C.liftIO (L.read_file_or "" "data/md/news.md")
  let (e,md) = N.parse s
      m = case N.entry_by_date_s d e of
            Just e' -> N.n_entry_md md e'
            Nothing -> "No news today"
      f = L.lt_markdown_to_html (add_prefix cf)
      h = L.lt_std_html cf ["?n="++d] (H.cdata_raw (f m))
  W.utf8_html_output (H.renderHTML5 h)

-- > splitDirectories "photos/fence" == ["photos","fence"]
dispatch :: L.Config -> W.Parameters -> W.Result
dispatch cf (m,p,q) =
    let e = e_config
        v = require_verified e
    in case (m,p,q) of
         ("GET",_,[("p",d)]) -> d_page cf (splitDirectories d)
         ("GET",_,[("e",d)]) -> v (E.edit_get (L.lt_markdown_file_name_f d))
         ("POST",_,[("e",d)]) -> v (E.edit_post e ("?p=" ++ d))
         ("GET",_,[("n","rss")]) -> rss_news cf
         ("GET",_,[("n",d)]) -> news_d cf d
         ("GET",_,[("o","login")]) -> E.login_get
         ("POST",_,[("o","login")]) -> E.login_post e
         ("GET",_,[("o","logout")]) -> E.logout_get e
         ("GET",_,[("o","upload")]) -> v E.upload_get
         ("POST",_,[("o","upload")]) -> v (E.upload_post e)
         ("GET",_,[("o","photos")]) -> v (E.edit_get "data/config/photos.hs")
         ("POST",_,[("o","photos")]) -> v (photos_post e cf)
         ("GET",_,_) -> d_page cf p
         _ -> E.unknown_request (m,p,q)

main :: IO ()
main = W.run_cgi (L.Config ("?p=" ++) "." True) dispatch
