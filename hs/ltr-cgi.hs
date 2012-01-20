import Data.List
import qualified Network.CGI as C {- cgi -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import System.FilePath {- filepath -}
import qualified WWW.Minus.CGI as W {- hwww-minus -}
import qualified WWW.Minus.Edit as E

import qualified Img as I
import qualified LTR as L
import qualified Pwd as P

add_prefix :: L.Config -> (String,String) -> (String,String)
add_prefix c (d,t) =
    if "http://" `isPrefixOf` d
    then (d,t)
    else if head (splitDirectories d) `elem` L.lt_data_dirs
         then (L.lt_root c </> d,t)
         else (L.lt_base c d,t)

md_page :: L.Config -> [FilePath] -> W.Result
md_page cf p = do
  let p' = L.lt_markdown_file_name p
  h <- C.liftIO (L.lt_markdown_to_html (add_prefix cf) p')
  cs <- C.liftIO (L.lt_img_data cf)
  let h' = L.lt_std_html cf cs p (H.cdata_raw h)
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

d_page :: L.Config -> [[Char]] -> W.Result
d_page cf d =
    case d of
      "photos":i -> ph_page cf i
      _ -> md_page cf d

e_config :: E.Config
e_config =
    E.Config {E.cfg_vcs = Just E.Git
             ,E.cfg_author = "lucie thorne"
             ,E.cfg_url = "http://luciethorne.com"
             ,E.cfg_pwd = P.lt_pwd}

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

-- > splitDirectories "photos/fence" == ["photos","fence"]
dispatch :: L.Config -> W.Parameters -> W.Result
dispatch cf (m,p,q) =
    let e = e_config
        v = require_verified e
    in case (m,p,q) of
         ("GET",_,[("p",d)]) -> d_page cf (splitDirectories d)
         ("GET",_,[("e",d)]) -> v (E.edit_get (L.lt_markdown_file_name_f d))
         ("POST",_,[("e",d)]) -> v (E.edit_post e ("?p=" ++ d))
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
