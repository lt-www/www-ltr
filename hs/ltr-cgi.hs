{-
lighttpd: url.redirect = ( "^/lt/([-_/0-9a-z]+)$" => "/lt/lt.cgi/$1" )
-}

import Data.List
import qualified Network.CGI as C
import qualified Text.HTML.Light as H
import System.FilePath
import WWW.Minus.CGI

import qualified Img as I
import qualified LTR as L

-- framework only: to implement all local names ought to be absolute,
-- the html variant would prepend the ascending path to all local
-- links, the cgi variant would be clean
add_prefix :: L.Config -> (String,String) -> (String,String)
add_prefix c (d,t) =
    if "http://" `isPrefixOf` d
    then (d,t)
    else if head (splitDirectories d) `elem` ["css","data","rgen"]
         then (L.lt_root c </> d,t)
         else (L.lt_base c d,t)

md_page :: L.Config -> [FilePath] -> Result
md_page cf p = do
  let p' = L.lt_markdown_file_name p
  h <- C.liftIO (L.lt_markdown_to_html (add_prefix cf) p')
  cs <- C.liftIO (L.lt_img_data cf)
  let h' = L.lt_std_html cf cs p (H.cdata_raw h)
  utf8_html_output (H.renderHTML5 h')

ph_page_def :: L.Config -> Result
ph_page_def cf = do
  cs <- C.liftIO (L.lt_img_data cf)
  let i = I.img_initial cs
  h <- C.liftIO (L.lt_photo_page_io cf i)
  utf8_html_output h

ph_page_i :: L.Config -> I.Id -> Result
ph_page_i cf i = do
  h <- C.liftIO (L.lt_photo_page_io_id cf i)
  utf8_html_output h

ph_page :: L.Config -> [I.Id] -> Result
ph_page cf x =
    case x of
      i:_ -> ph_page_i cf i
      _ -> ph_page_def cf

d_page :: L.Config -> [[Char]] -> Result
d_page cf d =
    case d of
      "photos":i -> ph_page cf i
      _ -> md_page cf d

-- > splitDirectories "photos/fence"
dispatch :: L.Config -> Parameters -> Result
dispatch cf (m,p,q) =
    case (m,p,q) of
      ("GET",_,[("p",d)]) -> d_page cf (splitDirectories d)
      ("GET",_,_) -> d_page cf p
      _ -> undefined

main :: IO ()
main = run_cgi (L.Config ("/ltr/?p=" ++) "/ltr" "." True) dispatch
