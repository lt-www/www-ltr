{-
lighttpd: url.redirect = ( "^/lt/([-_/0-9a-z]+)$" => "/lt/lt.cgi/$1" )
-}

import Data.List
import qualified Network.CGI as C
import qualified Text.HTML.Light as H
import System.FilePath
import WWW.Minus.CGI

import qualified LTR as L

-- framework only: to implement all local names ought to be absolute,
-- the html variant would prepend the ascending path to all local
-- links, the cgi variant would be clean
add_prefix :: L.Config -> (String,String) -> (String,String)
add_prefix c (d,t) =
    if "http://" `isPrefixOf` d
    then (d,t)
    else if head (splitDirectories d) `elem` ["css","data","rgen"]
         then (L.lt_root c</>d,t)
         else (L.lt_base c </> d,t)

dispatch :: L.Config -> Parameters -> Result
dispatch cf (m,p,q) =
    case (m,p,q) of
      ("GET",["photos",i],_) ->
          do h <- C.liftIO (L.lt_photo_page_id cf i)
             utf8_html_output h
      ("GET",_,_) ->
          do let p' = L.lt_markdown_file_name p
             h <- C.liftIO (L.lt_markdown_to_html (add_prefix cf) p')
             cs <- C.liftIO (L.lt_img_data cf)
             let h' = L.lt_std_html cf cs p (H.cdata_raw h)
             utf8_html_output (H.renderHTML5 h')
      _ -> undefined

main :: IO ()
main = run_cgi (L.Config "/ltr/ltr.cgi" "/ltr" "." True) dispatch
