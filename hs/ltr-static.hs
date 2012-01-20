module Main (main,rd_rebuild) where

import Control.Monad
import Data.List
import qualified Network.CGI as C {- cgi -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}
import qualified System.IO.UTF8 as U {- utf8-string -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified WWW.Minus.CGI as W
import qualified WWW.Minus.Edit as E

import qualified LTR as L
import qualified Pwd as P

e_config :: E.Config
e_config =
    E.Config {E.cfg_vcs = Just E.Git
             ,E.cfg_author = "lucie thorne"
             ,E.cfg_url = "http://luciethorne.com"
             ,E.cfg_pwd = P.lt_pwd}

add_prefix :: String -> (String,String) -> (String,String)
add_prefix p (d,t) =
    if "http://" `isPrefixOf` d
    then (d,t)
    else (p</>d,t)

make_file :: L.Config -> [FilePath] -> IO ()
make_file cf p = do
  cs <- L.lt_img_data cf
  let dir = joinPath p
      mfn = L.lt_dir cf </> L.lt_markdown_file_name p
      hfn = L.lt_dir cf </> L.lt_html_file_name p
      hfp = L.lt_dir cf </> dir
  mf <- L.lt_markdown_to_html (add_prefix (L.lt_root cf)) mfn
  let h = L.lt_std_html cf cs p (H.cdata_raw mf)
      h' = H.renderHTML5 h
  createDirectoryIfMissing True hfp
  U.writeFile hfn h'
  when (L.lt_class_tag p == "home")
       (U.writeFile (L.lt_dir cf </> "index.html") h')

edit_post_mk :: L.Config -> [FilePath] -> E.Config -> W.Result
edit_post_mk cf p e = do
  let ln = "/" </> L.lt_html_file_name p
  r <- E.edit_post e ln
  C.liftIO (make_file cf p)
  return r

rebuild_get :: L.Config -> W.Result
rebuild_get cf = do
  md <- C.liftIO (L.lt_md (L.lt_dir cf </> "data/md"))
  C.liftIO (do mapM_ (make_file cf) (L.md_flatten md)
               _ <- L.lt_img_reductions cf
               L.lt_write_photos_pages cf)
  E.std_reply e_config "rebuild_get" "rebuild completed"

photos_post :: L.Config -> W.Result
photos_post cf = do
  text <- C.getInput "text"
  let file = "data/config/photos.hs"
      text' = maybe "no_text" E.delete_cr text
  C.liftIO (do U.writeFile file text'
               _ <- E.vcs_commit_c e_config file
               _ <- L.lt_img_reductions cf
               L.lt_write_photos_pages cf)
  E.std_reply e_config "photos_post" "edit stored"

require_verified :: E.Config -> W.Result -> W.Result
require_verified e y = do
  v <- E.validated e
  let l = "/administration.cgi/login"
      n = E.message_link "require_verified" "un-verified" l
  if v then y else E.output_html n

request_dispatch :: L.Config -> W.Dispatch
request_dispatch cf (m,p,q) =
    let e = e_config
        v = require_verified e
    in case (m,p) of
         ("GET",("edit":p')) -> v (E.edit_get (L.lt_markdown_file_name p'))
         ("POST",("edit":p')) -> v (edit_post_mk cf p' e)
         ("GET",["login"]) -> E.login_get
         ("POST",["login"]) -> E.login_post e
         ("GET",["logout"]) -> E.logout_get e
         ("GET",["upload"]) -> v E.upload_get
         ("POST",["upload"]) -> v (E.upload_post e)
         ("GET",["rebuild"]) -> v (rebuild_get cf)
         ("GET",["photos"]) -> v (E.edit_get "data/config/photos.hs")
         ("POST",["photos"]) -> v (photos_post cf)
         _ -> E.unknown_request (m,p,q)

lt_static_config :: L.Config
lt_static_config = L.Config ("/" </>) "/" "." True

main :: IO ()
main = W.run_cgi lt_static_config request_dispatch

cf_rebuild :: L.Config -> IO ()
cf_rebuild = C.runCGI . C.handleErrors . rebuild_get

rd_rebuild :: IO ()
rd_rebuild = cf_rebuild (L.Config ("/ltr" </>) "/ltr" ".." True)
