module Main (main,rd_rebuild) where

import Control.Monad
import Data.List
import qualified Network.CGI as C {- cgi -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}
import qualified System.IO.UTF8 as U {- utf8-string -}
import qualified Text.HTML.Light as H {- html-minimalist -}

import qualified Edit as E
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

make_file :: L.Config -> ([String], String) -> IO ()
make_file cf (p_, nm) = do
  cs <- L.lt_img_data cf
  let dir = L.lt_path_to p_ nm
      dir' = splitDirectories dir
      mfn = L.lt_dir cf </> L.lt_markdown_file_name dir'
      hfn = L.lt_dir cf </> L.lt_html_file_name dir'
      hfp = L.lt_dir cf </> dir
  mf <- L.lt_markdown_to_html (add_prefix (L.lt_root cf)) mfn
  let h = L.lt_std_html cf cs p_ nm (H.cdata_raw mf)
      h' = H.renderHTML5 h
  createDirectoryIfMissing True hfp
  U.writeFile hfn h'
  when (nm == "home") (U.writeFile (L.lt_dir cf </> "index.html") h')

edit_post_mk :: L.Config -> [FilePath] -> E.Config -> E.Result
edit_post_mk cf p e = do
  let ln = "/" </> L.lt_html_file_name p
  r <- E.edit_post e ln
  C.liftIO (make_file cf (L.lt_clear_path p))
  return r

rebuild_get :: L.Config -> E.Result
rebuild_get cf = do
  md <- C.liftIO (L.lt_md (L.lt_dir cf </> "data/md"))
  let l = concatMap (\(p, ns) -> map (\n -> (p, n)) ns) md
  C.liftIO (do mapM_ (make_file cf) l
               _ <- L.lt_img_reductions cf
               L.lt_write_photos_pages cf)
  E.std_reply e_config "rebuild_get" "rebuild completed"

photos_post :: L.Config -> E.Result
photos_post cf = do
  text <- C.getInput "text"
  let file = "data/config/photos.hs"
      text' = maybe "no_text" E.delete_cr text
  C.liftIO (do U.writeFile file text'
               _ <- E.vcs_commit_c e_config file
               _ <- L.lt_img_reductions cf
               L.lt_write_photos_pages cf)
  E.std_reply e_config "photos_post" "edit stored"

require_verified :: E.Config -> E.Result -> E.Result
require_verified e y = do
  v <- E.validated e
  let l = "/administration.cgi/login"
      n = E.message_link "require_verified" "un-verified" l
  if v then y else E.output_html n

request_dispatch :: L.Config -> String -> [String] -> C.CGI C.CGIResult
request_dispatch cf m a =
    let e = e_config
        v = require_verified e
    in case (m,a) of
         ("GET",("edit":p)) -> v (E.edit_get (L.lt_markdown_file_name p))
         ("POST",("edit":p)) -> v (edit_post_mk cf p e)
         ("GET",["login"]) -> E.login_get
         ("POST",["login"]) -> E.login_post e
         ("GET",["logout"]) -> E.logout_get e
         ("GET",["upload"]) -> v E.upload_get
         ("POST",["upload"]) -> v (E.upload_post e)
         ("GET",["rebuild"]) -> v (rebuild_get cf)
         ("GET",["photos"]) -> v (E.edit_get "data/config/photos.hs")
         ("POST",["photos"]) -> v (photos_post cf)
         _ -> E.unknown_request m a

-- | If the initial entry is root, discard it.
drop_root :: [FilePath] -> [FilePath]
drop_root p =
    case p of
      "/":p' -> p'
      _ -> p

cgi_main :: L.Config -> C.CGI C.CGIResult
cgi_main cf = do
  method <- C.requestMethod
  path <- return . drop_root . splitDirectories =<< C.pathInfo
  request_dispatch cf method path

lt_static_config :: L.Config
lt_static_config = L.Config "/" "." True

main :: IO ()
main = C.runCGI (C.handleErrors (cgi_main lt_static_config))

cf_rebuild :: L.Config -> IO ()
cf_rebuild = C.runCGI . C.handleErrors . rebuild_get

rd_rebuild :: IO ()
rd_rebuild = cf_rebuild (L.Config "/ltr" ".." True)
