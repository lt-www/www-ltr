module Main (main,rd_rebuild) where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import qualified Network.CGI as C
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath
import qualified System.IO.UTF8 as U {- utf8-string -}
import System.Time
import qualified Text.XHtml as H
import qualified Text.HTML.Light as M

import LT
import Pwd

-- If the initial entry is root, discard it.
dropRoot :: [FilePath] -> [FilePath]
dropRoot p =
    case p of
      ("/" : p') -> p'
      _ -> p

outputHtml :: (String -> H.Html) -> C.CGI C.CGIResult
outputHtml f = do
  t <- C.liftIO getClockTime
  C.output (H.renderHtml (f (show t)))

readFileOr :: String -> FilePath -> IO String
readFileOr s f = do
  x <- doesFileExist f
  if x then U.readFile f else return s

-- This needs to inform darcs of new files...
edit_get :: [FilePath] -> C.CGI C.CGIResult
edit_get p = do
  let mfn = lt_markdown_file_name p
  contents <- C.liftIO (readFileOr "" mfn)
  outputHtml (text_editor mfn contents)

-- Delete carriage returns, the browser may insert these...
deleteCR :: String -> String
deleteCR = filter (/= '\r')

-- Static naming for the moment...
darcsBinary :: String
darcsBinary = "/home/lucie/bin/darcs"

-- Run darcs with given arguments.
run_darcs :: [String] -> IO ExitCode
run_darcs p = do
  let d = darcsBinary
  system (d ++ " " ++ unwords p ++ " >/dev/null")

-- This must not write to stdout...
darcs_rec :: String -> IO ExitCode
darcs_rec c = do
  t <- getClockTime
  let n = "'Online edit at " ++ show t ++ " [" ++ c ++ "]'"
  run_darcs ["record", "--all", "-m", n]

darcs_add :: FilePath -> IO ExitCode
darcs_add f = run_darcs ["add", f]

add_prefix :: String -> (String,String) -> (String,String)
add_prefix p (d,t) =
    if "http://" `isPrefixOf` d then (d,t) else (p</>d,t)

make_file :: Config -> ([String], String) -> IO ()
make_file cf (p_, nm) = do
  cs <- lt_img_data cf
  let dir = lt_path_to p_ nm
      dir' = splitDirectories dir
      mfn = lt_dir cf </> lt_markdown_file_name dir'
      hfn = lt_dir cf </> lt_html_file_name dir'
      hfp = lt_dir cf </> dir
  mf <- lt_markdown_to_html (add_prefix (lt_root cf)) mfn
  let h = lt_std_html cf cs p_ nm (M.cdata_raw mf)
      h' = M.renderHTML5 h
  createDirectoryIfMissing True hfp
  U.writeFile hfn h'
  when (nm == "home") (U.writeFile (lt_dir cf </> "index.html") h')

-- Generate path to file if not existent...
edit_post :: Config -> [FilePath] -> C.CGI C.CGIResult
edit_post cf p = do
  file <- C.getInput "file"
  text <- C.getInput "text"
  let file' = fromMaybe "no_file" file
      text' = maybe "no_text" deleteCR text
      link = "/" </> lt_html_file_name p
  _ <- C.liftIO (do createDirectoryIfMissing True (takeDirectory file')
                    U.writeFile file' text'
                    make_file cf (lt_clear_path p)
                    darcs_rec "edit")
  outputHtml (message_link "edit_post" "edit stored" link)

rebuild_get :: Config -> C.CGI C.CGIResult
rebuild_get cf = do
  md <- C.liftIO (lt_md (lt_dir cf </> "data/md"))
  let l = concatMap (\(p, ns) -> map (\n -> (p, n)) ns) md
  C.liftIO (do mapM_ (make_file cf) l
               _ <- lt_img_reductions cf
               lt_write_photos_pages cf)
  outputHtml (message_link "rebuild_get" "rebuild completed" "../")

login_get :: C.CGI C.CGIResult
login_get = outputHtml (pwd_entry "login_get")

logout_get :: C.CGI C.CGIResult
logout_get = do
  C.deleteCookie (C.newCookie login_cookie "")
  outputHtml (message_link "logout_get" "logged out" "../")

login_post :: C.CGI C.CGIResult
login_post = do
  pwd <- C.getInput "pwd"
  let valid = maybe False (== lt_pwd) pwd
      c_msg = message_link "login_post" "login succeeded" "../"
      w_msg = message_link "login_post" "login failed" "../"
      correct = do C.setCookie (C.newCookie login_cookie "")
                   outputHtml c_msg
      wrong = outputHtml w_msg
  if valid then correct else wrong

upload_get :: C.CGI C.CGIResult
upload_get = outputHtml (upload_entry "upload_get")

upload_post :: C.CGI C.CGIResult
upload_post = do
  dir <- C.getInput "dir"
  name <- C.getInputFilename "file"
  bytes <- C.getInputFPS "file"
  let dir' = fromMaybe "no_dir" dir
      name' = fromMaybe "no_name" name
      bytes' = fromMaybe B.empty bytes
      pw = joinPath [dir', takeFileName name']
      pl = joinPath ["../", pw]
  _ <- C.liftIO (do B.writeFile pw bytes'
                    _ <- darcs_add pw
                    darcs_rec "upload")
  outputHtml (message_link "upload_post" "upload successful" pl)

db_changes_get :: C.CGI C.CGIResult
db_changes_get = undefined

photos_get :: C.CGI C.CGIResult
photos_get = do
  let fn = "data/config/photos.hs"
  contents <- C.liftIO (readFileOr "" fn)
  outputHtml (text_editor fn contents)

photos_post :: Config -> C.CGI C.CGIResult
photos_post cf = do
  text <- C.getInput "text"
  let file = "data/config/photos.hs"
      text' = maybe "no_text" deleteCR text
  C.liftIO (do U.writeFile file text'
               _ <- darcs_rec "photos"
               _ <- lt_img_reductions cf
               lt_write_photos_pages cf)
  outputHtml (message_link "photos_post" "edit stored" "/")

unknown_request :: String -> [FilePath] -> C.CGI C.CGIResult
unknown_request m p =
    let m' = " method = " ++ m
        p' = " path = " ++ show p
        i = " illegal operation " ++ m' ++ p'
    in outputHtml (message_only "unknown_request" i)

require_verified :: C.CGI C.CGIResult -> C.CGI C.CGIResult
require_verified y = do
  v <- validated
  let l = "/administration.cgi/login"
      n = message_link "require_verified" "un-verified" l
  if v then y else outputHtml n

request_dispatch :: Config -> String -> [String] -> C.CGI C.CGIResult
request_dispatch cf m a =
    case (m,a) of
      ("GET",("edit":p)) -> require_verified (edit_get p)
      ("POST",("edit":p)) -> require_verified (edit_post cf p)
      ("GET",["login"]) -> login_get
      ("POST",["login"]) -> login_post
      ("GET",["logout"]) -> logout_get
      ("GET",["upload"]) -> require_verified upload_get
      ("POST",["upload"]) -> require_verified upload_post
      ("GET",["rebuild"]) -> require_verified (rebuild_get cf)
      ("GET",["db", "changes"]) -> require_verified db_changes_get
      ("GET",["photos"]) -> require_verified photos_get
      ("POST",["photos"]) -> require_verified (photos_post cf)
      _ -> unknown_request m a

cgiMain :: Config -> C.CGI C.CGIResult
cgiMain cf = do
  method <- C.requestMethod
  path <- return . dropRoot . splitDirectories =<< C.pathInfo
  request_dispatch cf method path

lt_static_config :: Config
lt_static_config = Config "/" "." True

main :: IO ()
main = C.runCGI (C.handleErrors (cgiMain lt_static_config))

cf_rebuild :: Config -> IO ()
cf_rebuild = C.runCGI . C.handleErrors . rebuild_get

rd_rebuild :: IO ()
rd_rebuild = cf_rebuild (Config "/ltr" ".." True)

-- ID string for the login cookie.
login_cookie :: String
login_cookie = "lt_secret_login_cookie"

validated :: C.CGI Bool
validated = do
  c <- C.getCookie login_cookie
  return (isJust c)

-- A simple header...
header_p :: String -> String -> H.Html
header_p title time = H.p H.<< (title ++ ": " ++ time)

std_header :: String -> H.Html
std_header title =
    let c = [ H.thetitle H.<< title
            , H.meta H.! [H.httpequiv "expires", H.content "-1"]
            , H.meta H.! [H.httpequiv "pragma", H.content "no-cache"] ]
    in H.header H.<< c

text_editor :: String -> String -> String -> H.Html
text_editor title initial time =
    let h = std_header title
        b = H.body H.<< [header_p title time, f]
        a = [H.method "post", H.enctype "multipart/form-data"]
        f = H.form H.! a H.<< c
        t = [H.name "text", H.rows "30", H.cols "80"]
        n = H.hidden "file" title
        s = H.submit "" "Enter"
        c = [H.textarea (H.stringToHtml initial) H.! t, n, s]
    in h H.+++ b

pwd_entry :: String -> String -> H.Html
pwd_entry title time =
    let h = std_header title
        b = H.body H.<< [header_p title time, f]
        a = [H.method "post", H.enctype "multipart/form-data"]
        f = H.form H.! a H.<< c
        c = [H.password "pwd", H.submit "" "Enter"]
    in h H.+++ b

message_only :: String -> String -> String -> H.Html
message_only title message time =
    let h = std_header title
        b = H.body H.<< [header_p title time, H.p H.<< message]
    in h H.+++ b

message_link :: String -> String -> String -> String -> H.Html
message_link title message link time =
    let h = std_header title
        l = H.p H.<< H.hotlink link (H.stringToHtml link)
        b = H.body H.<< [header_p title time, H.p H.<< message, l]
    in h H.+++ b

upload_entry :: String -> String -> H.Html
upload_entry title time =
    let h = std_header title
        b = H.body H.<< [header_p title time, f]
        a = [H.method "post", H.enctype "multipart/form-data"]
        f = H.form H.! a H.<< c
        c = [H.thespan H.<< "file: ", H.afile "file", H.br
            ,H.thespan H.<< "directory: ", H.textfield "dir", H.br
            ,H.submit "" "Enter"]
    in h H.+++ b
