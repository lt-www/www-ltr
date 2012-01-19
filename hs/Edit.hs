module Edit where

import qualified Codec.Binary.UTF8.String as U {- utf8-string -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.Maybe
import Data.Time {- time -}
import qualified Network.CGI as C {- cgi -}
import System.Cmd {- process -}
import System.Directory {- directory -}
import System.Exit
import System.FilePath {- filepath -}
import qualified System.IO.Strict as I {- strict -}
import qualified System.IO.UTF8 as U {- utf8-string -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified Text.XML.Light as X {- xml -}

data VCS = Darcs | Git
type Author = String
type URL = String
type Password = String
data Config = Config {cfg_vcs :: Maybe VCS
                     ,cfg_author :: Author
                     ,cfg_url :: URL
                     ,cfg_pwd :: Password}

type Result = C.CGI C.CGIResult

-- | Output @HTML@ 'String' in @utf-8@ encoding.
utf8_output :: String -> Result
utf8_output x = do
  C.setHeader "Content-Type" "text/html; charset=utf-8"
  C.output (U.encodeString x)

output_html :: (String -> X.Element) -> Result
output_html f = do
  t <- C.liftIO getCurrentTime
  utf8_output (H.renderHTML5 (f (show t)))

preamble :: String -> String -> X.Content
preamble title time = H.p [] [H.cdata (title ++ ": " ++ time)]

std_header :: String -> X.Content
std_header title = H.head [] [H.title [] [H.cdata title]]

std_html :: String -> String -> X.Content -> X.Element
std_html title time c =
    let b = H.body [] [preamble title time,c]
    in H.html [H.lang "en"] [std_header title,b]

text_editor :: String -> String -> String -> X.Element
text_editor title initial time =
    let a = [H.method "post", H.enctype "multipart/form-data"]
        f = H.form a c
        t = [H.name "text", H.rows "30", H.cols "80"]
        c = [H.textarea t [H.cdata initial]
            ,H.input [H.type' "hidden",H.name "file",H.value title]
            ,H.input [H.type' "submit",H.value "edit"]]
    in std_html title time f

read_file_or :: String -> FilePath -> IO String
read_file_or s f = do
  x <- doesFileExist f
  if x then I.readFile f else return s

edit_get :: FilePath -> Result
edit_get fn = do
  t <- C.liftIO (read_file_or "" fn)
  output_html (text_editor fn t)

-- | Delete carriage returns, the browser may insert these.
delete_cr :: String -> String
delete_cr = filter (/= '\r')

-- * Replies

message_only :: String -> String -> String -> X.Element
message_only title message time =
    let c = H.p [] [H.cdata message]
    in std_html title time c

message_link :: String -> String -> String -> String -> X.Element
message_link title message link time =
    let l = H.p [] [H.a [H.href link] [H.cdata link]]
        c = H.p [] [H.cdata message, l]
    in std_html title time c

std_reply :: Config -> String -> String -> Result
std_reply c t m = output_html (message_link t m (cfg_url c))

-- * Git

run_git :: [String] -> IO ExitCode
run_git p = system ("git " ++ unwords p ++ " &>/tmp/run_git")

git_add :: FilePath -> IO ExitCode
git_add f = run_git ["add", f]

git_commit :: Author -> FilePath -> IO ExitCode
git_commit au fn = do
  t <- getCurrentTime
  let m = "'online edit of " ++ fn ++ " at " ++ show t ++ "'"
  run_git ["commit"
          ,fn
          ,"-m", m
          ,"--author='" ++ au ++ "'"]

-- * Darcs

run_darcs :: [String] -> IO ExitCode
run_darcs p = system ("darcs " ++ unwords p ++ " &>/dev/null")

darcs_add :: FilePath -> IO ExitCode
darcs_add f = run_darcs ["add", f]

darcs_commit :: Author -> FilePath -> IO ExitCode
darcs_commit _ fn = do
  t <- getCurrentTime
  let m = "'online edit of " ++ fn ++ " at " ++ show t ++ "'"
  run_darcs ["record", fn, "-a", "-m", m]

-- * VCS

sel_vcs :: t -> t -> t -> Maybe VCS -> t
sel_vcs d g n v =
    case v of
      Just Darcs -> d
      Just Git -> g
      Nothing -> n

run_vcs :: Maybe VCS -> [String] -> IO ExitCode
run_vcs = sel_vcs run_darcs run_git (\_ -> return ExitSuccess)

vcs_add :: Maybe VCS -> FilePath -> IO ExitCode
vcs_add = sel_vcs darcs_add git_add (\_ -> return ExitSuccess)

vcs_commit :: Maybe VCS -> Author -> FilePath -> IO ExitCode
vcs_commit = sel_vcs darcs_commit git_commit (\_ _ -> return ExitSuccess)

-- * VCS (via Config)

vcs_add_c :: Config -> FilePath -> IO ExitCode
vcs_add_c c = vcs_add (cfg_vcs c)

vcs_commit_c :: Config -> FilePath -> IO ExitCode
vcs_commit_c c = vcs_commit (cfg_vcs c) (cfg_author c)

-- *

edit_post :: Config -> URL -> Result
edit_post c ln = do
  f <- C.getInput "file"
  t <- C.getInput "text"
  let f' = fromMaybe "no_file" f
      t' = maybe "no_text" delete_cr t
      act = do createDirectoryIfMissing True (takeDirectory f')
               U.writeFile f' t'
               vcs_commit_c c f'
  x <- C.liftIO act
  output_html (message_link
               "edit_post"
               ("edit stored: " ++ show (f', x))
               ln)

edit_text :: String -> Config -> FilePath -> Result
edit_text q c f =
    case q of
      "GET" -> edit_get f
      "POST" -> edit_post c (cfg_url c)
      _ -> undefined

-- * Login/Pwd

-- | ID string for the login cookie.
login_cookie :: String
login_cookie = "secret_login_cookie"

validated :: C.CGI Bool
validated = do
  c <- C.getCookie login_cookie
  return (isJust c)

pwd_entry :: String -> String -> X.Element
pwd_entry title time =
    let a = [H.method "post", H.enctype "multipart/form-data"]
        f = H.form a c
        c = [H.input [H.type' "password",H.name "pwd"]
            ,H.input [H.type' "submit",H.name "enter"]]
    in std_html title time f

login_get :: Result
login_get = output_html (pwd_entry "login_get")

login_post :: Config -> Result
login_post c = do
  pwd <- C.getInput "pwd"
  let valid = maybe False (== (cfg_pwd c)) pwd
      c_msg = message_link "login_post" "login succeeded" (cfg_url c)
      w_msg = message_link "login_post" "login failed" (cfg_url c)
      correct = do C.setCookie (C.newCookie login_cookie "")
                   output_html c_msg
      wrong = output_html w_msg
  if valid then correct else wrong

logout_get :: Config -> Result
logout_get c = do
  C.deleteCookie (C.newCookie login_cookie "")
  output_html (message_link "logout_get" "logged out" (cfg_url c))

-- * Upload

upload_entry :: String -> String -> X.Element
upload_entry title time =
    let a = [H.method "post"
            ,H.enctype "multipart/form-data"]
        f = H.form a c
        c = [H.cdata "file: "
            ,H.input [H.type' "file",H.name "file"]
            ,H.br []
            ,H.cdata "directory: "
            ,H.input [H.type' "text",H.name "dir"]
            ,H.br []
            ,H.input [H.type' "submit"]]
    in std_html title time f

upload_get :: Result
upload_get = output_html (upload_entry "upload_get")

upload_post :: Config -> Result
upload_post c = do
  dir <- C.getInput "dir"
  name <- C.getInputFilename "file"
  bytes <- C.getInputFPS "file"
  let dir' = fromMaybe "no_dir" dir
      name' = fromMaybe "no_name" name
      bytes' = fromMaybe B.empty bytes
      pw = joinPath [dir', takeFileName name']
      pl = joinPath ["../", pw]
  _ <- C.liftIO (do B.writeFile pw bytes'
                    _ <- vcs_add_c c pw
                    vcs_commit_c c pw)
  output_html (message_link "upload_post" "upload successful" pl)

-- * Errors

unknown_request :: String -> [FilePath] -> Result
unknown_request m p =
    let m' = " method = " ++ m
        p' = " path = " ++ show p
        i = " illegal operation " ++ m' ++ p'
    in output_html (message_only "unknown_request" i)

