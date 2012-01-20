module LTR where

import Data.Char
import Data.List
import Data.Function
import System.Directory {- directory -}
import qualified System.Directory.Tree as T {- directory-tree -}
import System.FilePath {- filepath -}
import qualified System.IO.UTF8 as U {- utf8-string -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified Text.HTML.Light.Composite as H
import qualified Text.Pandoc as P {- pandoc -}
import Text.Printf
import qualified Text.XML.Light as X {- xml -}

import qualified Img as I

-- * Genera

-- > title_case "title case" == "Title case"
title_case :: String -> String
title_case s =
    case s of
      [] -> []
      x:xs -> toUpper x : map toLower xs

-- * Configuration

data Config = Config {lt_base :: I.Renamer -- ^ menu
                     ,lt_root :: FilePath -- ^ css & data & rgen
                     ,lt_use_preload :: Bool}

lt_css :: Config -> FilePath
lt_css cf = lt_root cf </> "css/lt.css"

lt_site :: String
lt_site = "http://www.luciethorne.com/"

lt_edit_ln :: Config -> [Char] -> FilePath
lt_edit_ln cf p = lt_root cf </> "?e=" ++ p

lt_data_dirs :: [String]
lt_data_dirs = ["css","data","rgen"]

lt_menu_items :: [String]
lt_menu_items =
    [{-"home",-}
     "news","shows","albums","shop","press","photos","contact"]

lt_menu :: (String -> String) -> [(String,String)]
lt_menu cf =
    let f x = (cf x,x)
    in map f lt_menu_items

lt_class_tag :: [String] -> String
lt_class_tag p =
    case p of
      nm:_ -> nm
      [] -> "home"

-- * HTML

-- dsc = description
std_meta :: Config -> String -> [X.Content]
std_meta cf dsc =
    [H.title [] [H.cdata ("lucie thorne: " ++ dsc)]
    ,H.meta_description dsc
    ,H.meta_author "lucie thorne"
    ,H.meta_content_type "text/html; charset=UTF-8"
    ,H.link_css "all" (lt_css cf)
    ,H.meta_viewport "width=device-width,initial-scale=1,user-scalable=yes"]

std_html :: [X.Content] -> X.Element
std_html = H.html [H.lang "en"]

-- p = path to page
std_copyright :: Config -> FilePath -> X.Content
std_copyright cf p =
    H.footer
         [H.class' "footer"]
         [H.copy
         ,H.a [H.href lt_site] [H.cdata "lucie thorne"]
         ,H.cdata " 2012. " {- 1998- -}
         ,H.a [H.href H.w3_html_validator] [H.cdata "html"]
         ,H.cdata ", "
         ,H.a [H.href H.w3_css_validator] [H.cdata "css"]
         ,H.a [H.href (lt_edit_ln cf p)] [H.cdata "."]]

std_menu :: Config -> String -> X.Content
std_menu cf nm =
    let cl = H.class' "menu"
        f (m,p_) =
            let a_cl = H.class' (if m == nm then "here" else "not-here")
                ln = H.href (lt_base cf p_)
            in H.li [cl] [H.a [a_cl,ln] [H.cdata m]]
    in H.nav [cl] [H.ul [cl] (map f (lt_menu title_case))]

lt_h1 :: X.Content
lt_h1 =
    let t = H.h1 [H.title' "lucie thorne"] [H.cdata "Lucie Thorne"]
    in H.a [H.class' "h1",H.href lt_site] [t]

-- > joinPath ["a","b"] == "a/b"
lt_std_html :: Config -> [String] -> X.Content -> X.Element
lt_std_html cf p t =
    let n_ = lt_class_tag p
        a_ = joinPath p
        m_ = std_meta cf a_
        x_ = H.div [H.class' "content"] [lt_h1,t]
        b_ = [std_menu cf n_,x_,std_copyright cf a_]
    in std_html [H.head [] m_
                ,H.body [H.class' n_] [H.div [H.class' "main"] b_]]

-- * Markdown

revise_ln :: (P.Target -> P.Target) -> P.Pandoc -> P.Pandoc
revise_ln f =
    let g x = case x of
                P.Link m t -> P.Link m (f t)
                P.Image m t -> P.Image m (f t)
                _ -> x
    in P.processWith g {- pandoc-1.8.1.1 == P.bottomUp -}

read_file_or :: String -> FilePath -> IO String
read_file_or s f = do
  x <- doesFileExist f
  if x then U.readFile f else return s

lt_no_file :: String
lt_no_file =
    unlines ["unfortunately the file you've asked for doesn't exist."
            ,"we might have moved it a little?"
            ,"please try finding it using the menu."]

lt_markdown_to_html :: (P.Target -> P.Target) -> FilePath -> IO String
lt_markdown_to_html tf fn = do
  s <- read_file_or lt_no_file fn
  let p = P.defaultParserState {P.stateSmart = True}
      d = P.readMarkdown p (s ++ "\n")
      d' = revise_ln tf d
  return (P.writeHtmlString P.defaultWriterOptions d')

-- | Special case for the 'home' file.
lt_markdown_file_name_f :: FilePath -> FilePath
lt_markdown_file_name_f p =
    case p of
      "" -> "data/md/home.md"
      _ -> "data/md" </> p <.> "md"

lt_markdown_file_name :: [FilePath] -> FilePath
lt_markdown_file_name = lt_markdown_file_name_f . joinPath

-- * Images

-- the list of photos (read from a data file)
lt_img_data :: Config -> IO [I.Img]
lt_img_data cf = do
  s <- readFile (lt_root cf </> "data/config/photos.hs")
  return (read s)

arrows :: Config -> I.Neighbours -> X.Content
arrows cf (l,_,r) =
    let f s (Just n) = H.a [H.href (lt_base cf ("photos" </> n))] [s]
        f s Nothing = s
        ln = H.href (lt_root cf </> "?o=photos")
        cl = H.class' "edit"
        dot = H.a [cl,ln] [H.cdata "."]
    in H.span [H.class' "arrows"] [f H.larr l,H.nbsp,H.nbsp,f H.rarr r,dot]

photos_page :: Config ->
               [I.Img] ->
               X.Content ->
               I.Area ->
               I.Neighbours ->
               X.Element
photos_page cf im sm c nb =
    let rt = lt_root cf
        (_,(n,t),_) = nb
        a_ = "photos" </> n
        m_ = std_meta cf a_
        n_ = rt </> "rgen/photos" </> I.img_resize_dir 450 </> n <.> "jpg"
        f_ = rt </> "data/image/photos" </> n <.> "jpg"
        c_ = if null t then [] else [H.cdata ", ",H.cdata t]
        g_ = I.img_preview (rt,lt_base cf) im c
        i_ = H.p [] [H.a [H.href f_] [H.img [H.src n_,H.alt n]]
                    ,H.br []
                    ,H.span [] [H.a [H.href f_] [H.cdata "high resolution file"]]
                    ,H.span [] c_
                    ,arrows cf nb]
        x_ = H.div [H.class' "content"] [g_,i_,I.img_preload 450 (rt,lt_base cf) im c]
        b_ = [std_menu cf "photos",lt_h1,sm,x_]
    in std_html [H.head [] m_
                ,H.body [H.class' "photos"] [H.div [H.class' "main"] b_]]

-- generate single camera page
lt_photo_page :: Config -> (I.Area, I.Id) -> [I.Img] -> String
lt_photo_page cf (c,n) im =
  let sm = I.img_submenu (lt_base cf) im
      h = photos_page cf im sm c (I.img_neighbours im (c,n))
  in H.renderHTML5 h

lt_photo_page_io :: Config -> (I.Area,I.Id) -> IO String
lt_photo_page_io cf i = lt_img_data cf >>= return . lt_photo_page cf i

lt_photo_page_io_id :: Config -> I.Id -> IO String
lt_photo_page_io_id cf i = do
  im <- lt_img_data cf
  let x:_ = I.img_find i im
  return (lt_photo_page cf x im)

lt_img_reductions :: Config -> IO ()
lt_img_reductions cf = do
  im <- lt_img_data cf
  let nms = concatMap (map fst . snd) im
  mapM_ (I.img_mk_reduction (lt_root cf)) nms

-- * Redirects

-- lt_md helper
mk_md :: [T.FileName] -> T.DirTree t -> [([T.FileName],T.FileName)]
mk_md st t =
    case t of
      T.Dir nm xs -> concatMap (mk_md (st ++ [nm])) xs
      T.File nm _ -> [(st,nm)]
      T.Failed _ _ -> []

-- | The list of markdown files (read from file system).
--
-- > md <- lt_md "/home/rohan/ut/www-ltr/data/md"
lt_md :: FilePath -> IO [([String],[String])]
lt_md dir = do
  (_ T.:/ t) <- T.readDirectoryWith return dir
  let simplify xs = let a = tail (fst (head xs))
                        xs' = map (dropExtension . snd) xs
                    in (a,xs')
      d = mk_md [] t
      d' = groupBy ((==) `on` fst) (sort d)
      d'' = map simplify d'
  return d''

-- | Flatten results of 'lt_md'.
--
-- > lt_md "/home/rohan/ut/www-ltr/data/md" >>= return.md_flatten
md_flatten :: [([String],[String])] -> [[String]]
md_flatten =
    let f (a,e) = map (\x -> a ++ [x]) e
    in concatMap f

-- | Generate full set of apache redirects.
--
-- > lt_redirects "/home/rohan/ut/www-ltr/data/md" >>= mapM_ putStrLn
lt_redirects :: FilePath -> IO [String]
lt_redirects d = do
  md <- lt_md d
  let mdf = map joinPath (md_flatten md)
      f p = printf "Redirect permanent /%s /?p=%s" p p
  return (map f mdf)
