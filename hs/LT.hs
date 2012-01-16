module LT where

import Data.Char
import Data.List
import Data.Function
import System.Directory
import qualified System.Directory.Tree as T {- directory-tree -}
import System.FilePath
import qualified System.IO.UTF8 as U {- utf8-string -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified Text.HTML.Light.Common as H
import qualified Text.Pandoc as P {- pandoc -}
import qualified Text.XML.Light as X {- xml -}

import Img

data Config = Config {lt_root :: FilePath
                     ,lt_dir :: FilePath
                     ,lt_use_preload :: Bool}

lt_css :: Config -> FilePath
lt_css cf = lt_root cf </> "css/lt.css"

-- d = description
std_meta :: Config -> String -> [X.Content]
std_meta cf d =
    [H.title [] [H.cdata ("lucie thorne: " ++ d)]
    ,H.meta_description d
    ,H.meta_author "lucie thorne"
    ,H.meta_content_type "text/html; charset=UTF-8"
    ,H.link_css "all" (lt_css cf)
    ,H.meta_viewport "width=device-width,initial-scale=1,user-scalable=yes"]

std_html_attr :: [X.Attr]
std_html_attr = [H.lang "en"]

lt_site :: String
lt_site = "http://www.luciethorne.com/"

html_validator :: String
html_validator = "http://validator.w3.org/check/referer"

css_validator :: String
css_validator = "http://jigsaw.w3.org/css-validator/check/referer?profile=css3"

-- p = path to page
std_copyright :: Config -> FilePath -> X.Content
std_copyright cf p =
    let e = lt_root cf </> "administration.cgi" </> "edit" </> p
    in H.footer [H.class' "footer"]
           [H.copy
           ,H.a [H.href lt_site] [H.cdata "lucie thorne"]
           ,H.cdata " 2011. " {- 1998- -}
           ,H.a [H.href html_validator] [H.cdata "html"]
           ,H.cdata ", "
           ,H.a [H.href css_validator] [H.cdata "css"]
           ,H.a [H.href e] [H.cdata "."]]

lt_menu :: (String->String) -> [Img] -> [(String,String)]
lt_menu cf cs =
    let ph = let (_,y) = img_initial cs
             in "photos" </> y
        f x = case x of
                "photos" -> (cf "photos",ph)
                _ -> (cf x,x)
    in map f [{-"home",-}
              "news","shows","albums","shop","press","photos","contact"]

-- > title_case "title case" == "Title case"
title_case :: String -> String
title_case s =
    case s of
      [] -> []
      x:xs -> toUpper x : map toLower xs

-- cs = cameras
std_menu :: Config -> [Img] -> String -> X.Content
std_menu cf cs nm =
    let cl = H.class' "menu"
        f (m,p_) =
            let a_cl = H.class' (if m == nm then "here" else "not-here")
                ln = H.href (lt_root cf </> p_)
            in H.li [cl] [H.a [a_cl,ln] [H.cdata m]]
    in H.nav [cl] [H.ul [cl] (map f (lt_menu title_case cs))]

lt_path_to :: [String] -> String -> String
lt_path_to p nm =
    case p of
      [] -> nm
      _ -> joinPath p </> nm

classTag :: [String] -> String -> String
classTag p nm =
    case (p,nm) of
      ([],"") -> "home"
      ([],_) -> nm
      (p':_,_) -> p'

lt_h1 :: X.Content
lt_h1 =
    let t = H.h1 [H.title' "lucie thorne"] [H.cdata "Lucie Thorne"]
    in H.a [H.class' "h1",H.href lt_site] [t]

lt_std_html :: Config -> [Img] -> [String] -> String -> X.Content -> X.Element
lt_std_html cf cs ar nm t =
    let n_ = classTag ar nm
        a_ = lt_path_to ar nm
        cl = case ar of
               [] -> nm
               (x:_) -> x
        m_ = std_meta cf a_
        x_ = H.div [H.class' "content"] [lt_h1,t]
        b_ = [std_menu cf cs cl,x_,std_copyright cf a_]
    in H.html std_html_attr
           [H.head [] m_
           ,H.body [H.class' n_] [H.div [H.class' "main"] b_]]

revise_ln :: (P.Target -> P.Target) -> P.Pandoc -> P.Pandoc
revise_ln f =
    let g x = case x of
                P.Link m t -> P.Link m (f t)
                P.Image m t -> P.Image m (f t)
                _ -> x
    in P.processWith g {- pandoc-1.8.1.1 == P.bottomUp -}

lt_markdown_to_html :: (P.Target -> P.Target) -> FilePath -> IO String
lt_markdown_to_html tf fn = do
  s <- U.readFile fn
  let p = P.defaultParserState {P.stateSmart = True}
      d = P.readMarkdown p (s ++ "\n")
      d' = revise_ln tf d
  return (P.writeHtmlString P.defaultWriterOptions d')

-- | Special case for the 'home' file.
lt_markdown_file_name :: [FilePath] -> FilePath
lt_markdown_file_name p =
    case p of
      [] -> "data/md/home.md"
      _ -> "data/md" </> joinPath p <.> "md"

lt_html_file_name :: [FilePath] -> FilePath
lt_html_file_name p = joinPath p </> "index" <.> "html"

-- the list of photos (read from a data file)
lt_img_data :: Config -> IO [Img]
lt_img_data cf = do
  s <- readFile (lt_dir cf </> "data/config/photos.hs")
  return (read s)

-- lt_md helper
mk_md :: [T.FileName] -> T.DirTree t -> [([T.FileName],T.FileName)]
mk_md st t =
    case t of
      T.Dir nm xs -> concatMap (mk_md (st ++ [nm])) xs
      T.File nm _ -> [(st,nm)]
      T.Failed _ _ -> []

-- the list of markdown files (read from file system)
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

arrows :: Config -> Neighbours -> X.Content
arrows cf (l,_,r) =
    let f s (Just n) = H.a [H.href (lt_root cf </> "photos" </> n)] [s]
        f s Nothing = s
        ln = H.href (lt_root cf </> "administration.cgi" </> "photos")
        cl = H.class' "edit"
        dot = H.a [cl,ln] [H.cdata "."]
    in H.span [H.class' "arrows"] [f H.larr l,H.nbsp,H.nbsp,f H.rarr r,dot]

photos_page :: Config ->
               [Img] ->
               X.Content ->
               Area ->
               Neighbours ->
               X.Element
photos_page cf cs sm c nb =
    let rt = lt_root cf
        (_,(n,t),_) = nb
        a_ = "photos" </> n
        m_ = std_meta cf a_
        n_ = rt </> "rgen/photos" </> img_resize_dir 450 </> n <.> "jpg"
        f_ = rt </> "data/image/photos" </> n <.> "jpg"
        c_ = if null t then [] else [H.cdata ", ",H.cdata t]
        g_ = img_preview rt cs c
        i_ = H.p [] [H.a [H.href f_] [H.img [H.src n_,H.alt n]]
                    ,H.br []
                    ,H.span [] [H.a [H.href f_] [H.cdata "high resolution file"]]
                    ,H.span [] c_
                    ,arrows cf nb]
        x_ = H.div [H.class' "content"] [g_,i_,img_preload 450 rt cs c]
        b_ = [std_menu cf cs "photos",lt_h1,sm,x_]
    in H.html std_html_attr
           [H.head [] m_
           ,H.body [H.class' "photos"] [H.div [H.class' "main"] b_]]

write_photo_page :: Config ->
                    [Img] ->
                    X.Content ->
                    Area ->
                    Neighbours ->
                    IO ()
write_photo_page cf cs sm c (l,(n,t),r) = do
  let pp = lt_dir cf </> "photos" </> n
      pf = pp </> "index.html"
  createDirectoryIfMissing True pp
  writeFile pf (H.renderHTML5 (photos_page cf cs sm c (l,(n,t),r)))

-- | Write all photos pages to disk
lt_write_photos_pages :: Config -> IO ()
lt_write_photos_pages cf = do
  cs <- lt_img_data cf
  let sm = img_submenu (lt_root cf) cs
      f (c,n) = mapM (write_photo_page cf cs sm c) (sets n)
  mapM_ f cs

-- generate single camera page
lt_photo_page :: Config -> (String,String) -> IO String
lt_photo_page cf (c,n) = do
  cs <- lt_img_data cf
  let sm = img_submenu (lt_root cf) cs
      h = photos_page cf cs sm c (img_neighbours cs (c,n))
  return (H.renderHTML5 h)

lt_img_reductions :: Config -> IO ()
lt_img_reductions cf = do
  cs <- lt_img_data cf
  let nms = concatMap (map fst . snd) cs
  mapM_ (img_mk_reduction (lt_dir cf)) nms

-- This odd rule creates the structure at Data.lt_md
lt_clear_path :: [FilePath] -> ([FilePath],FilePath)
lt_clear_path p =
    case p of
      [] -> ([],"")
      _ -> (init p,last p)
