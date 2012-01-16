module LT (Config(..)
          ,lt_std_html
          ,lt_markdown_to_html
          ,lt_markdown_file_name
          ,lt_html_file_name
          ,Camera
          ,lt_camera_data
          ,lt_camera_page,lt_write_camera_pages
          ,lt_camera_reductions
          ,lt_md
          ,lt_clear_path
          ,lt_path_to) where

import Control.Monad
import Data.Char
import Data.List
import Data.Function
import System.Directory
import qualified System.Directory.Tree as T {- directory-tree -}
import System.FilePath
import System.Process
import qualified System.IO.UTF8 as U {- utf8-string -}
import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified Text.HTML.Light.Common as H
import qualified Text.Pandoc as P {- pandoc -}
import qualified Text.XML.Light as X {- xml -}

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

type Camera = (String,[(String,String)])
type Neighbours = (Maybe String,(String,String),Maybe String)

-- first photo,ie. (area,id)
lt_photo_ln :: [Camera] -> (String,String)
lt_photo_ln c =
    case c of
      ((x,((y,_):_)):_) -> (x,y)
      _ -> undefined

lt_menu :: (String->String) -> [Camera] -> [(String,String)]
lt_menu cf cs =
    let ph = let (_,y) = lt_photo_ln cs
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
std_menu :: Config -> [Camera] -> String -> X.Content
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

lt_std_html :: Config -> [Camera] -> [String] -> String -> X.Content -> X.Element
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
lt_camera_data :: Config -> IO [Camera]
lt_camera_data cf = do
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

camera_submenu :: Config -> [Camera] -> X.Content
camera_submenu cf d =
    let f (n,((i,_):_)) = (n,i)
        f (_,[]) = undefined
        adr i = lt_root cf </> "photos" </> i
        cl = H.class' "submenu"
        g (n,i) = H.li [cl] [H.a [cl,H.href (adr i)] [H.cdata n]]
    in H.nav [cl] [H.ul [cl] (map (g . f) d)]

arrows :: Config -> Neighbours -> X.Content
arrows cf (l,_,r) =
    let f s (Just n) = H.a [H.href (lt_root cf </> "photos" </> n)] [s]
        f s Nothing = s
        ln = H.href (lt_root cf </> "administration.cgi" </> "photos")
        cl = H.class' "edit"
        dot = H.a [cl,ln] [H.cdata "."]
    in H.span [H.class' "arrows"] [f H.larr l,H.nbsp,H.nbsp,f H.rarr r,dot]

camera_set :: (String,String) -> Config -> [Camera] -> String -> X.Content
camera_set (cl,rd) cf cs c =
    let (Just a) = lookup c cs
        fn n = lt_root cf </> "rgen/photos" </> rd </> n <.> "jpg"
        ln n = lt_root cf </> "photos" </> n
        cl' = H.class' cl
        f (n,_) = H.a [H.href (ln n)] [H.img [cl',H.src (fn n),H.alt n]]
    in H.div [cl'] (map f a)

camera_preview :: Config -> [Camera] -> String -> X.Content
camera_preview = camera_set ("photos-preview","r-60")

camera_sz :: String
camera_sz = "r-450"

camera_preload :: Config -> [Camera] -> String -> X.Content
camera_preload cf cs c =
    if lt_use_preload cf
    then camera_set ("photos-preload",camera_sz) cf cs c
    else H.div [H.class' "photos-preload"] []

camera_page :: Config ->
               [Camera] ->
               X.Content ->
               String ->
               Neighbours ->
               X.Element
camera_page cf cs sm c nb =
    let (_,(n,t),_) = nb
        a_ = "photos" </> n
        m_ = std_meta cf a_
        n_ = lt_root cf </> "rgen/photos" </> camera_sz </> n <.> "jpg"
        f_ = lt_root cf </> "data/image/photos" </> n <.> "jpg"
        c_ = if null t then [] else [H.cdata ", ",H.cdata t]
        g_ = camera_preview cf cs c
        i_ = H.p [] [H.a [H.href f_] [H.img [H.src n_,H.alt n]]
                    ,H.br []
                    ,H.span [] [H.a [H.href f_] [H.cdata "high resolution file"]]
                    ,H.span [] c_
                    ,arrows cf nb]
        x_ = H.div [H.class' "content"] [g_,i_,camera_preload cf cs c]
        b_ = [std_menu cf cs "photos",lt_h1,sm,x_]
    in H.html std_html_attr
           [H.head [] m_
           ,H.body [H.class' "photos"] [H.div [H.class' "main"] b_]]

sets :: [(a,b)] -> [(Maybe a,(a,b),Maybe a)]
sets n =
    let n' = map fst n
        n'' = map Just n'
    in zip3 (Nothing : n'') n (drop 1 n'' ++ [Nothing])

camera_neighbours :: [Camera] -> (String,String) -> Neighbours
camera_neighbours cs (a,x) =
    let Just s = find (\(a',_) -> a' == a) cs
        s' = sets (snd s)
        Just r = find (\(_,(x',_),_) -> x' == x) s'
    in r

write_camera_page :: Config ->
                     [Camera] ->
                     X.Content ->
                     String ->
                     Neighbours ->
                     IO ()
write_camera_page cf cs sm c (l,(n,t),r) = do
  let pp = lt_dir cf </> "photos" </> n
      pf = pp </> "index.html"
  createDirectoryIfMissing True pp
  writeFile pf (H.renderHTML5 (camera_page cf cs sm c (l,(n,t),r)))

-- write all camera pages to disk
lt_write_camera_pages :: Config -> IO ()
lt_write_camera_pages cf = do
  cs <- lt_camera_data cf
  let sm = camera_submenu cf cs
      f (c,n) = mapM (write_camera_page cf cs sm c) (sets n)
  mapM_ f cs

-- generate single camera page
lt_camera_page :: Config -> (String,String) -> IO String
lt_camera_page cf (c,n) = do
  cs <- lt_camera_data cf
  let sm = camera_submenu cf cs
      h = camera_page cf cs sm c (camera_neighbours cs (c,n))
  return (H.renderHTML5 h)

lt_mk_reduction :: Config -> String -> IO ()
lt_mk_reduction cf nm = do
  let sz = [60,80,400,450,500]::[Int]
      sz' = map show sz
      d = lt_dir cf </> "data/image/photos"
      i_fn = d </> nm <.> "jpg"
      o_fd n = lt_dir cf </> "rgen/photos/r-"++n
      o_fn n = o_fd n </> nm <.> "jpg"
      mk n = do
        e <- doesFileExist (o_fn n)
        when (not e)
             (rawSystem "mkdir" ["-p",o_fd n] >>
              rawSystem "convert" ["-resize","x"++n,i_fn,o_fn n] >>
              return ())
  mapM_ mk sz'

lt_camera_reductions :: Config -> IO ()
lt_camera_reductions cf = do
  cs <- lt_camera_data cf
  let nms = concatMap (map fst . snd) cs
  mapM_ (lt_mk_reduction cf) nms

-- This odd rule creates the structure at Data.lt_md
lt_clear_path :: [FilePath] -> ([FilePath],FilePath)
lt_clear_path p =
    case p of
      [] -> ([],"")
      _ -> (init p,last p)
