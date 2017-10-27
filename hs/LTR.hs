module LTR where

import Data.Char {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import qualified Text.HTML.Minus as H {- html-minus -}
import qualified WWW.Minus.IO as IO {- www-minus -}
import qualified WWW.Minus.MD as MD {- www-minus -}

import qualified Img as I

-- * Genera

-- > title_case "title case" == "Title case"
title_case :: String -> String
title_case s =
    case s of
      [] -> []
      x:xs -> toUpper x : map toLower xs

upper_case :: String -> String
upper_case = map toUpper

-- * Configuration

data Config = Config {lt_base :: I.Renamer -- ^ menu
                     ,lt_root :: FilePath -- ^ css & data & rgen
                     ,lt_use_preload :: Bool}

lt_file :: FilePath -> FilePath
lt_file = (</>) "/home/rohan/ut/www-ltr"

lt_css :: Config -> FilePath
lt_css cf = lt_root cf </> "css/lt.css"

lt_site :: String
lt_site = "http://luciethorne.com"

lt_edit_ln :: Config -> [Char] -> FilePath
lt_edit_ln cf p = lt_root cf </> "?e=" ++ p

lt_data_dirs :: [String]
lt_data_dirs = ["css","data","rgen"]

lt_menu_items :: [String]
lt_menu_items =
    [{-"home",-}
     "news","shows","albums",{-"shop",-}"press","photos","video","contact"]

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
std_meta :: Config -> String -> [H.Content]
std_meta cf dsc =
    [H.title [] [H.cdata ("lucie thorne: " ++ dsc)]
    ,H.meta_description dsc
    ,H.meta_author "lucie thorne"
    ,H.meta_content_type "text/html; charset=UTF-8"
    ,H.link_css "all" (lt_css cf)
    ,H.link_rss "rss" "?p=news/rss.xml"
    ,H.meta_viewport "width=device-width,initial-scale=1,user-scalable=yes"]

std_html :: [H.Content] -> H.Element
std_html = H.html [H.lang "en"]

-- p = path to page
std_copyright :: Config -> FilePath -> H.Content
std_copyright cf p =
    let rss_v = H.w3_rss_validator lt_site
        mk_i :: String -> Int -> H.Content
        mk_i nm sz = H.img [H.src (printf "data/png/icon/%s-%d.gr.png" nm sz)
                           ,H.alt nm]
        rss_i = mk_i "rss" 14
        fb_i = mk_i "fb" 14
        --ms_i = mk_i "ms" 14
        sc_i = mk_i "sc" 14
        ig_i = mk_i "ig" 14
    in H.footer
        [H.class_attr "footer"]
        [H.p []
         [H.a [H.href "?p=news/rss.xml"] [rss_i]
         ,H.a [H.href "http://www.facebook.com/lucie.thorne"] [fb_i]
         ,H.a [H.href "http://soundcloud.com/lucie-1-2"] [sc_i]
         ,H.a [H.href "http://instagram.com/luciennethorne"] [ig_i]
         --,H.a [H.href "http://www.myspace.com/luciethornemusic"] [ms_i]
         --,H.br []
         ,H.copy
         ,H.a [H.href lt_site] [H.cdata "lucie thorne"]
         ,H.cdata " 2017. " {- 1998- -}
         ,H.a [H.href H.w3_html_validator] [H.cdata "html"]
         ,H.cdata ", "
         ,H.a [H.href H.w3_css_validator] [H.cdata "css"]
         ,H.cdata ", "
         ,H.a [H.href rss_v] [H.cdata "rss"]
         ,H.a [H.href (lt_edit_ln cf p)] [H.cdata "."]]]

std_menu :: Config -> String -> H.Content
std_menu cf =
    let f (m,p_) = (m,p_,Just (lt_base cf p_))
    in H.nav_menu_span id "menu" (map f (lt_menu upper_case))

lt_h1 :: H.Content
lt_h1 =
    let t = H.h1 [H.title_attr "lucie thorne"] [H.cdata (upper_case "Lucie Thorne")]
    in H.a [H.class_attr "h1",H.href lt_site] [t]

-- > joinPath ["a","b"] == "a/b"
lt_std_html :: Config -> [String] -> H.Content -> H.Element
lt_std_html cf p t =
    let n_ = lt_class_tag p
        a_ = joinPath p
        m_ = std_meta cf a_
        x_ = H.div [H.class_attr "content"] [lt_h1,t]
        b_ = [std_menu cf n_,x_,std_copyright cf a_]
    in std_html [H.head [] m_
                ,H.body [H.class_attr n_] [H.div [H.class_attr "main"] b_]]

-- * Markdown

read_file_or :: String -> FilePath -> IO String
read_file_or s f = do
  x <- doesFileExist f
  if x then IO.read_file_utf8 f else return s

lt_no_file :: String
lt_no_file =
    unlines ["unfortunately the file you've asked for doesn't exist."
            ,"we might have moved it a little?"
            ,"please try finding it using the menu."]

lt_markdown_to_html :: String -> IO String
lt_markdown_to_html = MD.md_to_html

{-
lt_markdown_to_html_io :: FilePath -> IO String
lt_markdown_to_html_io fn =
    let f = lt_markdown_to_html
    in fmap f (read_file_or lt_no_file fn)
-}

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

arrows :: Config -> I.Neighbours -> H.Content
arrows cf (l,_,r) =
    let f s (Just n) = H.a [H.href (lt_base cf ("photos" </> n))] [s]
        f s Nothing = s
    in H.span [H.class_attr "arrows"] [f H.larr l,H.nbsp,H.nbsp,f H.rarr r]

photos_page :: Config ->
               [I.Img] ->
               H.Content ->
               I.Area ->
               I.Neighbours ->
               H.Element
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
        x_ = H.div [H.class_attr "content"] [g_,i_,I.img_preload 450 (rt,lt_base cf) im c]
        b_ = [std_menu cf "photos",lt_h1,sm,x_,std_copyright cf "photos"]
    in std_html [H.head [] m_
                ,H.body [H.class_attr "photos"] [H.div [H.class_attr "main"] b_]]

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
  let r = case I.img_find i im of
            [] -> lt_photo_page cf (I.img_initial im) im
            x:_ -> lt_photo_page cf x im
  return r

lt_img_reductions :: Config -> IO ()
lt_img_reductions cf = do
  im <- lt_img_data cf
  let nms = concatMap (map fst . snd) im
  mapM_ (I.img_mk_reduction (lt_root cf)) nms

