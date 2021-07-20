module LT where

import Data.Char {- base -}
import System.FilePath {- filepath -}

import qualified Text.HTML.Minus as H {- html-minus -}
import qualified WWW.Minus.MD as MD {- xml -}

-- * Genera

-- > title_case "title case" == "Title case"
title_case :: String -> String
title_case s =
    case s of
      [] -> []
      x:xs -> toUpper x : map toLower xs

upper_case :: String -> String
upper_case = map toUpper

no_hyphens :: String -> String
no_hyphens =
    let f z = if z == '-' then ' ' else z
    in map f

display_text :: String -> String
display_text = no_hyphens . upper_case

-- * Configuration

data Config = Config {lt_base :: FilePath -> FilePath -- ^ menu
                     ,lt_root :: FilePath -- ^ css & data & rgen
                     ,lt_use_preload :: Bool}

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
    ["news","tour","music","about","contact"]

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
    [H.title [] [H.cdata ("LUCIE THORNE: " ++ display_text dsc)]
    ,H.meta_description dsc
    ,H.meta_author "lucie thorne"
    ,H.meta_content_type "text/html; charset=UTF-8"
    ,H.link_css "all" (lt_css cf)
    ,H.link_ty H.Link_Icon [H.type_attr "image/png",H.href "https://luciethorne.com/data/image/ground/lt.512.png"]
    ,H.meta_viewport "width=device-width,initial-scale=1,user-scalable=yes"]

std_html :: [H.Content] -> H.Element
std_html = H.html [H.lang "en"]

std_menu :: Config -> String -> H.Content
std_menu cf =
    let f (m,p_) = (m,p_,Just (lt_base cf p_))
    in H.nav_menu_list id "menu" (map f (lt_menu display_text))

lt_h1 :: H.Content
lt_h1 =
    let t = H.h1
            [H.title_attr "lucie thorne"]
            [H.img [H.alt "LUCIE THORNE"
                   ,H.src "data/image/ground/lucie-thorne.png"]]
    in div_c "lucie-thorne" [H.a [H.class_attr "h1",H.href lt_site] [t]]

std_copyright :: Config -> FilePath -> H.Content
std_copyright _ _ =
  H.footer
        [H.class_attr "footer"]
        [H.a [H.href "https://luciethorne.bandcamp.com"] [H.cdata "BC"]
        ,H.a [H.href "http://instagram.com/luciennethorne"] [H.cdata "IG"]
        ,H.a [H.href "http://www.facebook.com/lucie.thorne"] [H.cdata "FB"]
        ,H.a [H.href "http://soundcloud.com/lucie-1-2"] [H.cdata "SC"]
        ,H.copy
        ,H.a [H.href lt_site] [H.cdata "lucie thorne"]
        ,H.cdata ", 2021. "]

div_c :: String -> [H.Content] -> H.Content
div_c c = H.div [H.class_attr c]

div_cid :: String -> String -> [H.Content] -> H.Content
div_cid c i = H.div [H.class_attr c,H.id i]

null_replace :: [a] -> [a] -> [a]
null_replace z e = if null e then z else e

-- > joinPath ["a","b"] == "a/b"
lt_std_html :: Config -> [String] -> H.Content -> H.Element
lt_std_html cf p t =
    let n_ = lt_class_tag p
        a_ = null_replace "home" (joinPath p)
        m_ = std_meta cf a_
        x_ = div_c "content" [t]
        bg = [div_cid "bg" "noise" [H.nbsp]
             ,div_cid "bg" "image" [H.nbsp]]
        b_ = [lt_h1,std_menu cf n_,x_,std_copyright cf a_]
    in std_html [H.head [] m_
                ,H.body [H.class_attr n_] (bg ++ [div_c "main" b_])]

-- * Markdown

lt_no_file :: String
lt_no_file =
    unlines ["unfortunately the file you've asked for doesn't exist."
            ,"we might have moved it a little?"
            ,"please try finding it using the menu."]

lt_markdown_to_html_io :: FilePath -> IO String
lt_markdown_to_html_io = MD.md_load_html "bin"

-- | Special case for the 'home' file.
lt_markdown_file_name_f :: FilePath -> FilePath
lt_markdown_file_name_f p =
    case p of
      "" -> "data/md/home.md"
      _ -> "data/md" </> p <.> "md"

lt_markdown_file_name :: [FilePath] -> FilePath
lt_markdown_file_name = lt_markdown_file_name_f . joinPath
