module LT where

import Data.Char {- base -}
import System.FilePath {- filepath -}

import qualified Text.Html.Minus as Html {- html-minus -}
import qualified Www.Minus.Md as Md {- xml -}

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
    ["news","tour","music","art","about","contact"]

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
std_meta :: Config -> String -> [Html.Content]
std_meta cf dsc =
    [Html.title [] [Html.cdata ("LUCIE THORNE: " ++ display_text dsc)]
    ,Html.meta_description dsc
    ,Html.meta_author "lucie thorne"
    ,Html.meta_content_type "text/html; charset=UTF-8"
    ,Html.link_css "all" (lt_css cf)
    ,Html.link_ty Html.Link_Icon [Html.type_attr "image/png",Html.href "https://luciethorne.com/data/image/ground/lt.512.png"]
    ,Html.meta_viewport "width=device-width,initial-scale=1,user-scalable=yes"]

std_html :: [Html.Content] -> Html.Element
std_html = Html.html [Html.lang "en"]

std_menu :: Config -> String -> Html.Content
std_menu cf =
    let f (m,p_) = (m,p_,Just (lt_base cf p_))
    in Html.nav_menu_list id "menu" (map f (lt_menu display_text))

lt_h1 :: Html.Content
lt_h1 =
    let t = Html.h1
            [Html.title_attr "lucie thorne"]
            [Html.img [Html.alt "LUCIE THORNE"
                   ,Html.src "data/image/ground/lucie-thorne.png"]]
    in div_c "lucie-thorne" [Html.a [Html.class_attr "h1",Html.href lt_site] [t]]

std_copyright :: Config -> FilePath -> Html.Content
std_copyright _ _ =
  Html.footer
        [Html.class_attr "footer"]
        [Html.a [Html.href "https://luciethorne.bandcamp.com"] [Html.cdata "BC"]
        ,Html.a [Html.href "http://instagram.com/luciennethorne"] [Html.cdata "IG"]
        ,Html.a [Html.href "http://www.facebook.com/lucie.thorne"] [Html.cdata "FB"]
        ,Html.a [Html.href "http://soundcloud.com/lucie-1-2"] [Html.cdata "SC"]
        ,Html.copy
        ,Html.a [Html.href lt_site] [Html.cdata "lucie thorne"]
        ,Html.cdata ", 2021. "]

div_c :: String -> [Html.Content] -> Html.Content
div_c c = Html.div [Html.class_attr c]

div_cid :: String -> String -> [Html.Content] -> Html.Content
div_cid c i = Html.div [Html.class_attr c,Html.id i]

null_replace :: [a] -> [a] -> [a]
null_replace z e = if null e then z else e

-- > joinPath ["a","b"] == "a/b"
lt_std_html :: Config -> [String] -> Html.Content -> Html.Element
lt_std_html cf p t =
    let n_ = lt_class_tag p
        a_ = null_replace "home" (joinPath p)
        m_ = std_meta cf a_
        x_ = div_c "content" [t]
        bg = [div_cid "bg" "noise" [Html.nbsp]
             ,div_cid "bg" "image" [Html.nbsp]]
        b_ = [lt_h1,std_menu cf n_,x_,std_copyright cf a_]
    in std_html [Html.head [] m_
                ,Html.body [Html.class_attr n_] (bg ++ [div_c "main" b_])]

-- * Markdown

lt_no_file :: String
lt_no_file =
    unlines ["unfortunately the file you've asked for doesn't exist."
            ,"we might have moved it a little?"
            ,"please try finding it using the menu."]

lt_markdown_to_html_io :: FilePath -> IO String
lt_markdown_to_html_io = Md.md_load_html "bin"

-- | Special case for the 'home' file.
lt_markdown_file_name_f :: FilePath -> FilePath
lt_markdown_file_name_f p =
    case p of
      "" -> "data/md/home.md"
      _ -> "data/md" </> p <.> "md"

lt_markdown_file_name :: [FilePath] -> FilePath
lt_markdown_file_name = lt_markdown_file_name_f . joinPath
