import System.FilePath {- filepath -}

import qualified Text.HTML.Minus as H {- html-minus -}

import qualified WWW.Minus.CGI as CGI {- www-minus -}
import qualified WWW.Minus.IO as IO {- www-minus -}

import qualified LT2.Img as I
import qualified LT2.LTR as L

md_page :: L.Config -> [FilePath] -> IO ()
md_page cf p = do
  let fn = L.lt_markdown_file_name p
  md <- IO.read_file_utf8 fn
  h <- L.lt_markdown_to_html md
  let h' = L.lt_std_html cf p (H.cdata_raw h)
  CGI.utf8_html_output (H.renderHTML5_pp h')

ph_page_def :: L.Config -> IO ()
ph_page_def cf = do
  cs <- L.lt_img_data cf
  let i = I.img_initial cs
  h <- L.lt_photo_page_io cf i
  CGI.utf8_html_output h

ph_page_i :: L.Config -> I.Id -> IO ()
ph_page_i cf i = do
  h <- L.lt_photo_page_io_id cf i
  CGI.utf8_html_output h

ph_page :: L.Config -> [I.Id] -> IO ()
ph_page cf x =
    case x of
      i:_ -> ph_page_i cf i
      _ -> ph_page_def cf

d_page :: L.Config -> [String] -> IO ()
d_page cf d =
    case d of
      "photos":i -> ph_page cf i
      _ -> md_page cf d

mk_viewer :: String -> H.Content
mk_viewer v =
    let v' = "https://www.youtube.com/embed/" ++ v
        o_a = [H.width "425", H.height "344"]
        o_c = [H.param [H.name "movie"
                       ,H.value v']
              ,H.param [H.name "allowFullScreen"
                       ,H.value "true"]
              ,H.param [H.name "allowscriptaccess"
                       ,H.value "always"]
              ,H.iframe [H.src v'
                        ,H.frameborder "0"
                        ,H.mk_attr "allowfullscreen" "true"
                        ,H.width "560"
                        ,H.height "315"] []]
        o = H.object o_a o_c
    in H.div [H.class_attr "viewer"] [o]

v_page :: L.Config -> String -> IO ()
v_page cf d = do
  let h' = L.lt_std_html cf ["?v="++d] (mk_viewer d)
  CGI.utf8_html_output (H.renderHTML5_pp h')

resize_get :: L.Config -> IO ()
resize_get cf = do
  L.lt_img_reductions cf
  return ()

-- > splitDirectories "photos/fence" == ["photos","fence"]
dispatch :: L.Config -> CGI.Parameters -> IO ()
dispatch cf (m,q) =
      case (m,q) of
         ("GET",[("p",d)]) -> d_page cf (splitDirectories d)
         ("GET",[("o","resize")]) -> resize_get cf >> v_page cf []
         ("GET",[("v",d)]) -> v_page cf d
         ("GET",[]) -> d_page cf []
         _ -> CGI.utf8_text_output "ltr: unknown_request"

main :: IO ()
main = CGI.cgi_main (dispatch (L.Config ("?p=" ++) "." True))
