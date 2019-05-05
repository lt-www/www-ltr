import System.FilePath {- filepath -}

import qualified Text.HTML.Minus as H {- html-minus -}
import qualified WWW.Minus.CGI as CGI {- www-minus -}

import qualified LT

md_page :: LT.Config -> [FilePath] -> IO ()
md_page cf p = do
  let p' = LT.lt_markdown_file_name p
  h <- LT.lt_markdown_to_html_io p'
  let h' = LT.lt_std_html cf p (H.cdata_raw h)
  CGI.utf8_html_output (H.renderHTML5_pp h')

dispatch :: LT.Config -> CGI.Parameters -> IO ()
dispatch cf (m,q) =
    case (m,q) of
      ("GET",[]) -> md_page cf []
      ("GET",[("p",d)]) -> md_page cf (splitDirectories d)
      _ -> CGI.utf8_text_output "unknown request?"

main :: IO ()
main = CGI.cgi_main (dispatch (LT.Config ("?p=" ++) "." True))
