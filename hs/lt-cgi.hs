import System.FilePath {- filepath -}

import qualified Text.Html.Minus as Html {- html-minus -}
import qualified Www.Minus.Cgi as Cgi {- www-minus -}

import qualified LT

md_page :: LT.Config -> [FilePath] -> IO ()
md_page cf p = do
  let p' = LT.lt_markdown_file_name p
  h <- LT.lt_markdown_to_html_io p'
  let h' = LT.lt_std_html cf p (Html.cdata_raw h)
  Cgi.utf8_html_output (Html.renderHtml5_pp h')

dispatch :: LT.Config -> Cgi.Parameters -> IO ()
dispatch cf (m,q) =
    case (m,q) of
      ("GET",[]) -> md_page cf []
      ("GET",("p",d):_) -> md_page cf (splitDirectories d)
      _ -> Cgi.utf8_text_output "unknown request?"

main :: IO ()
main = Cgi.cgi_main (dispatch (LT.Config ("?p=" ++) "." True))
