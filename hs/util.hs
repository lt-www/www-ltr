import LTR

type LT_VID = (String,String,String,String)

lt_videos :: [LT_VID]
lt_videos =
    [("Fx5ETWEJZQc","Causeway","with Hamish Stuart","ABC Radio")
    ,("mSVKt4RqET0","Butterfly Blues","with Pieta Brown","ABC Radio National")
    ,("2DJJfnrM7hw","Can't Sleep for Dreaming","","SBS TV 'Rockwiz'")
    ,("1EPzVPGBlBc","As You Find It","","The Melbourne Folk Club")
    ,("6wR7JYtz2r8","Till The Season","with Hamish Stuart","The Tanja Hall, ABC Southeast")
    ,("QKN4qOatpKo","When I Get There","with Hamish Stuart","The Guildford Hall")
    ,("A_r7HWTzuT4","Falling","with Hamish Stuart","'Work in progress', ABC Southeast, Bimbaya")
    ,("xbO8nk_jeSY","Open Sky","","The Basement, Sydney")]

gen_video :: LT_VID -> [String]
gen_video (yt,nm,wt,wh) =
    let c = concat
    in ["<div class=\"yt-entry\">"
       ,"<div class=\"yt-img\">"
       ,c ["<a href=\"?v=",yt,"\">"]
       ,c ["<img src=\"http://i.ytimg.com/vi/",yt,"/default.jpg\" width=\"120\" height=\"90\" />"]
       ,"</a>"
       ,"</div>"
       ,"<div class=\"yt-txt\">"
       ,c ["<a href=\"?v=",yt,"\">",nm,"</a> ",wt,"<br />"]
       ,wh
       ,"</div>"
       ,"</div>"]

videos_html :: [String]
videos_html = concatMap gen_video lt_videos ++ ["","<div class=\"clear\"></div>",""]

videos_io :: IO ()
videos_io = writeFile (lt_file "data/md/video.md") (unlines videos_html)
