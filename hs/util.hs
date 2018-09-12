import LTR

type LT_VID = (String,String,String,String)

lt_videos :: [LT_VID]
lt_videos =
    [("DxTKUIL_tpI","The Rushing Dark","","Video by Heike Qualitz")
    ,("XUfGjLWvW9g","Blackwing / Moving Colours / Green & Blue","","Live in Session - 2SER radio Sydney")
    ,("2DJJfnrM7hw","Can't Sleep for Dreaming","","SBS TV 'Rockwiz'")
    ,("1EPzVPGBlBc","As You Find It","","The Melbourne Folk Club")
    ,("xbO8nk_jeSY","Open Sky","","The Basement, Sydney")]

lt_videos_ll :: [LT_VID]
lt_videos_ll =
    [("A_r7HWTzuT4","Falling","with Hamish Stuart","'Work in progress', ABC Southeast, Bimbaya")
    ,("6wR7JYtz2r8","Till The Season","with Hamish Stuart","The Tanja Hall, ABC Southeast")
    ,("l4GRfPzoEXM","Green & Blue","with Hamish Stuart","The Music Show - ABC RN")
    ,("RVNV-tyCFtU","Blackwing","with Hamish Stuart","The Music Show - ABC RN")
    ,("Fx5ETWEJZQc","Causeway","with Hamish Stuart","ABC Radio")
    ,("mSVKt4RqET0","Butterfly Blues","with Pieta Brown","ABC Radio National")
    ,("QKN4qOatpKo","When I Get There","with Hamish Stuart","The Guildford Hall")
    ]

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
