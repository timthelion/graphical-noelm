{-GENERATED BY GRAPHICALELM0.0-}
import Window{-MISC_ENTRY-}{-*****-}
main=(\acf dcf->flow down [acf,dcf])<~{-_language_ElmLang_base_code_#####$#(\acf dcf->flow down [acf,dcf])<~#$#####_parents_-}analogClockFace~digitalClockFace{-*****-}
analogClockFace=(\sh mh hh cd->collage cd cd [sh,mh,hh])<~{-_language_ElmLang_base_code_#####$#(\sh mh hh cd->collage cd cd [sh,mh,hh])<~#$#####_parents_-}secondsHand~minutesHand~hoursHand~clockDiameter{-*****-}
secondsHand=(\p c-> traced (solid c) p)<~ {-_language_ElmLang_base_code_#####$#(\p c-> traced (solid c) p)<~ #$#####_parents_-}secondsHandPath~secondsHandColor{-*****-}
minutesHand=(\p c-> traced (solid c) p)<~ {-_language_ElmLang_base_code_#####$#(\p c-> traced (solid c) p)<~ #$#####_parents_-}minutesHandPath~minutesHandColor{-*****-}
hoursHand=(\p c-> traced (solid c) p)<~ {-_language_ElmLang_base_code_#####$#(\p c-> traced (solid c) p)<~ #$#####_parents_-}hoursHandPath~hoursHandColor{-*****-}
seconds=(\t->rem (round <| inSeconds t) 60) <~{-_language_ElmLang_base_code_#####$#(\t->rem (round <| inSeconds t) 60) <~#$#####_parents_-}time{-*****-}
minutes=(\t->rem (round <| inMinutes t) 60) <~{-_language_ElmLang_base_code_#####$#(\t->rem (round <| inMinutes t) 60) <~#$#####_parents_-}time{-*****-}
hours=(\t->rem (round <| inHours t) 12) <~{-_language_ElmLang_base_code_#####$#(\t->rem (round <| inHours t) 12) <~#$#####_parents_-}time{-*****-}
secondsAngle=(\sp->degrees <| 90-sp*360)<~{-_language_ElmLang_base_code_#####$#(\sp->degrees <| 90-sp*360)<~#$#####_parents_-}secondsProcent{-*****-}
minutesAngle=(\t->degrees <| 90-t*360)<~{-_language_ElmLang_base_code_#####$#(\t->degrees <| 90-t*360)<~#$#####_parents_-}minutesProcent{-*****-}
hoursAngle=(\t->degrees <| 90-t*360)<~{-_language_ElmLang_base_code_#####$#(\t->degrees <| 90-t*360)<~#$#####_parents_-}hoursProcent{-*****-}
time=every second{-_language_ElmLang_base_code_#####$#every second#$#####_parents_-}{-*****-}
secondsProcent=(\seconds->toFloat seconds/60)<~{-_language_ElmLang_base_code_#####$#(\seconds->toFloat seconds/60)<~#$#####_parents_-}seconds{-*****-}
minutesProcent=(\minutes->toFloat minutes/60)<~{-_language_ElmLang_base_code_#####$#(\minutes->toFloat minutes/60)<~#$#####_parents_-}minutes{-*****-}
hoursProcent=(\hours->toFloat hours/12)<~{-_language_ElmLang_base_code_#####$#(\hours->toFloat hours/12)<~#$#####_parents_-}hours{-*****-}
secondsHandPath=(\x y->[(0,0),(x,y)])<~{-_language_ElmLang_base_code_#####$#(\x y->[(0,0),(x,y)])<~#$#####_parents_-}secondsHandX~secondsHandY{-*****-}
minutesHandPath=(\x y->[(0,0),(x,y)])<~{-_language_ElmLang_base_code_#####$#(\x y->[(0,0),(x,y)])<~#$#####_parents_-}minutesHandX~minutesHandY{-*****-}
hoursHandPath=(\x y->[(0,0),(x,y)])<~{-_language_ElmLang_base_code_#####$#(\x y->[(0,0),(x,y)])<~#$#####_parents_-}hoursHandX~hoursHandY{-*****-}
secondsHandX=(\a l->cos a * l)<~{-_language_ElmLang_base_code_#####$#(\a l->cos a * l)<~#$#####_parents_-}secondsAngle~secondsHandLength{-*****-}
secondsHandY=(\a l->sin a *l)<~{-_language_ElmLang_base_code_#####$#(\a l->sin a *l)<~#$#####_parents_-}secondsAngle~secondsHandLength{-*****-}
minutesHandX=(\a l->cos a * l)<~{-_language_ElmLang_base_code_#####$#(\a l->cos a * l)<~#$#####_parents_-}minutesAngle~minutesHandLength{-*****-}
minutesHandY=(\a l->sin a *l)<~{-_language_ElmLang_base_code_#####$#(\a l->sin a *l)<~#$#####_parents_-}minutesAngle~minutesHandLength{-*****-}
hoursHandX=(\a l->cos a*l)<~{-_language_ElmLang_base_code_#####$#(\a l->cos a*l)<~#$#####_parents_-}hoursAngle~hoursHandLength{-*****-}
hoursHandY=(\a l->sin a * l)<~{-_language_ElmLang_base_code_#####$#(\a l->sin a * l)<~#$#####_parents_-}hoursAngle~hoursHandLength{-*****-}
secondsHandLength=(\r->0.8*r)<~{-_language_ElmLang_base_code_#####$#(\r->0.8*r)<~#$#####_parents_-}clockRadius{-*****-}
minutesHandLength=(\r->0.7*r)<~{-_language_ElmLang_base_code_#####$#(\r->0.7*r)<~#$#####_parents_-}clockRadius{-*****-}
hoursHandLength=(\clockRadius->((*)(0.6)(clockRadius)))<~{-_language_Ikcilpazc_base_code_#####$#0.6 clockRadius .. *#$#####_parents_-}clockRadius{-*****-}
secondsHandColor=constant red{-_language_ElmLang_base_code_#####$#constant red#$#####_parents_-}{-*****-}
minutesHandColor=constant blue{-_language_ElmLang_base_code_#####$#constant blue#$#####_parents_-}{-*****-}
hoursHandColor=constant green{-_language_ElmLang_base_code_#####$#constant green#$#####_parents_-}{-*****-}
clockRadius=(\w h dch->((toFloat <| min w h)/2-(toFloat dch/2)))<~{-_language_ElmLang_base_code_#####$#(\w h dch->((toFloat <| min w h)/2-(toFloat dch/2)))<~#$#####_parents_-}windowWidth~windowHeight~digitalClockHeight{-*****-}
digitalClockFace=(\s m h->flow right [asText h,plainText ":",asText m,plainText ":",asText  s,plainText "UTC"])<~{-_language_ElmLang_base_code_#####$#(\s m h->flow right [asText h,plainText ":",asText m,plainText ":",asText  s,plainText "UTC"])<~#$#####_parents_-}seconds~minutes~hours{-*****-}
windowWidth=Window.width{-_language_ElmLang_base_code_#####$#Window.width#$#####_parents_-}{-*****-}
windowHeight=Window.height{-_language_ElmLang_base_code_#####$#Window.height#$#####_parents_-}{-*****-}
clockDiameter=(\r->round (r*2))<~{-_language_ElmLang_base_code_#####$#(\r->round (r*2))<~#$#####_parents_-}clockRadius{-*****-}
digitalClockHeight=(\dcf->heightOf dcf) <~{-_language_ElmLang_base_code_#####$#(\dcf->heightOf dcf) <~#$#####_parents_-}digitalClockFace{-*****-}