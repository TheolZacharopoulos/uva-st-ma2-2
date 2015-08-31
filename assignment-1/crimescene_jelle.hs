data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]


says :: Boy -> Boy -> Bool

says Matthew Carl -> False
says Matthew Matthew -> False

says Peter Matthew -> True
says Peter Jack -> True

says Jack Carl -> True
says Jack Matthew -> False
says Jack Jack -> False





says Jack Matthew -> False
says Jack Peter -> False

says Arnold Matthew -> if honest Peter then False else True
says Arnold Peter -> if honest Matthew then False else True

says Carl Arnold -> not $ says Arnold _



accusers :: Boy -> [Boy]

accusers Matthew Carl = False
accusers Matthew Matthew = False

accusers Peter Matthew = True
accusers Peter Jack = True




