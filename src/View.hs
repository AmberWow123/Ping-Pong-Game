drawUI :: PlayState -> [Widget n]
drawUI g = [C.center $ padRight (Pad 2) (drawStats g) <+> drawBoard g]


		
	
		
	
		
