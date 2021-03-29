module project

import StdEnv, StdIO, StdFunc, StdDebug, osfont

::PiecePicture = {
			 tileWidth::Int  , 
			 tileHeight:: Int , 
			 arrayOfPixels :: [RGBColour] 
		   }

		   
readPicture :: *File -> (PiecePicture,*File)
readPicture file
	# (b1, x, file) = freadi file
	# (b2, y, file) = freadi file
	# (pixels, file) = getPixels file 
	| b1 && b2 = ({tileWidth = x,tileHeight = y,arrayOfPixels = pixels}, file)
	| otherwise = abort "Reading Error 1 !"

getPixels :: *File -> ([RGBColour], *File)
getPixels file
 	#(isEnd, file) = fend file
  	|isEnd = ([], file)
	# (b1,r1 ,file) = freadi file
	# (b2,r2 ,file) = freadi file
	# (b3,r3 ,file) = freadi file
	| not(b1 && b2 && b3) = ([], file) 
  	#rec = {r = r1, g = r2, b=r3}
  	#(res,file) = getPixels file
 	=([rec:res],file)
	

LoadPicture :: String *World ->  (PiecePicture,*World)
LoadPicture fname w
	# (ok, file, w) = fopen fname FReadText w
	| not ok = abort "Can't open"
	//# (content, file) = readPicture file
	//# (ok, w) = fclose file w
	| not ok = abort "Can't close"
	= (content, w)

TILE_SIZE :== 64

instance == RGBColour
where
	(==) x y = x.r == y.r && x.b == y.b && x.g == y.g
	(==) _ _ = False
	
Start:: *World -> *World
Start world 
	# (arrayOfRGB , world) = LoadPicture "in.txt.txt" world //<--- here you change the file name!
	= startIO SDI Void (initProcessState arrayOfRGB) [ProcessClose closeProcess] world
	where 
    	initProcessState arrayOfRGB pst
    	# (errorM, pst) = openWindow undef (window arrayOfRGB) pst
    	= pst
		
		window arrayOfRGB = Window "Title" NilLS 
					[ WindowClose quit, 
					  WindowViewSize {w = 8*TILE_SIZE, h = 8*TILE_SIZE},
					  WindowLook True (paintFun arrayOfRGB) 
					]

		paintFun :: PiecePicture SelectState UpdateState *Picture -> *Picture  
		paintFun arrayOfRGB _ _ pic 
		# wedth = 1
		# height = 1
		# rgbColour = {r =130, g=63, b=59}
		# pic = setPenColour (RGB rgbColour) pic 
		= (paintFuncAux (arrayOfRGB.arrayOfPixels) wedth height ) pic
		where  
			tile2 = {box_w = 64*64 , box_h = 64*64} // <--- change those sizes if you want to change the size of the image in the window
			paintFuncAux :: [RGBColour] Int Int *Picture -> *Picture
			paintFuncAux [] _ _ picc = picc
			paintFuncAux [x:xs] w h picc
			# rgbColour = {r =130, g=63, b=59}
			# picc = setPenColour (RGB x) picc
			| x.r == 255 && x.g == 0 && x.b == 0 = paintFuncAux [rgbColour:xs] w h picc
			| w == 32 = paintFuncAux xs 1 (h+1) picc 
			= paintFuncAux xs (w + 1) h (fillAt { x = h , y= w} tile2 picc ) 
	
		
		quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)
		quit (local, pst) = (local, closeProcess pst)


