module Main where
	data Note = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs 
		deriving (Eq, Show)
	
	--data Interval = Unison | Min2 | Maj2 | Min3 | Maj3 deriving (Show)
	--data Scale = [Note]
	--data Chord = [Note]

	--data ScaleType = Major | Minor deriving (Show)
	--data ChordType = Major | Minor deriving (Show)

	scale1 = [A, As, B, C, Cs, D, Ds, E, F, Fs, G, Gs]

	cscale = n1 where
		n1 	= A:n2
		n2 	= As:n3	
		n3 	= B:n4
		n4 	= C:n5
		n5 	= Cs:n6
		n6 	= D:n7
		n7 	= Ds:n8
		n8 	= E:n9
		n9 	= F:n10
		n10 = Fs:n11
		n11 = G:n12a  
		n12 = Gs:n12

	--learn to write in a standardized way: integrate using guards and pattern matching

	lastnote :: [Note] -> Note
	lastnote (n:ns)
		| (n:ns)==[] = n
		| ns==[] = n 
		| otherwise = lastnote ns

	firstnote :: [Note] -> Note
	firstnote (n:ns)
		| (n:ns)==[] = error "Nil list entered"
		| otherwise = head ns

	--scale: a starting note -> a note list
	scale :: Note -> [Note]
	scale note = scale1
	--scale  = take 7 scale1

	-- starting note and base scale
	getscale :: Note -> [Note] -> [Note]
	getscale _ [] = [] 
	getscale n (h:t) | n==h = h:t
	getscale n (h:t) = getscale n t
	

	--getScale n (h:t) = getScale n t