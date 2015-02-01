all:
	hlint Main.hs
	ghc -Wall Main.hs
