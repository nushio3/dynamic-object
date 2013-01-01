init:
	cabal install --enable-tests --only-dependencies
	cabal configure --enable-tests
doc:
	cabal haddock --hyperlink-source

