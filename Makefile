
install:
		stack build

configure:
		stack setup

run:
		stack ghci
#		stack exec hCM

clean:
		stack clean

doc:
		haddock --html -o doc src/CM/*
