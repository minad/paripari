example.hs : README.md Makefile
	unlit --language haskell -f markdown -i README.md -o example.hs
