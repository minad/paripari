example.hs : README.md Makefile
	unlit --language haskell -f markdown -i README.md -o example.hs
	sed -i '/OPTIONS_GHC/d' example.hs
	sed -i 's/{-# SPECIALISE_ALL/-- {-# SPECIALISE_ALL/g' example.hs
