doctest:
	doctest $(shell find lib | grep .hs$)

hspec:
	cabal test --show-details=always --test-option=--color

test: doctest hspec

watch:
	find . | grep .hs$ | entr make test
