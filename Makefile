
lhs = $(wildcard *.lhs)

notebooks = $(patsubst %.lhs,notebooks/%.ipynb, $(lhs))

all: $(notebooks)

notebooks/%.ipynb: %.lhs
	IHaskell convert --force -o $@ $^

.PHONEY: cabal
cabal:
	cabal install --prefix=`pwd`
