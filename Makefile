src = $(wildcard *.lhs)
notebooks = $(patsubst %.lhs,notebooks/%.ipynb,$(src))

all: $(notebooks)

notebooks/%.ipynb: %.lhs
	IHaskell convert --force -o $@ $^
