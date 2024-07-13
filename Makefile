.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f game.zip
	zip -r game.zip .

clean:
	dune clean
	rm -f game.zip

play:
	dune exec ./main.exe

doc:
	dune build @doc

