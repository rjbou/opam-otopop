all: opam-otopop

ALWAYS:
	@

opam-otopop: ALWAYS
	dune build src/otopop.exe
	@cp _build/default/src/otopop.exe $@

run: opam-otopop
	dune exec src/otopop.exe

clean:
	rm dune-project
	dune clean

distclean:
	rm opam-otopop
