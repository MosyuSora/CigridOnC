.PHONY: all clean

all:
	ocamlbuild -use-menhir cigrid.native	
	mv cigrid.native cigrid

clean:
	$(RM) -rf _build
	$(RM) -f cigrid
