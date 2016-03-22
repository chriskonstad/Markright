EXEC=markright.native
SOURCES=markright.ml


$(EXEC): $(SOURCES)
	corebuild -use-ocamlfind -pkgs Batteries -pkgs Yojson markright.native
