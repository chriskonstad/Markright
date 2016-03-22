EXEC=markright.native
SOURCES=src/markright.ml


$(EXEC): $(SOURCES)
	corebuild -I src -use-ocamlfind -pkgs Batteries -pkgs Yojson markright.native
