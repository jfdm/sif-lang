##  Makefile

IDRIS := idris
LIB   := sif
EXE   := sifexe

.PHONY: doc clobber check clean lib install

install:
	${IDRIS} --install ${LIB}.ipkg

lib:
	${IDRIS} --build ${LIB}.ipkg

exe: install
	${IDRIS} --build ${EXE}.ipkg

clean:
	${IDRIS} --clean ${LIB}.ipkg
	find . -name "*~" -delete
	${IDRIS} --clean ${EXE}.ipkg

check: clobber
	${IDRIS} --checkpkg ${LIB}.ipkg

clobber : clean
	find . -name "*.ibc" -delete

# test: install
# 	$(MAKE) -C test build
# 	(cd test; ./a.out)
# 	$(MAKE) -C test clean

doc:
	${IDRIS} --mkdoc ${LIB}.ipkg
