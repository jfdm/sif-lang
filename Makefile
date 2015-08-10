##  Makefile

IDRIS := idris
LIB   := sif
VIEWER_EXE := sifViewer
CHECKER_EXE := sifChecker

.PHONY: doc clobber check clean lib install

install:
	${IDRIS} --install ${LIB}.ipkg

lib:
	${IDRIS} --build ${LIB}.ipkg

exe: install
	${IDRIS} --build ${VIEWER_EXE}.ipkg
	${IDRIS} --build ${CHECKER_EXE}.ipkg


clean:
	${IDRIS} --clean ${LIB}.ipkg
	find . -name "*~" -delete
	${IDRIS} --clean ${VIEWER_EXE}.ipkg
	${IDRIS} --clean ${CHECKER_EXE}.ipkg

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
