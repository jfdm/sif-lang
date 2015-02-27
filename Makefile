##  Makefile

IDRIS := idris
LIB   := sif
OPTS  :=

.PHONY: doc clobber check clean lib install

install:
	${IDRIS} ${OPTS} --install ${LIB}.ipkg

lib:
	${IDRIS} ${OPTS} --build ${LIB}.ipkg

clean:
	${IDRIS} --clean ${LIB}.ipkg
	find . -name "*~" -delete

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
