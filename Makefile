##  Makefile

IDRIS   := idris
LANGLIB := sif
IOLIB   := sifio
EXE     := sifexe

.PHONY: doc clobber check clean io lang lib linecount testLang testIO tests

lang:
	${IDRIS} --build ${LANGLIB}.ipkg
	${IDRIS} --install ${LANGLIB}.ipkg

io:
	${IDRIS} --build ${IOLIB}.ipkg
	${IDRIS} --install ${IOLIB}.ipkg

lib: lang io


exe: lib
	${IDRIS} --build ${EXE}.ipkg


clean:
	${IDRIS} --clean ${LANGLIB}.ipkg
	${IDRIS} --clean ${IOLIB}.ipkg
	${IDRIS} --clean ${EXE}.ipkg
	find . -name "*~" -delete

check: clobber
	${IDRIS} --checkpkg ${LANGLIB}.ipkg
	${IDRIS} --checkpkg ${IOLIB}.ipkg
	${IDRIS} --checkpkg ${EXE}.ipkg

clobber : clean
	find . -name "*.ibc" -delete

doc:
	${IDRIS} --mkdoc ${LANGLIB}.ipkg
	${IDRIS} --mkdoc ${IOLIB}.ipkg
	${IDRIS} --mkdoc ${EXE}.ipkg

# testLang: lang

# ${IDRIS} --testpkg ${LANGLIB}.ipkg

testIO: lang
	${IDRIS} --testpkg ${IOLIB}.ipkg

tests: testIO


linecount:
	find . -name "*.idr" | grep -v Lib |xargs wc -l -
